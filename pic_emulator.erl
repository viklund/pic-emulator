-module(pic_emulator).
%-compile(export_all). %% REPLACE

-export([pic200_new/0, pic200_print/1, pic200_load_program/2,
        pic200_run_program/1, pic200_cycle/1]).

%% Record of a pic200.
% accumulator 8 bits
% register, 32 bytes. Divided into SFR (Special Function Registers) and GPR
%                     0-7 SFR and 16 GPR
%             0 -> INDF, Uses content of FSR to address data memory
%             1 -> TMR0, 8-bit real-time clock/counter
%             2 -> PCL, Low-order 8 bits of PC (Program counter)
%             3 -> STATUS, Status of ALU
%             4 -> FSR, Indirect data memory access pointer
%             5 -> OSCCAL
%             6 -> GPIO    last 4 bits only.
%             7 -> CMCON0 (pic204 only).
%         16-31 -> GPR

-record(pic200,{ 
        accumulator = <<0>>,       %        8 bits
        register,                  %       32 bytes
        callstack = [<<0>>,<<0>>], %        2 bytes
        program,                   % 256 x 12 bits

        option,                    %        8 bits
        trisgpio,                  %        8 bits (but only 4bits used)

        instruction_register = <<0:12>>
    }).

-record(opcode,{
        opcode    = nop,
        address   = 0,
        whence    = accumulator,
        constant  = 0,
        bit       = 0
    }).

register_new()             -> register_new(32,8).
register_new(Length,Width) -> lists:duplicate(Length,<<0:Width>>).

register_get(N, L) -> lists:nth(N+1,L).
register_set(N, V, L) ->
    {H,[_|T]} = lists:split(N,L),
    H ++ [V] ++ T.

pic200_new() ->
    #pic200{
        register = register_new(),
        program  = register_new(10,12)
    }.

pic200_load_program(P, File) ->
    {ok, Bin} = file:read_file(File),
    Program = read_program(Bin,[]),
    P#pic200{ program = Program ++ lists:duplicate(10,<<0:12>>) }.

read_program(<<>>, Acc) -> lists:reverse(Acc);
read_program(<<H:12,Rest/bitstring>>, Acc) ->
    read_program(Rest, [<<H:12>>|Acc]);
read_program(<<_/bitstring>>, Acc) -> lists:reverse(Acc).

%% Description of one cycle (4 clocks)
%     Fetch cycle       Execution cycle
% Q1. inc_pc            fetch_instr -> ir2
% Q2.   -               read_memory (from reg)
% Q3.   -               execute_instr
% Q4. fetch_pc -> ir1   write_result  (to reg)

% maybe simulate a clock with an actor to drive this instead
% But then the pic200 record needs to be expanded
pic200_cycle(P) ->
    % Q1 Fetch
    P0    = increment_program_counter(P),
    % Q1 Execute
    Instr = P0#pic200.instruction_register,

    % Q2 Execute
    #opcode{opcode = Opcode, address = Addr, whence = Whence, constant = K, bit = B} 
        = unpack_opcode(Instr),
    <<FVal:8>> = register_get( Addr, P0#pic200.register ),

    % Q3 Execute
    Func = orddict:fetch(Opcode,opcode_table()),
    {Store,Res,P1} = Func(P0, P0#pic200.accumulator, FVal, K, B),

    % Q4 Fetch
    <<PC:8>>  = register_get( 2, P1#pic200.register ),
    NextInstr = register_get( PC, P1#pic200.program ),
    P2 = P1#pic200{ instruction_register = NextInstr },

    % Q4 Execute
    case store(Store,Whence) of
        accumulator     -> P2#pic200{ accumulator = <<Res:8>> };
        source_register -> P2#pic200{ register = register_set(Addr, <<Res:8>>, P1#pic200.register) };
        no              -> P2
    end.

pic200_run_program(P = #pic200{ register = R, program = Program } ) ->
    <<PC:8>>  = register_get( 2, R ),
    OpcodeBin = register_get( PC, Program ),
    <<T:12>> = OpcodeBin,
    io:format("Will run opcode(~w) ~w~n",[PC,T]),
    execute_opcode(OpcodeBin, P).

execute_opcode(OpcodeBin, P = #pic200{accumulator = <<Acc:8>>}) ->
    #opcode{opcode = Opcode, address = Addr, whence = Whence, constant = K, bit = B} 
        = unpack_opcode(OpcodeBin),
    valid_addr(Addr),
    <<FVal:8>> = register_get( Addr, P#pic200.register ),
    Func = orddict:fetch(Opcode,opcode_table()),
    {Store,Res,ResP} = Func(increment_program_counter(P), Acc, FVal, K, B), % Should we really increment here
    case store(Store,Whence) of
        accumulator     -> ResP#pic200{ accumulator = <<Res:8>> };
        source_register -> ResP#pic200{ register = register_set(Addr, <<Res:8>>, ResP#pic200.register) };
        no              -> ResP
    end.

pic200_print(#pic200{
        accumulator = <<A>>, register = R, callstack = [<<C1>>,<<C2>>],
        program     = P
    }) ->
    io:format("Accumulator: ~8.2B ~3.10B   Callstack: ~8.2B ~8.2b~n",[A,A,C1,C2]),
    io:format("Registers:~n"),
    register_print(R),
    io:format("Program:~n"),
    program_print(P),
    ok.

register_print(R) ->
    register_print(R,0).

program_print(P) ->
    program_print(lists:sublist(P,1,9),0).

register_print([],_) -> io:nl();
register_print([_|R],A) when A > 7, A < 16 -> register_print(R,A+1);
register_print([<<H:8>>|R],A) ->
    case A of
        0  -> io:format("  SPR:~n  ");
        16 -> io:format("GPR:~n  ");
        _  -> nil
    end,
    case A rem 4 of
        3 -> io:format("  ~2.10B : ~8.2.0B ~3.10B~n  ",[A,H,H]), register_print(R,A+1);
        _ -> io:format("  ~2.10B : ~8.2.0B ~3.10B",    [A,H,H]), register_print(R,A+1)
    end.

program_print([],_) -> io:nl();
program_print([HeadBin|R],A) ->
    <<H:12>> = HeadBin,
    OT = opcode_term(HeadBin),
    io:format("  ~2.10B : ~12.2.0B ~5.10B [~6w]~n",[A,H,H,OT]), program_print(R,A+1).

opcode_term(OpcodeBin) ->
    #opcode{opcode = Opcode} = unpack_opcode(OpcodeBin),
    Opcode.

valid_addr(_) -> ok. % Should check in future

store(store,W) -> W;
store(S,_)     -> S.

%% fun(#pic200, Acc, RVal, K, bits)
opcode_table() -> orddict:from_list([
        {nop,   fun(P,_,_,_,_) -> {             no,       0 , P} end },
        {clrw,  fun(P,_,_,_,_) -> {    accumulator,       0 , P} end },
        {clrf,  fun(P,_,_,_,_) -> {source_register,       0 , P} end },
        {movwf, fun(P,A,_,_,_) -> {source_register,       A , P} end },

        {subwf, fun(P,A,F,_,_) -> {store,               F-A , P} end },
        {decf,  fun(P,_,F,_,_) -> {store,               F-1 , P} end },
        {iorwf, fun(P,A,F,_,_) -> {store,          A  bor F , P} end },
        {andwf, fun(P,A,F,_,_) -> {store,          A band F , P} end },
        {xorwf, fun(P,A,F,_,_) -> {store,          A bxor F , P} end },
        {addwf, fun(P,A,F,_,_) -> {store,               A+F , P} end },
        {movf,  fun(P,_,F,_,_) -> {store,                 F , P} end },
        {comf,  fun(P,_,F,_,_) -> {store,            bnot F , P} end },
        {incf,  fun(P,_,F,_,_) -> {store,               F+1 , P} end },
        {decfsz,fun(P,_,1,_,_) -> {accumulator,           0 , P};       % TODO: skip
                   (P,_,F,_,_) -> {store,               F-1 , P} end },
        {rrf,   fun(P,_,F,_,_) -> {store,           F bsr 1 , P} end }, % TODO: carry
        {rlf,   fun(P,_,F,_,_) -> {store,           F bsl 1 , P} end }, % TODO: carry
        {swapf, fun(P,_,F,_,_) -> {store,          swapf(F) , P} end },
        {incfsz,fun(P,_,255,_,_) -> {accumulator,         0 , P};       % TODO: skip
                   (P,_,F,_,_) -> {store,               F+1 , P} end },

        {bcf,   fun(P,_,F,_,B) -> {source_register,F bxor (F band (1 bsl (B-1))), P} end},
        {bsf,   fun(P,_,F,_,B) -> {source_register,F bor (1 bsl (B-1)), P} end },
        {btfsc, fun(P,_,F,_,B) -> {no, F band (1 bsl (B-1)), P} end }, % TODO: check condition
        {btfss, fun(P,_,F,_,B) -> {no, F band (1 bsl (B-1)), P} end }, % TODO: check condition

        {retlw, fun(P,_,_,K,_) -> {accumulator,    K, return(P)} end },
        {call,  fun(P,_,_,K,_) -> {no         ,    0, call(P,K)} end },
        {goto,  fun(P,_,_,K,_) -> {no         ,    0, goto(P,K)} end },
        {movlw, fun(P,_,_,K,_) -> {accumulator,    K,         P} end },
        {iorlw, fun(P,A,_,K,_) -> {accumulator, K bor A,      P} end },
        {andlw, fun(P,A,_,K,_) -> {accumulator, K band A,     P} end },
        {xorlw, fun(P,A,_,K,_) -> {accumulator, K bxor A,     P} end }
    ]).

goto(P = #pic200{ register = R }, K) ->
    case bit_size(K) of
        8 -> P#pic200{ register = register_set(2, K, R) };
        9 -> <<_:1,RealK:8>> = K,
             P#pic200{ register = register_set(2, RealK, R) }
    end.

call(P = #pic200{ register = R, callstack = C }, K) ->
    goto( P#pic200{ callstack = [register_get(2,R),hd(C)] }, K ).

return(P = #pic200{ callstack = [A|R] } ) ->
    goto( P#pic200{ callstack = R }, A ).

swapf(V) ->
    <<A:4,B:4>> = <<V>>,
    <<R:8>> = <<B:4,A:4>>,
    R.

increment_program_counter(P = #pic200{ register = Reg } ) ->
    <<PC:8>> = register_get(2, Reg),
    Inc      = PC + 1,
    P#pic200{ register = register_set(2, <<Inc:8>>, Reg) }.


whence(0) -> accumulator;
whence(1) -> source_register.

unpack_opcode(<<0:12>>)         -> #opcode{opcode = nop    };
unpack_opcode(<<2:12>>)         -> #opcode{opcode = option };
unpack_opcode(<<3:12>>)         -> #opcode{opcode = sleep  };
unpack_opcode(<<4:12>>)         -> #opcode{opcode = clrwdt };
unpack_opcode(<<1:10,A:2>>)     -> #opcode{opcode = tris, address = A};

unpack_opcode(<<1:7,A:5>>)      -> #opcode{opcode = movwf, address = A};
unpack_opcode(<<2:7,_:5>>)      -> #opcode{opcode = clrw };
unpack_opcode(<<3:7,A:5>>)      -> #opcode{opcode = clrf, address = A};

unpack_opcode(<< 2:6,W:1,A:5>>) -> #opcode{opcode = subwf,  address = A, whence = whence(W)};
unpack_opcode(<< 3:6,W:1,A:5>>) -> #opcode{opcode = decf,   address = A, whence = whence(W)};
unpack_opcode(<< 4:6,W:1,A:5>>) -> #opcode{opcode = iorwf,  address = A, whence = whence(W)};
unpack_opcode(<< 5:6,W:1,A:5>>) -> #opcode{opcode = andwf,  address = A, whence = whence(W)};
unpack_opcode(<< 6:6,W:1,A:5>>) -> #opcode{opcode = xorwf,  address = A, whence = whence(W)};
unpack_opcode(<< 7:6,W:1,A:5>>) -> #opcode{opcode = addwf,  address = A, whence = whence(W)};
unpack_opcode(<< 8:6,W:1,A:5>>) -> #opcode{opcode = movf,   address = A, whence = whence(W)};
unpack_opcode(<< 9:6,W:1,A:5>>) -> #opcode{opcode = comf,   address = A, whence = whence(W)};
unpack_opcode(<<10:6,W:1,A:5>>) -> #opcode{opcode = incf,   address = A, whence = whence(W)};
unpack_opcode(<<11:6,W:1,A:5>>) -> #opcode{opcode = decfsz, address = A, whence = whence(W)};
unpack_opcode(<<12:6,W:1,A:5>>) -> #opcode{opcode = rrf,    address = A, whence = whence(W)};
unpack_opcode(<<13:6,W:1,A:5>>) -> #opcode{opcode = rlf,    address = A, whence = whence(W)};
unpack_opcode(<<14:6,W:1,A:5>>) -> #opcode{opcode = swapf,  address = A, whence = whence(W)};
unpack_opcode(<<15:6,W:1,A:5>>) -> #opcode{opcode = incfsz, address = A, whence = whence(W)};

unpack_opcode(<<4:4,B:3,A:5>>)  -> #opcode{opcode = bcf,    address = A, bit = B};
unpack_opcode(<<5:4,B:3,A:5>>)  -> #opcode{opcode = bsf,    address = A, bit = B};
unpack_opcode(<<6:4,B:3,A:5>>)  -> #opcode{opcode = btfsc,  address = A, bit = B};
unpack_opcode(<<7:4,B:3,A:5>>)  -> #opcode{opcode = btfss,  address = A, bit = B};

unpack_opcode(<< 8:4, K:8>>)    -> #opcode{opcode = retlw, constant = K};
unpack_opcode(<< 9:4, K:8>>)    -> #opcode{opcode = call,  constant = K};
unpack_opcode(<< 5:3, K:9>>)    -> #opcode{opcode = goto,  constant = K};
unpack_opcode(<<12:4, K:8>>)    -> #opcode{opcode = movlw, constant = K};
unpack_opcode(<<13:4, K:8>>)    -> #opcode{opcode = iorlw, constant = K};
unpack_opcode(<<14:4, K:8>>)    -> #opcode{opcode = andlw, constant = K};
unpack_opcode(<<15:4, K:8>>)    -> #opcode{opcode = xorlw, constant = K}.

