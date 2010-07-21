-module(pic_emulator).
%-compile(export_all). %% REPLACE

-export([new/0, print/1, load_program/2,
        run_program/1,run_program/2, cycle/3]).

-record(pic200,{ 
        accumulator = <<0>>,           %        8 bits
        register = pic_register:new(), %       32 bytes
        callstack = [<<0>>,<<0>>],     %        2 bytes
        program,                       % 256 x 12 bits

        option,                        %        8 bits
        trisgpio,                      %        8 bits (but only 4bits used)

        instruction_register = <<0:12>>,
        current_opcode,
        current_value
    }).

-record(opcode,{
        opcode    = nop,
        address   = 16#1F, % Last register is safe default
        whence    = accumulator,
        constant  = 0,
        bit       = 0
    }).

new() ->
    #pic200{
        register = pic_register:new(),
        program  = lists:duplicate(255,<<0:12>>)
    }.

load_program(P, File) ->
    {ok, Bin} = file:read_file(File),
    Program = read_program(Bin,[]),
    E = 255 - length(Program),
    P#pic200{ program = Program ++ lists:duplicate(E,<<0:12>>) }.

read_program(<<>>, Acc) -> lists:reverse(Acc);
read_program(<<H:12,Rest/bitstring>>, Acc) ->
    read_program(Rest, [<<H:12>>|Acc]).
%read_program(<<_/bitstring>>, Acc) -> lists:reverse(Acc).

%% Description of one cycle (4 clocks)
%     Fetch cycle       Execution cycle
% Q1. inc_pc            fetch_instr -> ir2
% Q2.   -               read_memory (from reg)
% Q3.   -               execute_instr
% Q4. fetch_pc -> ir1   write_result  (to reg)

cycle(P,q1,fetch) -> increment_program_counter(P);
cycle(P,q4,fetch) ->
    <<PC:8>>  = pic_register:get( 2,  P#pic200.register ),
    NextInstr = pic_register:get( PC, P#pic200.program ),
    P#pic200{ instruction_register = NextInstr };
cycle(P,_,fetch) -> P;

cycle(P,q1,execute) ->
    O = unpack_opcode(P#pic200{ instruction_register } ),
    P#pic200{ current_opcode = O };
cycle(P,q2,execute) ->
    O = P#pic200.current_opcode,
    <<FVal:8>> = pic_register:get( O#opcode.address, P#pic200.register ),
    P#pic200{ current_value = FVal };
cycle(P,q3,execute) -> P;
cycle(P,q4,execute) -> P.

run_program(P , 0 ) -> P;
run_program(P, Steps ) ->
    PN = run_program(P),
    run_program(PN, Steps - 1).

run_program(P = #pic200{ register = R, program = Program } ) ->
    <<PC:8>>  = pic_register:get( 2, R ),
    OpcodeBin = pic_register:get( PC, Program ),
    execute_opcode(OpcodeBin, P).

execute_opcode(OpcodeBin, P = #pic200{accumulator = <<Acc:8>>}) ->
    #opcode{opcode = Opcode, address = Addr, whence = Whence, constant = K, bit = B} 
        = unpack_opcode(OpcodeBin),
    valid_addr(Addr),
    <<FVal:8>> = pic_register:get( Addr, P#pic200.register ),
    Func = orddict:fetch(Opcode,opcode_table()),
    {Store,Res,ResP,Status} = Func(increment_program_counter(P), Acc, FVal, K, B), % Should we really increment here

    case store(Store,Whence) of
        accumulator     -> ResP#pic200{ accumulator = <<Res:8>> };
        source_register -> ResP#pic200{ register = pic_register:set(Addr, <<Res:8>>, ResP#pic200.register) };
        no              -> ResP
    end.

chain(Obj, []) -> Obj;
chain(Obj, [F|Funcs]) -> chain(F(Obj), Funcs).

print(#pic200{
        accumulator = <<A>>, register = R, callstack = [<<C1>>,<<C2>>],
        program     = P
    }) ->
    io:format("Accumulator: ~8.2B ~3.10B   Callstack: ~8.2B ~8.2b~n",[A,A,C1,C2]),
    io:format("Registers:~n"),
    pic_register:print(R),
    io:format("Program:~n"),
    program_print(P),
    ok.

program_print(P) ->
    program_print(lists:sublist(P,1,9),0).

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

%% fun(#pic200, Acc, RVal, K, bits) -> {store, res, P}
opcode_table() -> orddict:from_list([
        {nop,   fun(P,_,_,_,_) -> {             no,       0 , P, []} end },

        {clrwdt,fun(P,_,_,_,_) -> {             no,       0 , P, [to,pd]} end },
        {sleep, fun(P,_,_,_,_) -> {             no,       0 , P, [to,pd]} end },

        {clrw,  fun(P,_,_,_,_) -> {    accumulator,       0 , P, [z]} end },
        {clrf,  fun(P,_,_,_,_) -> {source_register,       0 , P, [z]} end },
        {movwf, fun(P,A,_,_,_) -> {source_register,       A , P, []} end },

        {subwf, fun(P,A,F,_,_) -> {store,               F-A , P, [z,c,dc]} end },
        {decf,  fun(P,_,F,_,_) -> {store,               F-1 , P, [z]} end },
        {iorwf, fun(P,A,F,_,_) -> {store,          A  bor F , P, [z]} end },
        {andwf, fun(P,A,F,_,_) -> {store,          A band F , P, [z]} end },
        {xorwf, fun(P,A,F,_,_) -> {store,          A bxor F , P, [z]} end },
        {addwf, fun(P,A,F,_,_) -> {store,               A+F , P, [z,c,dc]} end },
        {movf,  fun(P,_,F,_,_) -> {store,                 F , P, [z]} end },
        {comf,  fun(P,_,F,_,_) -> {store,            bnot F , P, [z]} end },
        {incf,  fun(P,_,F,_,_) -> {store,               F+1 , P, [z]} end },
        {decfsz,fun(P,_,1,_,_) -> {accumulator,           0 , skip(P), []};
                   (P,_,F,_,_) -> {store,               F-1 , P, []} end },
        {rrf,   fun(P,_,F,_,_) -> {store,           F bsr 1 , P, [c]} end }, % TODO: carry
        {rlf,   fun(P,_,F,_,_) -> {store,           F bsl 1 , P, [c]} end }, % TODO: carry
        {swapf, fun(P,_,F,_,_) -> {store,          swapf(F) , P, []} end },
        {incfsz,fun(P,_,255,_,_) -> {accumulator,         0 , skip(P), []};
                   (P,_,F,_,_) -> {store,               F+1 , P, []} end },

        {bcf,   fun(P,_,F,_,B) -> {source_register,F bxor (F band (1 bsl (B-1))), P, []} end},
        {bsf,   fun(P,_,F,_,B) -> {source_register,F bor (1 bsl (B-1)), P, []} end },
        {btfsc, fun(P,_,F,_,B) -> {no, F band (1 bsl (B-1)), P, []} end }, % TODO: check condition
        {btfss, fun(P,_,F,_,B) -> {no, F band (1 bsl (B-1)), P, []} end }, % TODO: check condition

        {retlw, fun(P,_,_,K,_) -> {accumulator,    K, return(P), []} end },
        {call,  fun(P,_,_,K,_) -> {no         ,    0, call(P,K), []} end },
        {goto,  fun(P,_,_,K,_) -> {no         ,    0, goto(P,K), []} end },
        {movlw, fun(P,_,_,K,_) -> {accumulator,    K,         P, []} end },
        {iorlw, fun(P,A,_,K,_) -> {accumulator, K bor A,      P, [z]} end },
        {andlw, fun(P,A,_,K,_) -> {accumulator, K band A,     P, [z]} end },
        {xorlw, fun(P,A,_,K,_) -> {accumulator, K bxor A,     P, []} end }
    ]).

clear_insreg(P) -> P#pic200{ instruction_register = <<0:12>> }.

skip(P = #pic200{ register = R}) ->
    <<O:8>> = pic_register:get(2,R),
    clear_insreg(P#pic200{ register = pic_register:set(2,O+1,R) }).

goto(P = #pic200{ register = R }, K) ->
    Pn = case bit_size(K) of
        8 -> P#pic200{ register = pic_register:set(2, K, R) };
        9 -> <<_:1,RealK:8>> = K,
             P#pic200{ register = pic_register:set(2, RealK, R) }
    end,
    clear_insreg(Pn).

call(P = #pic200{ register = R, callstack = C }, K) ->
    goto( P#pic200{ callstack = [pic_register:get(2,R),hd(C)] }, K ).

return(P = #pic200{ callstack = [A|R] } ) ->
    goto( P#pic200{ callstack = R }, A ).

swapf(V) ->
    <<A:4,B:4>> = <<V>>,
    <<R:8>> = <<B:4,A:4>>,
    R.

increment_program_counter(P = #pic200{ register = Reg } ) ->
    <<PC:8>> = pic_register:get(2, Reg),
    Inc      = PC + 1,
    P#pic200{ register = pic_register:set(2, <<Inc:8>>, Reg) }.

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

