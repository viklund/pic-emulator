-module(new_pic_emulator).
-compile(export_all).

-record(pic, {
        type,

        accumulator,
        register_file,
        callstack,
        program,

        special_registers, % orddict
        opcodes,           % fun/1

        instruction_register, % Bin value of next instr
        current_opcode,       % Some kind of opcode desc, possibly an orddict
        current_value         % Either value of last execution or last fetch
    }
).

start_clock(P) ->
    Pid = spawn( fun() -> clock({q1,q2,q3,q4},1,P) end ),
    register(clock, Pid),
    Pid.

stop_clock() ->
    clock ! exit,
    unregister(clock).

clock(T,C,Target) ->
    receive
        exit -> ok
    after 100 ->
        Target ! element(C,T),
        clock(T, C rem 4 + 1, Target)
    end.

start_pic(Pic) ->
    Pid = spawn( fun() -> run(Pic) end ),
    register(running_pic, Pid),
    Pid.

stop_pic() ->
    running_pic ! {self(), exit},
    P = receive
        V -> V
    end,
    P.

create_pic200() ->
    #pic{
        type                 = pic200,

        accumulator          = <<0:8>>,
        register_file        = pic_register:new(),
        callstack            = [<<0:8>>,<<0:8>>],
        program              = lists:duplicate(255,<<0:12>>),

        special_registers    = orddict:from_list([]), % Flesh out later
        opcodes              = fun pic200_opcodes/1,

        instruction_register = <<0:12>>,
        current_opcode       = orddict:from_list([]),
        current_value        = <<0:8>>
    }.

load_program(P = #pic{register_file = R}, File) ->
    {ok, Bin} = file:read_file(File),
    Program = read_program(Bin,[]),
    E = 255 - length(Program),
    P#pic{ 
        program = Program ++ lists:duplicate(E,<<0:12>>),
        register_file = pic_register:set(2,<<255:8>>,R)
    }.

read_program(<<>>, Acc) -> lists:reverse(Acc);
read_program(<<H:12,Rest/bitstring>>, Acc) ->
    read_program(Rest, [<<H:12>>|Acc]);
read_program(<<_/bitstring>>, Acc) -> lists:reverse(Acc). % Program code overflow

run(P = #pic{}) ->
    show_info(P),
    receive
        q1 -> P0 = increment_program_counter(P), run( decode_instruction(P0) );
        q2 -> run( fetch_from_register( P ) );
        q3 -> run( execute_instruction( P ) );
        q4 -> P0 = fetch_next_instruction(P), run( store_result( P0 ) );
        exit -> ok;
        {Pid, exit} -> Pid ! P, ok;
        S  -> io:format("Got signal ~w~n",[S]), run(P)
    end.

show_info( P = #pic{ accumulator = <<A:8>>, register_file = R, instruction_register = <<I:12>> }) ->
    <<PC:8>> = pic_register:get(2,R),
    io:format("W=~8.2.0B PC=~8.2.0B I=~12.2.0B",[A,PC,I]),
    case orddict:find( name, P#pic.current_opcode ) of
        {ok, Name} -> io:format(" ~w~n", [Name]);
        error      -> io:format("~n")
    end,

    lists:map( fun(V) -> <<Vl:8>> = pic_register:get(V,R), io:format("~8.2.0B ",[Vl]) end, lists:seq(0,7) ),
    io:format("~n"),
    lists:map( fun(V) -> <<Vl:8>> = pic_register:get(V,R), io:format("~8.2.0B ",[Vl]) end, lists:seq(8,15) ),
    io:format("~n"),
    lists:map( fun(V) -> <<Vl:8>> = pic_register:get(V,R), io:format("~8.2.0B ",[Vl]) end, lists:seq(16,23) ),
    io:format("~n"),
    lists:map( fun(V) -> <<Vl:8>> = pic_register:get(V,R), io:format("~8.2.0B ",[Vl]) end, lists:seq(24,31) ),
    io:format("~n").


decode_instruction( P = #pic{ instruction_register = Ir, opcodes = Oc } ) ->
    O = Oc(Ir),
    G = orddict:from_list(O(Ir)),
    P#pic{ current_opcode = G }.

fetch_from_register( P = #pic{ current_opcode = O, register_file = R } ) ->
    P#pic{ current_value = case orddict:find(fetch,O) of
        {ok,Addr} -> pic_register:get(Addr, R);
        error     -> <<0:8>>
    end}.

% This method should also check for the status thingy
execute_instruction( P = #pic{ current_opcode = O, current_value = <<Cv:8>>, accumulator = <<A:8>> } ) ->
    Val = case orddict:find(execute,O) of
        {ok, Func} -> Func(A,Cv);
        error      -> Cv
    end,
    P#pic{ current_value = <<Val:8>> }.

store_result( P = #pic{ current_opcode = O, current_value = Cv, register_file = R, accumulator = A } ) ->
    {R1,A1} = case orddict:find(store,O) of
        {ok, {source_register, Addr}} -> {pic_register:set(Addr,Cv,R),A};
        {ok, accumulator }            -> {R, Cv};
        error                         -> {R,A}
    end,
    P0 = P#pic{register_file = R1, accumulator = A1},
    P1 = case orddict:find(postproc,O) of
        {ok, Func} -> Func(P0,Cv);
        error      -> P0
    end,
    P1.


%% Byte oriented file_register operations
pic200_opcodes(<< 7:6,_:6>>) -> fetch_store_opcode(addwf  , fun (A,F) -> A + F end); % More status
pic200_opcodes(<< 5:6,_:6>>) -> fetch_store_opcode(andwf  , fun (A,F) -> A band F end);
pic200_opcodes(<< 3:7,_:5>>) -> fetch_store_opcode(clrf   , fun (_,_) -> 0 end);
pic200_opcodes(<< 2:7,0:5>>) -> fetch_store_opcode(clrw   , fun (_,_) -> 0 end);
pic200_opcodes(<< 9:6,_:6>>) -> fetch_store_opcode(comf   , fun (_,F) -> F bxor 255 end);
pic200_opcodes(<< 3:6,_:6>>) -> fetch_store_opcode(decf   , fun (_,F) -> F - 1 end);
pic200_opcodes(<<11:6,_:6>>) -> skip_if_zero_opcode(decfsz, fun (_,F) -> F - 1 end);
pic200_opcodes(<<10:6,_:6>>) -> fetch_store_opcode(incf   , fun (_,F) -> F + 1 end);
pic200_opcodes(<<15:6,_:6>>) -> skip_if_zero_opcode(incfsz, fun (_,F) -> F + 1 end);
pic200_opcodes(<< 4:6,_:6>>) -> fetch_store_opcode(iorwf  , fun (A,F) -> A bor F end);
pic200_opcodes(<< 8:6,_:6>>) -> fetch_store_opcode(movf   , fun (_,F) -> F end);
pic200_opcodes(<< 1:7,_:5>>) -> movwf();
pic200_opcodes(<<0:12>>)     -> fun(_) -> [{name, nop}] end;
pic200_opcodes(<<13:6,_:6>>) -> fetch_store_opcode(rlf    , fun (_,F) -> F bsl 1 end); % Carry bit
pic200_opcodes(<<12:6,_:6>>) -> fetch_store_opcode(rrf    , fun (_,F) -> F bsr 1 end); % Carry bit
pic200_opcodes(<< 2:6,_:6>>) -> fetch_store_opcode(subwf  , fun (A,F) -> F - A end); % More status
pic200_opcodes(<<14:6,_:6>>) -> swapf();
pic200_opcodes(<< 6:6,_:6>>) -> fetch_store_opcode(xorwf  , fun (A,F) -> F bxor A end);

pic200_opcodes(<<6:4,_:8>>) -> bit_skip_if_zero();

pic200_opcodes(<<12:4,_:8>>) -> move_literal_opcode();
pic200_opcodes(<< 5:3,_:9>>) -> goto_opcode().

bit_skip_if_zero() ->
    fun(<<Bin:12>>) ->
        Addr   = 31 band Bin,
        Bit    = 1 bsl (2#11100000 band Bin bsr 5),
        [
            {name,    btfsc},
            {fetch,   Addr},
            {execute, fun(_,V) -> V band Bit end},
            {postproc, fun skip_if_zero/2 }
        ]
    end.
            

goto_opcode() ->
    fun(<<Bin:12>>) ->
            Address = 2#11111111 band Bin,
            [
                {name, goto},
                {postproc, fun (P,_) ->
                            P0 = set_program_counter(Address, P),
                            P0#pic{ instruction_register = <<0:12>> } end}
            ]
    end.

move_literal_opcode() ->
    fun(<<Bin:12>>) ->
        Val = 2#11111111 band Bin,
        [
            {name, movlw},
            {store, accumulator},
            {execute, fun (_,_) -> Val end}
        ]
    end.

swapf() ->
    fun(<<Bin:12>>) ->
        Whence = 32 band Bin bsr 5,
        Addr   = 31 band Bin,
        [
            {name,    swapf},
            {fetch,   Addr},
            {store,   whence(Whence,Addr)},
            {execute, fun(_,F) ->
                <<A:4,B:4>> = <<F>>,
                <<R:8>> = <<B:4,A:4>>,
                R end }
        ]
    end.

movwf() ->
    fun(<<Bin:12>>) ->
        Whence = 32 band Bin bsr 5,
        Addr   = 31 band Bin,
        [
            {name,    movwf},
            {execute, fun (A,_) -> A end},
            {store,   whence(Whence, Addr)}
        ]
    end.

skip_if_zero_opcode(Name, ExecuteFun) ->
    fun(<<Bin:12>>) ->
        Whence = 32 band Bin bsr 5,
        Addr   = 31 band Bin,
        [
            {name,    Name},
            {fetch,   Addr},
            {execute, ExecuteFun},
            {store,   whence(Whence, Addr)},
            {postproc, fun skip_if_zero/2 }
        ]
    end.

skip_if_zero(P,<<0:8>>) ->
    %P0 = increment_program_counter(P),
    P#pic{ instruction_register = <<0:12>> };
skip_if_zero(P,_) -> P.

fetch_store_opcode(Name, ExecuteFun) ->
    fun(<<Bin:12>>) ->
        Whence = 32 band Bin bsr 5,
        Addr   = 31 band Bin,
        [
            {name,    Name},
            {fetch,   Addr},
            {store,   whence(Whence,Addr)},
            {execute, ExecuteFun},
            {status,  [{z,fun z_status/2}] }
        ]
    end.

z_status(P,_) -> P.

set_program_counter(PVal, P = #pic{ register_file = Reg } ) ->
    P#pic{ register_file = pic_register:set(2, <<PVal:8>>, Reg) }.

increment_program_counter(P = #pic{ register_file = Reg } ) ->
    <<PC:8>> = pic_register:get(2, Reg),
    Inc      = PC + 1,
    P#pic{ register_file = pic_register:set(2, <<Inc:8>>, Reg) }.

fetch_next_instruction(P = #pic{ register_file = Reg, program = Pr } ) ->
    <<PC:8>> = pic_register:get(2, Reg),
    Bin = lists:nth(PC+1,Pr),
    P#pic{ instruction_register = Bin }.

whence(0,_)    -> accumulator;
whence(1,Addr) -> {source_register,Addr}.
