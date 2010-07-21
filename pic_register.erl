-module(pic_register).
-export([ new/0, new/2, get/2, set/3, print/1 ]).

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

new()             -> new(32,8).
new(Length,Width) -> lists:duplicate(Length,<<0:Width>>).

get(N, L) -> lists:nth(N+1,L).

set(0, V, L) -> set_fsr(V, L);
set(1, V, L) -> simple_set(1, V, L);  % Timer
set(2, V, L) -> simple_set(2, V, L);  % Program counter
set(3, V, L) -> mask_set(3, V, L, 2#11000000);   % Status register
set(4, V, L) -> mask_set(4, V, L,2#00011111);  % fsr addr
set(5, V, L) -> simple_set(5, V, L);  % osccal
set(6, V, L) -> mask_set(6, V, L, 2#00000111); % GPIO

set(N, _, L) when N < 16 -> L; % NO-OP
set(N, V, L) when N < 32 -> simple_set(N, V, L).

set_fsr(V,L) ->
    <<Addr:8>> = get(4, L),
    case Addr of
        0 -> L;
        _ -> set(Addr, V, L)
    end.

mask_set(N,<<V:8>>,L,Mask) ->
    <<OldValue:8>> = get(N,L),
    NV = V band Mask bor OldValue,
    simple_set(N, <<NV:8>>, L).
    
simple_set(N, V, L) ->
    {H,[_|T]} = lists:split(N,L),
    H ++ [V] ++ T.

print(R) ->
    print(R,0).

print([],_) -> io:nl();
print([_|R],A) when A > 7, A < 16 -> print(R,A+1);
print([<<H:8>>|R],A) ->
    case A of
        0  -> io:format("  SPR:~n  ");
        16 -> io:format("GPR:~n  ");
        _  -> nil
    end,
    case A rem 4 of
        3 -> io:format("  ~2.10B : ~8.2.0B ~3.10B~n  ",[A,H,H]), print(R,A+1);
        _ -> io:format("  ~2.10B : ~8.2.0B ~3.10B",    [A,H,H]), print(R,A+1)
    end.
