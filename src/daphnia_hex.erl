-module (daphnia_hex).

-export ([bin_to_hex_string/1, list_to_hex/1]).

bin_to_hex_string(Bin) ->
    lists:flatten(list_to_hex(binary_to_list(Bin))).

list_to_hex(L) -> lists:map(fun(X) -> int_to_hex(X) end, L).
int_to_hex(N) when N < 256         -> [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10                 -> $0+N;
hex(N) when N >= 10, N < 16        -> $a + (N-10).
