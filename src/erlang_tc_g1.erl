-module(erlang_tc_g1).

-export([
    %% G1 API
    zero/0,
    cmp/2
]).

-spec zero() -> reference().
zero() ->
    erlang_tc:g1_zero().

-spec cmp(G1_1 :: reference(), G1_2 :: reference()) -> boolean().
cmp(G1_1, G1_2) ->
    erlang_tc:cmp_g1(G1_1, G1_2).
