-module(g1).

-export([
    %% G1 API
    zero/0,
    cmp/2,
    random/0
]).

-type g1() :: reference().
-export_type([g1/0]).

-spec zero() -> g1().
zero() ->
    erlang_tc:g1_zero().

-spec cmp(G1_1 :: g1(), G1_2 :: g1()) -> boolean().
cmp(G1_1, G1_2) ->
    erlang_tc:cmp_g1(G1_1, G1_2).

-spec random() -> g1().
random() ->
    erlang_tc:g1_random().
