-module(erlang_tc_commitment).

-export([
    %% commitment API
    degree/1,
    eval/2,
    cmp/2,
    reveal/1,
    add/2
]).

-spec degree(C :: reference()) -> non_neg_integer().
degree(C) ->
    erlang_tc:degree_commitment(C).

-spec eval(C :: reference(), Point :: integer()) -> reference().
eval(C, Point) ->
    erlang_tc:eval_commitment(C, Point).

-spec cmp(C1 :: reference(), C2 :: reference()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:cmp_commitment(C1, C2).

-spec reveal(C :: reference()) -> string().
reveal(C) ->
    erlang_tc:reveal_commitment(C).

-spec add(C1 :: reference(), C2 :: reference()) -> reference().
add(C1, C2) ->
    erlang_tc:add_commitment(C1, C2).
