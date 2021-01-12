-module(erlang_tc_fr).

-export([
    %% field API
    into/1,
    cmp/2,
    zero/0,
    add_assign/2
]).

-spec into(Num :: integer()) -> reference().
into(Num) ->
    erlang_tc:into_fr(Num).

-spec cmp(FR1 :: reference(), FR2 :: reference()) -> boolean().
cmp(FR1, FR2) ->
    erlang_tc:cmp_fr(FR1, FR2).

-spec zero() -> reference().
zero() ->
    erlang_tc:zero_fr().

-spec add_assign(FR1 :: reference(), FR2 :: reference()) -> reference().
add_assign(FR1, FR2) ->
    erlang_tc:add_assign_fr(FR1, FR2).
