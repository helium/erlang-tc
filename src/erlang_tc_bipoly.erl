-module(erlang_tc_bipoly).

-export([
    %% bivariate polynomial API
    random/1,
    degree/1,
    reveal/1,
    eval/3,
    row/2,
    commitment/1,
    zeroize/1
]).

-spec random(Degree :: non_neg_integer()) -> reference().
random(Degree) ->
    erlang_tc:random_bivar_poly(Degree).

-spec degree(BiPoly :: reference()) -> non_neg_integer().
degree(BiPoly) ->
    erlang_tc:degree_bivar_poly(BiPoly).

-spec reveal(BiPoly :: reference()) -> string().
reveal(BiPoly) ->
    erlang_tc:reveal_bivar_poly(BiPoly).

-spec eval(BiPoly :: reference(), X :: integer(), Y :: integer()) -> reference().
eval(BiPoly, X, Y) ->
    erlang_tc:eval_bivar_poly(BiPoly, X, Y).

-spec row(BiPoly :: reference(), X :: integer()) -> reference().
row(BiPoly, X) ->
    erlang_tc:row_bivar_poly(BiPoly, X).

-spec commitment(BiPoly :: reference()) -> reference().
commitment(BiPoly) ->
    erlang_tc:commitment_bivar_poly(BiPoly).

-spec zeroize(BiPoly :: reference()) -> reference().
zeroize(BiPoly) ->
    erlang_tc:zeroize_bivar_poly(BiPoly).
