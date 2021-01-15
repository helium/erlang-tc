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

-type bipoly() :: reference().
-export_type([bipoly/0]).

-spec random(Degree :: non_neg_integer()) -> bipoly().
random(Degree) ->
    erlang_tc:random_bivar_poly(Degree).

-spec degree(BiPoly :: bipoly()) -> non_neg_integer().
degree(BiPoly) ->
    erlang_tc:degree_bivar_poly(BiPoly).

-spec reveal(BiPoly :: bipoly()) -> string().
reveal(BiPoly) ->
    erlang_tc:reveal_bivar_poly(BiPoly).

-spec eval(BiPoly :: bipoly(), X :: integer(), Y :: integer()) -> erlang_tc_fr:fr().
eval(BiPoly, X, Y) ->
    erlang_tc:eval_bivar_poly(BiPoly, X, Y).

-spec row(BiPoly :: bipoly(), X :: integer()) -> erlang_tc_poly:poly().
row(BiPoly, X) ->
    erlang_tc:row_bivar_poly(BiPoly, X).

-spec commitment(BiPoly :: bipoly()) -> erlang_tc_bicommitment:bicommitment().
commitment(BiPoly) ->
    erlang_tc:commitment_bivar_poly(BiPoly).

-spec zeroize(BiPoly :: bipoly()) -> bipoly().
zeroize(BiPoly) ->
    erlang_tc:zeroize_bivar_poly(BiPoly).
