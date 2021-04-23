-module(tc_bipoly).

-export([
    %% bivariate polynomial API
    random/1,
    degree/1,
    reveal/1,
    eval/3,
    row/2,
    commitment/1,
    zeroize/1,
    with_secret/2,
    serialize/1,
    deserialize/1
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

-spec eval(BiPoly :: bipoly(), X :: integer(), Y :: integer()) -> tc_fr:fr().
eval(BiPoly, X, Y) ->
    erlang_tc:eval_bivar_poly(BiPoly, X, Y).

-spec row(BiPoly :: bipoly(), X :: integer()) -> tc_poly:poly().
row(BiPoly, X) ->
    erlang_tc:row_bivar_poly(BiPoly, X).

-spec commitment(BiPoly :: bipoly()) -> tc_bicommitment:bicommitment().
commitment(BiPoly) ->
    erlang_tc:commitment_bivar_poly(BiPoly).

-spec zeroize(BiPoly :: bipoly()) -> bipoly().
zeroize(BiPoly) ->
    erlang_tc:zeroize_bivar_poly(BiPoly).

-spec with_secret(Secret :: non_neg_integer(), Degree :: non_neg_integer()) -> bipoly().
with_secret(Secret, Degree) ->
    erlang_tc:with_secret_bivar_poly(Secret, Degree).

-spec serialize(C :: bipoly()) -> binary().
serialize(C) ->
    erlang_tc:serialize_bivar_poly(C).

-spec deserialize(B :: binary()) -> bipoly().
deserialize(B) ->
    erlang_tc:deserialize_bivar_poly(B).
