-module(erlang_tc_g1_affine).

-export([
    %% G1Affine API
    one/0,
    mul/2
]).

-spec one() -> reference().
one() ->
    erlang_tc:g1_affine_one().

-spec mul(G1Affine :: reference(), Fr :: reference()) -> reference().
mul(G1Affine, Fr) ->
    erlang_tc:g1_affine_mul(G1Affine, Fr).
