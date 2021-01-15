-module(erlang_tc_g1_affine).

-export([
    %% G1Affine API
    one/0,
    mul/2
]).

-type g1_affine() :: reference().
-export_type([g1_affine/0]).

-spec one() -> g1_affine().
one() ->
    erlang_tc:g1_affine_one().

-spec mul(G1Affine :: g1_affine(), Fr :: erlang_tc_fr:fr()) -> erlang_tc_g1:g1().
mul(G1Affine, Fr) ->
    erlang_tc:g1_affine_mul(G1Affine, Fr).
