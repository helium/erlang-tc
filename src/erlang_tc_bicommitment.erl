-module(erlang_tc_bicommitment).

-export([
    %% bivariate commitment API
    degree/1,
    eval/3,
    row/2,
    cmp/2,
    reveal/1,
    verify_poly/3,
    verify_point/4
]).

-type bicommitment() :: reference().
-export_type([bicommitment/0]).

-spec degree(C :: bicommitment()) -> non_neg_integer().
degree(C) ->
    erlang_tc:degree_bivar_commitment(C).

-spec eval(C :: bicommitment(), X :: integer(), Y :: integer()) -> erlang_tc_g1:g1().
eval(C, X, Y) ->
    erlang_tc:eval_bivar_commitment(C, X, Y).

-spec row(C :: bicommitment(), X :: integer()) -> erlang_tc_commitment:commitment().
row(C, X) ->
    erlang_tc:row_bivar_commitment(C, X).

-spec cmp(C1 :: bicommitment(), C2 :: bicommitment()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:cmp_bivar_commitment(C1, C2).

-spec reveal(C :: bicommitment()) -> string().
reveal(C) ->
    erlang_tc:reveal_bivar_commitment(C).

-spec verify_poly(
    BiCommitment :: bicommitment(),
    RowPoly :: erlang_tc_poly:poly(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_poly(BiCommitment, RowPoly, VerifierID) ->
    RowCommit = erlang_tc:row_bivar_commitment(BiCommitment, VerifierID),
    erlang_tc:cmp_commitment(erlang_tc:commitment_poly(RowPoly), RowCommit).

-spec verify_point(
    BiCommitment :: bicommitment(),
    RowPoly :: erlang_tc_poly:poly(),
    SenderID :: non_neg_integer(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_point(BiCommitment, RowPoly, SenderID, VerifierID) ->
    Val = erlang_tc:eval_uni_poly(RowPoly, SenderID),
    G1AffineOne = erlang_tc:g1_affine_one(),
    ValG1 = erlang_tc:g1_affine_mul(G1AffineOne, Val),
    erlang_tc:cmp_g1(erlang_tc:eval_bivar_commitment(BiCommitment, VerifierID, SenderID), ValG1).
