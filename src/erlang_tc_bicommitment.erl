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

-spec degree(C :: reference()) -> non_neg_integer().
degree(C) ->
    erlang_tc:degree_bivar_commitment(C).

-spec eval(C :: reference(), X :: integer(), Y :: integer()) -> reference().
eval(C, X, Y) ->
    erlang_tc:eval_bivar_commitment(C, X, Y).

-spec row(C :: reference(), X :: integer()) -> reference().
row(C, X) ->
    erlang_tc:row_bivar_commitment(C, X).

-spec cmp(C1 :: reference(), C2 :: reference()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:cmp_bivar_commitment(C1, C2).

-spec reveal(C :: reference()) -> string().
reveal(C) ->
    erlang_tc:reveal_bivar_commitment(C).

-spec verify_poly(
    BiCommitment :: reference(),
    RowPoly :: reference(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_poly(BiCommitment, RowPoly, VerifierID) ->
    RowCommit = erlang_tc:row_bivar_commitment(BiCommitment, VerifierID),
    erlang_tc:cmp_commitment(erlang_tc:commitment_poly(RowPoly), RowCommit).

-spec verify_point(
    BiCommitment :: reference(),
    RowPoly :: reference(),
    SenderID :: non_neg_integer(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_point(BiCommitment, RowPoly, SenderID, VerifierID) ->
    Val = erlang_tc:eval_uni_poly(RowPoly, SenderID),
    G1AffineOne = erlang_tc:g1_affine_one(),
    ValG1 = erlang_tc:g1_affine_mul(G1AffineOne, Val),
    erlang_tc:cmp_g1(erlang_tc:eval_bivar_commitment(BiCommitment, VerifierID, SenderID), ValG1).
