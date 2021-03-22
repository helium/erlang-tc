-module(bicommitment).

-export([
    %% bivariate commitment API
    degree/1,
    eval/3,
    row/2,
    cmp/2,
    reveal/1,
    verify_poly/3,
    verify_point/4,
    validate_point/4,
    serialize/1,
    deserialize/1
]).

-type bicommitment() :: reference().
-export_type([bicommitment/0]).

-spec degree(C :: bicommitment()) -> non_neg_integer().
degree(C) ->
    erlang_tc:degree_bivar_commitment(C).

-spec eval(C :: bicommitment(), X :: integer(), Y :: integer()) -> g1:g1().
eval(C, X, Y) ->
    erlang_tc:eval_bivar_commitment(C, X, Y).

-spec row(C :: bicommitment(), X :: integer()) -> commitment:commitment().
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
    RowPoly :: poly:poly(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_poly(BiCommitment, RowPoly, VerifierID) ->
    RowCommit = erlang_tc:row_bivar_commitment(BiCommitment, VerifierID),
    erlang_tc:cmp_commitment(erlang_tc:commitment_poly(RowPoly), RowCommit).

-spec verify_point(
    BiCommitment :: bicommitment(),
    RowPoly :: poly:poly(),
    SenderID :: non_neg_integer(),
    VerifierID :: non_neg_integer()
) -> boolean().
verify_point(BiCommitment, RowPoly, SenderID, VerifierID) ->
    Val = poly:eval(RowPoly, SenderID),
    G1AffineOne = g1_affine:one(),
    ValG1 = g1_affine:mul(G1AffineOne, Val),
    erlang_tc:cmp_g1(erlang_tc:eval_bivar_commitment(BiCommitment, VerifierID, SenderID), ValG1).

-spec validate_point(BiCommitment :: bicommitment(),
                     SenderID :: non_neg_integer(),
                     VerifierID :: non_neg_integer(),
                     Point :: non_neg_integer() | fr:fr()) -> boolean().
validate_point(BiCommitment, SenderID, VerifierID, Point) when is_reference(Point) ->
    G1AffineOne = g1_affine:one(),
    ValG1 = g1_affine:mul(G1AffineOne, Point),
    erlang_tc:cmp_g1(erlang_tc:eval_bivar_commitment(BiCommitment, VerifierID, SenderID), ValG1);
validate_point(BiCommitment, SenderID, VerifierID, Point) when is_integer(Point) ->
    G1AffineOne = g1_affine:one(),
    ValG1 = g1_affine:mul(G1AffineOne, fr:into(Point)),
    erlang_tc:cmp_g1(erlang_tc:eval_bivar_commitment(BiCommitment, VerifierID, SenderID), ValG1).

-spec serialize(C :: bicommitment()) -> binary().
serialize(C) ->
    erlang_tc:serialize_bivar_commitment(C).

-spec deserialize(B :: binary()) -> bicommitment().
deserialize(B) ->
    erlang_tc:deserialize_bivar_commitment(B).
