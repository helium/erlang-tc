-module(commitment).

-export([
    %% commitment API
    degree/1,
    eval/2,
    cmp/2,
    reveal/1,
    add/2,
    serialize/1,
    deserialize/1,
    public_key/1
]).

-type commitment() :: reference().
-export_type([commitment/0]).

-spec degree(C :: commitment()) -> non_neg_integer().
degree(C) ->
    erlang_tc:degree_commitment(C).

-spec eval(C :: commitment(), Point :: integer()) -> g1:g1().
eval(C, Point) ->
    erlang_tc:eval_commitment(C, Point).

-spec cmp(C1 :: commitment(), C2 :: commitment()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:cmp_commitment(C1, C2).

-spec reveal(C :: commitment()) -> string().
reveal(C) ->
    erlang_tc:reveal_commitment(C).

-spec add(C1 :: commitment(), C2 :: commitment()) -> commitment().
add(C1, C2) ->
    erlang_tc:add_commitment(C1, C2).

-spec serialize(C :: commitment()) -> binary().
serialize(C) ->
    erlang_tc:serialize_commitment(C).

-spec deserialize(B :: binary()) -> commitment().
deserialize(B) ->
    erlang_tc:deserialize_commitment(B).

-spec public_key(C :: commitment()) -> public_key:pk().
public_key(C) ->
    erlang_tc:commitment_public_key(C).
