-module(ciphertext).

-export([
    %% Ciphertext API
    verify/1,
    cmp/2,
    serialize/1,
    deserialize/1
]).

-type ciphertext() :: reference().
-export_type([ciphertext/0]).

-spec verify(Ciphertext :: ciphertext()) -> boolean().
verify(Ciphertext) ->
    erlang_tc:ciphertext_verify(Ciphertext).

-spec cmp(C1 :: ciphertext(), C2 :: ciphertext()) -> boolean().
cmp(C1, C2) ->
    erlang_tc:ciphertext_cmp(C1, C2).

-spec serialize(Ciphertext :: ciphertext()) -> binary().
serialize(Ciphertext) ->
    erlang_tc:ciphertext_serialize(Ciphertext).

-spec deserialize(BinC :: binary()) -> ciphertext().
deserialize(BinC) ->
    erlang_tc:ciphertext_deserialize(BinC).
