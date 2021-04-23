-module(tc_pubkey).

-export([
    %% public key API
    reveal/1,
    verify/3,
    encrypt/2,
    serialize/1,
    deserialize/1,
    cmp/2
]).

-type pk() :: reference().
-export_type([pk/0]).

-spec reveal(PK :: pk()) -> string().
reveal(PK) ->
    erlang_tc:pk_reveal(PK).

-spec verify(PK :: pk(), Sig :: tc_signature:sig(), Msg :: binary()) -> boolean().
verify(PK, Sig, Msg) ->
    erlang_tc:pk_verify(PK, Sig, Msg).

-spec encrypt(PK :: pk(), Msg :: binary()) -> tc_ciphertext:ciphertext().
encrypt(PK, Msg) ->
    erlang_tc:pk_encrypt(PK, Msg).

-spec serialize(PK :: pk()) -> binary().
serialize(PK) ->
    erlang_tc:pk_serialize(PK).

-spec deserialize(Bin :: binary()) -> pk().
deserialize(PK) ->
    erlang_tc:pk_deserialize(PK).

-spec cmp(PK1 :: pk(), PK2 :: pk()) -> boolean().
cmp(PK1, PK2) ->
    erlang_tc:pk_cmp(PK1, PK2).
