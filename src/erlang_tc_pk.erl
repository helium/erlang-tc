-module(erlang_tc_pk).

-export([
    %% public key API
    reveal/1,
    to_bytes/1,
    verify/3,
    encrypt/2
]).

-type pk() :: reference().
-export_type([pk/0]).

-spec reveal(PK :: pk()) -> string().
reveal(PK) ->
    erlang_tc:pk_reveal(PK).

-spec to_bytes(PK :: pk()) -> binary().
to_bytes(PK) ->
    erlang_tc:pk_to_bytes(PK).

-spec verify(PK :: pk(), Sig :: erlang_tc_sig:sig(), Msg :: binary()) -> binary().
verify(PK, Sig, Msg) ->
    erlang_tc:pk_verify(PK, Sig, Msg).

-spec encrypt(PK :: pk(), Msg :: binary()) -> erlang_tc_ciphertext:ciphertext().
encrypt(PK, Msg) ->
    erlang_tc:pk_encrypt(PK, Msg).
