-module(erlang_tc_pk).

-export([
    %% public key API
    reveal/1,
    to_bytes/1,
    verify/3,
    encrypt/2
]).

-spec reveal(PK :: reference()) -> reference().
reveal(PK) ->
    erlang_tc:pk_reveal(PK).

-spec to_bytes(PK :: reference()) -> binary().
to_bytes(PK) ->
    erlang_tc:pk_to_bytes(PK).

-spec verify(PK :: reference(), Sig :: reference(), Msg :: binary()) -> binary().
verify(PK, Sig, Msg) ->
    erlang_tc:pk_verify(PK, Sig, Msg).

-spec encrypt(PK :: reference(), Msg :: binary()) -> reference().
encrypt(PK, Msg) ->
    erlang_tc:pk_encrypt(PK, Msg).
