-module(erlang_tc_pk).

-export([
    %% public key API
    reveal/1,
    to_bytes/1
]).

-spec reveal(PK :: reference()) -> reference().
reveal(PK) ->
    erlang_tc:pk_reveal(PK).

-spec to_bytes(PK :: reference()) -> binary().
to_bytes(PK) ->
    erlang_tc:pk_to_bytes(PK).
