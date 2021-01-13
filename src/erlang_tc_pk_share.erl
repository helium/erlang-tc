-module(erlang_tc_pk_share).

-export([
    %% Signature API
    from_pk/1
]).

-spec from_pk(PK :: reference()) -> reference().
from_pk(PK) ->
    erlang_tc:share_from_pk(PK).
