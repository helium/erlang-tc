-module(erlang_tc_sig).

-export([
    %% Signature API
    to_bytes/1,
    parity/1
]).

-spec to_bytes(Sig :: reference()) -> binary().
to_bytes(Sig) ->
    erlang_tc:sig_to_bytes(Sig).

-spec parity(Sig :: reference()) -> boolean().
parity(Sig) ->
    erlang_tc:sig_parity(Sig).
