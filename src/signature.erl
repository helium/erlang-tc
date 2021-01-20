-module(signature).

-export([
    %% Signature API
    to_bytes/1,
    parity/1
]).


-type sig() :: reference().
-export_type([sig/0]).

-spec to_bytes(Sig :: sig()) -> binary().
to_bytes(Sig) ->
    erlang_tc:sig_to_bytes(Sig).

-spec parity(Sig :: sig()) -> boolean().
parity(Sig) ->
    erlang_tc:sig_parity(Sig).
