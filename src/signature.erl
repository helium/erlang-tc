-module(signature).

-export([
    %% Signature API
    to_bytes/1,
    parity/1,
    cmp/2,
    serialize/1,
    deserialize/1
]).


-type sig() :: reference().
-export_type([sig/0]).

-spec to_bytes(Sig :: sig()) -> binary().
to_bytes(Sig) ->
    erlang_tc:sig_to_bytes(Sig).

-spec parity(Sig :: sig()) -> boolean().
parity(Sig) ->
    erlang_tc:sig_parity(Sig).

-spec cmp(S1 :: sig(), S2 :: sig()) -> boolean().
cmp(S1, S2) ->
    erlang_tc:sig_cmp(S1, S2).

-spec serialize(Sig :: sig()) -> binary().
serialize(Sig) ->
    erlang_tc:sig_serialize(Sig).

-spec deserialize(BinSig :: binary()) -> sig().
deserialize(BinSig) ->
    erlang_tc:sig_deserialize(BinSig).
