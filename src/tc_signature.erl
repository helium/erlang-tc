-module(tc_signature).

-export([
    %% Signature API
    parity/1,
    cmp/2,
    aggregate_from_sigs/1,
    core_aggregate_verify/2,
    serialize/1,
    deserialize/1
]).


-type sig() :: reference().
-export_type([sig/0]).

-spec parity(Sig :: sig()) -> boolean().
parity(Sig) ->
    erlang_tc:sig_parity(Sig).

-spec cmp(S1 :: sig(), S2 :: sig()) -> boolean().
cmp(S1, S2) ->
    erlang_tc:sig_cmp(S1, S2).

-spec aggregate_from_sigs(Sigs :: [sig()]) -> sig().
aggregate_from_sigs(Sigs) ->
    erlang_tc:sig_aggregate_from_sigs(Sigs).

-spec core_aggregate_verify(Sig :: sig(), PubkeysAndMsgs :: [{Pubkey :: tc_pubkey:pk(), Msg :: binary()}]) -> boolean().
core_aggregate_verify(Sig, PubkeysAndMsgs) ->
    erlang_tc:sig_core_aggregate_verify(Sig, PubkeysAndMsgs).

-spec serialize(Sig :: sig()) -> binary().
serialize(Sig) ->
    erlang_tc:sig_serialize(Sig).

-spec deserialize(BinSig :: binary()) -> sig().
deserialize(BinSig) ->
    erlang_tc:sig_deserialize(BinSig).
