-module(tc_signature_share).

-export([
    serialize/1,
    deserialize/1
]).

-type sig_share() :: reference().

-export_type([sig_share/0]).

-spec serialize(SigShare :: sig_share()) -> binary().
serialize(SigShare) ->
    erlang_tc:sig_share_serialize(SigShare).

-spec deserialize(BinSigShare :: binary()) -> sig_share().
deserialize(BinSigShare) ->
    erlang_tc:sig_share_deserialize(BinSigShare).
