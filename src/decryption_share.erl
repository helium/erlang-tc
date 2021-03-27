-module(decryption_share).

-export([
    serialize/1,
    deserialize/1
]).

-type dec_share() :: reference().
-export_type([dec_share/0]).

-spec serialize(DecShare :: dec_share()) -> binary().
serialize(DecShare) ->
    erlang_tc:dec_share_serialize(DecShare).

-spec deserialize(BinSigShare :: binary()) -> dec_share().
deserialize(BinSigShare) ->
    erlang_tc:dec_share_deserialize(BinSigShare).
