-module(tc_public_key_share).

-export([
    verify_decryption_share/3,
    verify_signature_share/3,
    reveal/1,
    combine/2,
    serialize/1,
    deserialize/1,
    cmp/2
]).

-type pk_share() :: reference().

-export_type([pk_share/0]).

-spec verify_decryption_share(
    PKShare :: pk_share(),
    DecShare :: tc_decryption_share:dec_share(),
    Cipher :: tc_ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(PKShare, DecShare, Cipher) ->
    erlang_tc:pk_share_verify_decryption_share(PKShare, DecShare, Cipher).

-spec verify_signature_share(PKShare :: pk_share(),
                             NodeSig :: tc_signature_share:sig_share(),
                             Msg :: binary()) -> boolean().
verify_signature_share(PKShare, NodeSig, Msg) ->
    erlang_tc:pk_share_verify_signature_share(PKShare, NodeSig, Msg).

-spec reveal(PKShare :: pk_share()) -> string().
reveal(PKShare) ->
    erlang_tc:pk_share_reveal(PKShare).

-spec combine(PKS1 :: pk_share(), PKS2 :: pk_share()) -> pk_share().
combine(PKS1, PKS2) ->
    erlang_tc:pk_share_combine(PKS1, PKS2).

-spec serialize(PKS :: pk_share()) -> binary().
serialize(PKS) ->
    erlang_tc:pk_share_serialize(PKS).

-spec deserialize(Bin :: binary()) -> pk_share().
deserialize(Bin) ->
    erlang_tc:pk_share_deserialize(Bin).

-spec cmp(PKS1 :: pk_share(), PKS2 :: pk_share()) -> boolean().
cmp(PKS1, PKS2) ->
    erlang_tc:pk_share_cmp(PKS1, PKS2).
