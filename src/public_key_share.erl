-module(public_key_share).

-export([
    %% Signature API
    verify_decryption_share/3,
    verify_signature_share/3,
    reveal/1,
    to_bytes/1
]).

-type pk_share() :: reference().

-export_type([pk_share/0]).

-spec verify_decryption_share(
    PKShare :: pk_share(),
    DecShare :: decryption_share:dec_share(),
    Cipher :: ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(PKShare, DecShare, Cipher) ->
    erlang_tc:pk_share_verify_decryption_share(PKShare, DecShare, Cipher).

-spec verify_signature_share(PKShare :: pk_share(),
                             NodeSig :: signature_share:sig_share(),
                             Msg :: binary()) -> boolean().
verify_signature_share(PKShare, NodeSig, Msg) ->
    erlang_tc:pk_share_verify_signature_share(PKShare, NodeSig, Msg).

-spec reveal(PKShare :: pk_share()) -> string().
reveal(PKShare) ->
    erlang_tc:pk_share_reveal(PKShare).

-spec to_bytes(PKShare :: pk_share()) -> binary().
to_bytes(PKShare) ->
    erlang_tc:pk_share_to_bytes(PKShare).
