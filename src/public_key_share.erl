-module(public_key_share).

-export([
    %% Signature API
    from_pk/1,
    verify_decryption_share/3,
    verify/3
]).

-type pk_share() :: reference().

-export_type([pk_share/0]).

-spec from_pk(PK :: public_key:pk()) -> pk_share().
from_pk(PK) ->
    erlang_tc:share_from_pk(PK).

-spec verify_decryption_share(
    PKShare :: pk_share(),
    DecShare :: decryption_share:dec_share(),
    Cipher :: ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(PKShare, DecShare, Cipher) ->
    erlang_tc:pk_share_verify_decryption_share(PKShare, DecShare, Cipher).

-spec verify(PKShare :: pk_share(),
             NodeSig :: signature_share:sig_share(),
             Msg :: binary()) -> boolean().
verify(PKShare, NodeSig, Msg) ->
    erlang_tc:pk_share_verify(PKShare, NodeSig, Msg).
