-module(erlang_tc_pk_share).

-export([
    %% Signature API
    from_pk/1,
    verify_decryption_share/3,
    verify/3
]).

-type pk_share() :: reference().

-export_type([pk_share/0]).

-spec from_pk(PK :: erlang_tc_pk:pk()) -> pk_share().
from_pk(PK) ->
    erlang_tc:share_from_pk(PK).

-spec verify_decryption_share(
    PKShare :: pk_share(),
    DecShare :: erlang_tc_dec_share:dec_share(),
    Cipher :: erlang_tc_ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(PKShare, DecShare, Cipher) ->
    erlang_tc:pk_share_verify_decryption_share(PKShare, DecShare, Cipher).

-spec verify(PKShare :: pk_share(),
             NodeSig :: erlang_tc_sig_share:sig_share(),
             Msg :: binary()) -> boolean().
verify(PKShare, NodeSig, Msg) ->
    erlang_tc:pk_share_verify(PKShare, NodeSig, Msg).
