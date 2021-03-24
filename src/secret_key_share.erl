-module(secret_key_share).

-export([
    decrypt_share/2,
    sign/2,
    from_fr/1,
    public_key_share/1
]).

-type sk_share() :: reference().

-export_type([sk_share/0]).

-spec from_fr(Fr :: fr:fr()) -> sk_share().
from_fr(Fr) ->
    erlang_tc:sk_share_from_fr(Fr).

-spec decrypt_share(
    SKShare :: sk_share(),
    Ciphertext :: ciphertext:ciphertext()
) -> decryption_share:dec_share().
decrypt_share(SKShare, Ciphertext) ->
    erlang_tc:sk_share_decryption_share(SKShare, Ciphertext).

-spec sign(
    SKShare :: sk_share(),
    Msg :: binary()
) -> signature_share:sig_share().
sign(SKShare, Msg) ->
    erlang_tc:sk_share_sign(SKShare, Msg).

-spec public_key_share(SKShare :: sk_share()) -> public_key_share:pk_share().
public_key_share(SKShare) ->
    erlang_tc:sk_share_public_key_share(SKShare).
