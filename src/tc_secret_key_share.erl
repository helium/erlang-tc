-module(tc_secret_key_share).

-export([
    decrypt_share/2,
    sign/2,
    from_fr/1,
    public_key_share/1,
    reveal/1,
    combine/2,
    serialize/1,
    deserialize/1,
    cmp/2
]).

-type sk_share() :: reference().

-export_type([sk_share/0]).

-spec from_fr(Fr :: tc_fr:fr()) -> sk_share().
from_fr(Fr) ->
    erlang_tc:sk_share_from_fr(Fr).

-spec decrypt_share(
    SKShare :: sk_share(),
    Ciphertext :: tc_ciphertext:ciphertext()
) -> tc_decryption_share:dec_share().
decrypt_share(SKShare, Ciphertext) ->
    erlang_tc:sk_share_decryption_share(SKShare, Ciphertext).

-spec sign(
    SKShare :: sk_share(),
    Msg :: binary()
) -> tc_signature_share:sig_share().
sign(SKShare, Msg) ->
    erlang_tc:sk_share_sign(SKShare, Msg).

-spec public_key_share(SKShare :: sk_share()) -> tc_public_key_share:pk_share().
public_key_share(SKShare) ->
    erlang_tc:sk_share_public_key_share(SKShare).

-spec reveal(SKShare :: sk_share()) -> string().
reveal(SKShare) ->
    erlang_tc:sk_share_reveal(SKShare).

-spec combine(SKS1 :: sk_share(), SKS2 :: sk_share()) -> sk_share().
combine(SKS1, SKS2) ->
    erlang_tc:sk_share_combine(SKS1, SKS2).

-spec serialize(SKShare :: sk_share()) -> binary().
serialize(SKShare) ->
    erlang_tc:sk_share_serialize(SKShare).

-spec deserialize(BinSKS :: binary()) -> sk_share().
deserialize(BinSKS) ->
    erlang_tc:sk_share_deserialize(BinSKS).

-spec cmp(SKS1 :: sk_share(), SKS2 :: sk_share()) -> boolean().
cmp(SKS1, SKS2) ->
    erlang_tc:sk_share_cmp(SKS1, SKS2).
