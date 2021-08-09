-module(tc_secret_key).

-export([
    %% SecretKey API
    random/0,
    from_fr/1,
    public_key/1,
    reveal/1,
    sign/2,
    decrypt/2,
    serialize/1,
    deserialize/1
]).

-type sk() :: reference().
-export_type([sk/0]).

-spec random() -> sk().
random() ->
    erlang_tc:sk_random().

-spec from_fr(Fr :: tc_fr:fr()) -> sk().
from_fr(Fr) ->
    erlang_tc:sk_from_fr(Fr).

-spec public_key(SK :: sk()) -> tc_pubkey:pk().
public_key(SK) ->
    erlang_tc:sk_public_key(SK).

-spec reveal(SK :: sk()) -> string().
reveal(SK) ->
    erlang_tc:sk_reveal(SK).

-spec sign(SK :: sk(), Msg :: binary()) -> tc_signature:sig().
sign(SK, Msg) ->
    erlang_tc:sk_sign(SK, Msg).

-spec decrypt(SK :: sk(), Cipher :: tc_ciphertext:ciphertext()) -> binary().
decrypt(SK, Cipher) ->
    erlang_tc:sk_decrypt(SK, Cipher).

-spec serialize(SK :: sk()) -> binary().
serialize(SK) ->
    erlang_tc:sk_serialize(SK).

-spec deserialize(Bin :: binary()) -> sk().
deserialize(Bin) ->
    erlang_tc:sk_deserialize(Bin).

