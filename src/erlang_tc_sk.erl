-module(erlang_tc_sk).

-export([
    %% SecretKey API
    random/0,
    from_fr/1,
    public_key/1,
    reveal/1,
    sign/2,
    decrypt/2
]).

-type sk() :: reference().
-export_type([sk/0]).

-spec random() -> sk().
random() ->
    erlang_tc:sk_random().

-spec from_fr(Fr :: erlang_tc_fr:fr()) -> sk().
from_fr(Fr) ->
    erlang_tc:sk_from_fr(Fr).

-spec public_key(SK :: sk()) -> erlang_tc_pk:pk().
public_key(SK) ->
    erlang_tc:sk_public_key(SK).

-spec reveal(SK :: sk()) -> string().
reveal(SK) ->
    erlang_tc:sk_reveal(SK).

-spec sign(SK :: sk(), Msg :: binary()) -> erlang_tc_sig:sig().
sign(SK, Msg) ->
    erlang_tc:sk_sign(SK, Msg).

-spec decrypt(SK :: sk(), Cipher :: erlang_tc_ciphertext:ciphertext()) -> binary().
decrypt(SK, Cipher) ->
    erlang_tc:sk_decrypt(SK, Cipher).
