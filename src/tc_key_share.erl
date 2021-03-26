-module(tc_key_share).

-export([
    new/3,
    encrypt/2,
    decrypt_share/2,
    verify_decryption_share/3,
    combine_decryption_shares/3,
    verify/3,
    sign_share/2,
    verify_signature_share/3,
    combine_signature_shares/2,
    serialize/1,
    deserialize/1
]).

-record(tc_key_share, {
    public_key_set :: public_key_set:pk_set(),
    secret_key_share :: secret_key_share:sk_share(),
    %% indexed from 0!
    index :: non_neg_integer()
}).

-type tc_key_share() :: #tc_key_share{}.
-export_type([tc_key_share/0]).

-spec new(non_neg_integer(), public_key_set:pk_set(), secret_key_share:sk_share()) ->
    tc_key_share().
new(Id, PublicKeySet, SecretKeyShare) ->
    #tc_key_share{index = Id, public_key_set = PublicKeySet, secret_key_share = SecretKeyShare}.

-spec encrypt(#tc_key_share{}, binary()) -> ciphertext:ciphertext().
encrypt(#tc_key_share{public_key_set = PublicKeySet}, PlainText) ->
    pubkey:encrypt(public_key_set:public_key(PublicKeySet), PlainText).

-spec decrypt_share(tc_key_share(), ciphertext:ciphertext()) ->
    {non_neg_integer(), ciphertext:ciphertext()}.
decrypt_share(#tc_key_share{secret_key_share = SK, index = Id}, Ciphertext) ->
    {Id, secret_key_share:decrypt_share(SK, Ciphertext)}.

-spec verify_decryption_share(
    tc_key_share(),
    {non_neg_integer(), decryption_share:dec_share()},
    ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(#tc_key_share{public_key_set = PK}, {Id, DecShare}, Ciphertext) ->
    public_key_share:verify_decryption_share(
        public_key_set:public_key_share(PK, Id),
        DecShare,
        Ciphertext
    ).

-spec combine_decryption_shares(
    tc_key_share(),
    [{non_neg_integer(), decryption_share:dec_share()}],
    ciphertext:ciphertext()
) -> {ok, binary()} | {error, cannot_decrypt}.
combine_decryption_shares(#tc_key_share{public_key_set = PK}, DecShares, Ciphertext) ->
    public_key_set:decrypt(PK, DecShares, Ciphertext).

-spec verify(tc_key_share(), signature:sig(), binary()) -> boolean().
verify(#tc_key_share{public_key_set = PK}, Signature, Msg) ->
    pubkey:verify(public_key_set:public_key(PK), Signature, Msg).

-spec sign_share(tc_key_share(), binary()) -> {non_neg_integer(), signature_share:sig_share()}.
sign_share(#tc_key_share{secret_key_share = SK, index = Id}, Msg) ->
    {Id, secret_key_share:sign(SK, Msg)}.

-spec verify_signature_share(
    tc_key_share(),
    {non_neg_integer(), signature_share:sig_share()},
    binary()
) -> boolean().
verify_signature_share(#tc_key_share{public_key_set = PK}, {Id, SigShare}, Msg) ->
    public_key_share:verify_signature_share(public_key_set:public_key_share(PK, Id), SigShare, Msg).

-spec combine_signature_shares(tc_key_share(), [{non_neg_integer(), signature_share:sig_share()}]) ->
    {ok, signature:sig()} | {error, cannot_combine}.
combine_signature_shares(#tc_key_share{public_key_set = PK}, SigShares) ->
    public_key_set:combine_signatures(PK, SigShares).

-spec serialize(tc_key_share()) -> binary().
serialize(#tc_key_share{public_key_set = PK, secret_key_share = SK, index = Id}) ->
    SerPK = public_key_set:serialize(PK),
    SerSK = secret_key_share:serialize(SK),
    PKLen = byte_size(SerPK),
    <<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK/binary, SerSK/binary>>.

-spec deserialize(binary()) -> tc_key_share().
deserialize(<<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK:PKLen/binary, SerSK/binary>>) ->
    #tc_key_share{
        index = Id,
        public_key_set = public_key_set:deserialize(SerPK),
        secret_key_share = secret_key_share:deserialize(SerSK)
    }.
