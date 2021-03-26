-module(tc_key_share).

-export([new/3, encrypt/2, decrypt_share/2, combine_decryption_shares/3, verify/3, sign_share/2, combine_signature_shares/2, serialize/1, deserialize/1]).

-record(tc_key_share, {
          public_key_set :: public_key_set:pk_set(),
          secret_key_share :: secret_key_share:sk_share(),
          index :: non_neg_integer() %% indexed from 0!
         }).

-spec new(non_neg_integer(), public_key_set:pk_set(), secret_key_share:sk_share()) -> #tc_key_share{}.
new(Id, PublicKeySet, SecretKeyShare) ->
    #tc_key_share{index=Id, public_key_set=PublicKeySet, secret_key_share=SecretKeyShare}.

-spec encrypt(#tc_key_share{}, binary()) -> ciphertext:ciphertext().
encrypt(#tc_key_share{public_key_set=PublicKeySet}, PlainText) ->
    pubkey:encrypt(public_key_set:public_key(PublicKeySet), PlainText).

decrypt_share(#tc_key_share{secret_key_share=SK, index=Id}, Ciphertext) ->
    {Id, secret_key_share:decrypt_share(SK, Ciphertext)}.

combine_decryption_shares(#tc_key_share{public_key_set=PK}, DecShares, Ciphertext) ->
    public_key_set:decrypt(PK, DecShares, Ciphertext).

verify(#tc_key_share{public_key_set=PK}, Signature, Msg) ->
    pubkey:verify(public_key_set:public_key(PK), Signature, Msg).

sign_share(#tc_key_share{secret_key_share=SK, index=Id}, Msg) ->
    {Id, secret_key_share:sign(SK, Msg)}.

combine_signature_shares(#tc_key_share{public_key_set=PK}, SigShares) ->
    public_key_set:combine_signatures(PK, SigShares).

serialize(#tc_key_share{public_key_set=PK, secret_key_share=SK, index=Id}) ->
    SerPK = public_key_set:serialize(PK),
    SerSK = secret_key_share:serialize(SK),
    PKLen = byte_size(SerPK),
    <<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK/binary, SerSK/binary>>.

deserialize(<<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK:PKLen/binary, SerSK/binary>>) ->
    #tc_key_share{index=Id, public_key_set=public_key_set:deserialize(SerPK), secret_key_share=secret_key_share:deserialize(SerSK)}.
