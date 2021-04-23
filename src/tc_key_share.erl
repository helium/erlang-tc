-module(tc_key_share).

-export([
    new/3,
    is_key_share/1,
    encrypt/2,
    decrypt_share/2,
    verify_decryption_share/3,
    combine_decryption_shares/3,
    verify/3,
    sign_share/2,
    verify_signature_share/3,
    combine_signature_shares/2,
    serialize/1,
    deserialize/1,
    deal/2
]).

-export([
    public_key/1,
    public_key_set/1,
    secret_key_share/1
]).

-record(tc_key_share, {
    public_key_set :: tc_public_key_set:pk_set(),
    secret_key_share :: tc_secret_key_share:sk_share(),
    %% indexed from 0!
    index :: non_neg_integer()
}).

-type tc_key_share() :: #tc_key_share{}.
-export_type([tc_key_share/0]).

-spec public_key(tc_key_share()) -> tc_pubkey:pk().
public_key(#tc_key_share{public_key_set = PKSet}) ->
    tc_public_key_set:public_key(PKSet).

-spec public_key_set(tc_key_share()) -> tc_public_key_set:pk_set().
public_key_set(#tc_key_share{public_key_set = PKSet}) ->
    PKSet.

-spec secret_key_share(tc_key_share()) -> tc_secret_key_share:sk_share().
secret_key_share(#tc_key_share{secret_key_share = SKShare}) ->
    SKShare.

-spec new(non_neg_integer(), tc_public_key_set:pk_set(), tc_secret_key_share:sk_share()) ->
    tc_key_share().
new(Id, PublicKeySet, SecretKeyShare) ->
    #tc_key_share{index = Id, public_key_set = PublicKeySet, secret_key_share = SecretKeyShare}.

-spec is_key_share(any()) -> boolean().
is_key_share(#tc_key_share{}) ->
    true;
is_key_share(_) ->
    false.

-spec encrypt(#tc_key_share{}, binary()) -> tc_ciphertext:ciphertext().
encrypt(#tc_key_share{public_key_set = PublicKeySet}, PlainText) ->
    tc_pubkey:encrypt(tc_public_key_set:public_key(PublicKeySet), PlainText).

-spec decrypt_share(tc_key_share(), tc_ciphertext:ciphertext()) ->
    {non_neg_integer(), tc_decryption_share:dec_share()}.
decrypt_share(#tc_key_share{secret_key_share = SK, index = Id}, Ciphertext) ->
    {Id, tc_secret_key_share:decrypt_share(SK, Ciphertext)}.

-spec verify_decryption_share(
    tc_key_share(),
    {non_neg_integer(), tc_decryption_share:dec_share()},
    tc_ciphertext:ciphertext()
) -> boolean().
verify_decryption_share(#tc_key_share{public_key_set = PK}, {Id, DecShare}, Ciphertext) ->
    tc_public_key_share:verify_decryption_share(
        tc_public_key_set:public_key_share(PK, Id),
        DecShare,
        Ciphertext
    ).

-spec combine_decryption_shares(
    tc_key_share(),
    [{non_neg_integer(), tc_decryption_share:dec_share()}],
    tc_ciphertext:ciphertext()
) -> {ok, binary()} | {error, cannot_decrypt}.
combine_decryption_shares(#tc_key_share{public_key_set = PK}, DecShares, Ciphertext) ->
    tc_public_key_set:decrypt(PK, DecShares, Ciphertext).

-spec verify(tc_key_share(), tc_signature:sig(), binary()) -> boolean().
verify(#tc_key_share{public_key_set = PK}, Signature, Msg) ->
    tc_pubkey:verify(tc_public_key_set:public_key(PK), Signature, Msg).

-spec sign_share(tc_key_share(), binary()) -> {non_neg_integer(), tc_signature_share:sig_share()}.
sign_share(#tc_key_share{secret_key_share = SK, index = Id}, Msg) ->
    {Id, tc_secret_key_share:sign(SK, Msg)}.

-spec verify_signature_share(
    tc_key_share(),
    {non_neg_integer(), tc_signature_share:sig_share()},
    binary()
) -> boolean().
verify_signature_share(#tc_key_share{public_key_set = PK}, {Id, SigShare}, Msg) ->
    tc_public_key_share:verify_signature_share(tc_public_key_set:public_key_share(PK, Id), SigShare, Msg).

-spec combine_signature_shares(tc_key_share(), [{non_neg_integer(), tc_signature_share:sig_share()}]) ->
    {ok, tc_signature:sig()} | {error, cannot_combine}.
combine_signature_shares(#tc_key_share{public_key_set = PK}, SigShares) ->
    tc_public_key_set:combine_signatures(PK, SigShares).

-spec serialize(tc_key_share()) -> binary().
serialize(#tc_key_share{public_key_set = PK, secret_key_share = SK, index = Id}) ->
    SerPK = tc_public_key_set:serialize(PK),
    SerSK = tc_secret_key_share:serialize(SK),
    PKLen = byte_size(SerPK),
    <<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK/binary, SerSK/binary>>.

-spec deserialize(binary()) -> tc_key_share().
deserialize(<<Id:8/integer, PKLen:32/integer-unsigned-little, SerPK:PKLen/binary, SerSK/binary>>) ->
    #tc_key_share{
        index = Id,
        public_key_set = tc_public_key_set:deserialize(SerPK),
        secret_key_share = tc_secret_key_share:deserialize(SerSK)
    }.

-spec deal(N :: non_neg_integer(), Threshold :: non_neg_integer()) -> [tc_key_share()].
deal(N, Threshold) ->
    SKSet = tc_secret_key_set:random(Threshold),
    lists:reverse(
        lists:foldl(
            fun(Id, Acc) ->
                [
                    new(
                        Id,
                        tc_secret_key_set:public_keys(SKSet),
                        tc_secret_key_set:secret_key_share(SKSet, Id)
                    )
                    | Acc
                ]
            end,
            [],
            lists:seq(0, N - 1)
        )
    ).
