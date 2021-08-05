-module(key_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        pk_size_test/1,
        pk_serde_test/1,
        signature_test/1,
        agg_signature_test/1,
        big_agg_signature_test/1,
        big_agg_same_msg_signature_test/1,
        pk_set_test/1,
        pk_set_serde_test/1,
        pk_set_combine_test/1,
        pk_share_combine_test/1,
        pk_share_serde_test/1,
        sk_set_test/1,
        random_sk_set_test/1,
        verify_sig_test/1,
        verify_ciphertext_test/1,
        ciphertext_serde_test/1,
        sk_share_combine_test/1,
        fr_serde_test/1,
        sk_share_serde_test/1,
        signature_serde_test/1
    ]
).

all() ->
    [
        pk_size_test,
        pk_serde_test,
        signature_test,
        agg_signature_test,
        big_agg_signature_test,
        big_agg_same_msg_signature_test,
        pk_set_test,
        pk_set_serde_test,
        pk_set_combine_test,
        pk_share_combine_test,
        pk_share_serde_test,
        sk_set_test,
        random_sk_set_test,
        verify_sig_test,
        verify_ciphertext_test,
        ciphertext_serde_test,
        sk_share_combine_test,
        fr_serde_test,
        sk_share_serde_test,
        signature_serde_test
    ].

init_per_testcase(_, Config) ->
    [{pk_size, 48}, {sig_size, 96}, {degree, 5} | Config].

end_per_testcase(_, Config) ->
    Config.

-include_lib("eunit/include/eunit.hrl").

pk_size_test(Config) ->
    PKSize = ?config(pk_size, Config),
    SK = tc_secret_key:random(),
    PK = tc_secret_key:public_key(SK),
    PKSize = byte_size(tc_pubkey:serialize(PK)),
    ok.

pk_serde_test(_Config) ->
    SK = tc_secret_key:random(),
    PK = tc_secret_key:public_key(SK),

    SPK = tc_pubkey:serialize(PK),
    DPK = tc_pubkey:deserialize(SPK),

    ?assert(tc_pubkey:cmp(PK, DPK)),
    ok.

signature_test(Config) ->
    SigSize = ?config(sig_size, Config),
    SK = tc_secret_key:random(),
    Signature = tc_secret_key:sign(SK, <<"resistance is futile">>),
    %% Parity = tc_signature:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    SigSize = byte_size(tc_signature:serialize(Signature)),
    ok.

agg_signature_test(_Config) ->
    SK1 = tc_secret_key:random(),
    Msg1 = <<"rip and tear">>,
    Sig1 = tc_secret_key:sign(SK1, Msg1),
    SK2 = tc_secret_key:random(),
    Msg2 = <<"until it's done">>,
    Sig2 = tc_secret_key:sign(SK2, Msg2),

    Sig = tc_signature:aggregate_from_sigs([Sig1, Sig2]),
    ?assert(tc_signature:core_aggregate_verify(Sig,
                                               [{tc_secret_key:public_key(SK1), Msg1},
                                                {tc_secret_key:public_key(SK2), Msg2}
                                               ])),
    ok.

big_agg_signature_test(_Config) ->
    Total = 43,

    SecretKeys = [tc_secret_key:random() || _ <- lists:seq(1, Total)],
    PublicKeys = [tc_secret_key:public_key(SK) || SK <- SecretKeys],
    Msgs = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, Total)],
    Sigs = [tc_secret_key:sign(SK, Msg) || {SK, Msg} <- lists:zip(SecretKeys, Msgs)],

    Sig = tc_signature:aggregate_from_sigs(Sigs),
    ?assert(tc_signature:core_aggregate_verify(Sig, lists:zip(PublicKeys, Msgs))),
    ok.

big_agg_same_msg_signature_test(_Config) ->
    Total = 43,

    SecretKeys = [tc_secret_key:random() || _ <- lists:seq(1, Total)],
    Msg = <<"Rip and tear until it's done">>,

    PubkeysAndMsgs = [{tc_secret_key:public_key(SK), Msg} || SK <- SecretKeys],

    Sigs = [tc_secret_key:sign(SK, Msg) || SK <- SecretKeys],

    Sig = tc_signature:aggregate_from_sigs(Sigs),
    ?assert(tc_signature:core_aggregate_verify(Sig, PubkeysAndMsgs)),
    ok.

pk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    Commitment = tc_poly:commitment(RandomPoly),
    PKSet = tc_public_key_set:from_commitment(Commitment),
    PK = tc_public_key_set:public_key(PKSet),
    PKSize = byte_size(tc_pubkey:serialize(PK)),
    Degree = tc_public_key_set:threshold(PKSet),
    ok.

pk_set_serde_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    Commitment = tc_poly:commitment(RandomPoly),
    PKSet = tc_public_key_set:from_commitment(Commitment),

    SerPKSet = tc_public_key_set:serialize(PKSet),
    DeserPKSet = tc_public_key_set:deserialize(SerPKSet),

    ?assert(tc_public_key_set:cmp(PKSet, DeserPKSet)),

    ok.

pk_set_combine_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    Commitment = tc_poly:commitment(RandomPoly),
    PKSet = tc_public_key_set:from_commitment(Commitment),

    RandomPoly2 = tc_poly:random(Degree),
    Commitment2 = tc_poly:commitment(RandomPoly2),
    PKSet2 = tc_public_key_set:from_commitment(Commitment2),

    PKSC = tc_public_key_set:combine(PKSet, PKSet2),

    ct:pal("PKS1: ~p", [tc_public_key_set:serialize(PKSet)]),
    ct:pal("PKS2: ~p", [tc_public_key_set:serialize(PKSet2)]),
    ct:pal("PKSC: ~p", [tc_public_key_set:serialize(PKSC)]),

    ok.

pk_share_combine_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    Commitment = tc_poly:commitment(RandomPoly),
    PKSet = tc_public_key_set:from_commitment(Commitment),

    PKS1 = tc_public_key_set:public_key_share(PKSet, 1),
    PKS2 = tc_public_key_set:public_key_share(PKSet, 2),
    PKSC = tc_public_key_share:combine(PKS1, PKS2),

    ct:pal("PKS1: ~p", [tc_public_key_share:reveal(PKS1)]),
    ct:pal("PKS2: ~p", [tc_public_key_share:reveal(PKS2)]),
    ct:pal("PKSC: ~p", [tc_public_key_share:reveal(PKSC)]),

    ok.

pk_share_serde_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    Commitment = tc_poly:commitment(RandomPoly),
    PKSet = tc_public_key_set:from_commitment(Commitment),
    PKShare = tc_public_key_set:public_key_share(PKSet, 1),

    SerPKShare = tc_public_key_share:serialize(PKShare),
    DeserPKShare = tc_public_key_share:deserialize(SerPKShare),

    ?assert(tc_public_key_share:cmp(PKShare, DeserPKShare)),

    ok.
sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = tc_poly:random(Degree),
    SKSet = tc_secret_key_set:from_poly(RandomPoly),
    PKSet = tc_secret_key_set:public_keys(SKSet),
    PK = tc_public_key_set:public_key(PKSet),
    PKSize = byte_size(tc_pubkey:serialize(PK)),
    Degree = tc_secret_key_set:threshold(SKSet),
    ok.

random_sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    SKSet = tc_secret_key_set:random(Degree),
    PKSet = tc_secret_key_set:public_keys(SKSet),
    PK = tc_public_key_set:public_key(PKSet),
    PKSize = byte_size(tc_pubkey:serialize(PK)),
    Degree = tc_secret_key_set:threshold(SKSet),
    ok.

verify_sig_test(_Config) ->
    SK = tc_secret_key:random(),
    Msg = <<"Say hello to my little friend">>,
    Sig = tc_secret_key:sign(SK, Msg),
    PK = tc_secret_key:public_key(SK),
    true = tc_pubkey:verify(PK, Sig, Msg),
    ok.

sk_share_combine_test(_Config) ->
    Fr1 = tc_fr:into(42),
    Fr2 = tc_fr:into(666),

    SKS1 = tc_secret_key_share:from_fr(Fr1),
    SKS2 = tc_secret_key_share:from_fr(Fr2),

    SKSC = tc_secret_key_share:combine(SKS1, SKS2),

    ct:pal("SKS1: ~p", [tc_secret_key_share:reveal(SKS1)]),
    ct:pal("SKS2: ~p", [tc_secret_key_share:reveal(SKS2)]),
    ct:pal("SKSC: ~p", [tc_secret_key_share:reveal(SKSC)]),

    ok.

verify_ciphertext_test(_Config) ->
    SK = tc_secret_key:random(),
    Msg = <<"His name is Robert Paulson">>,
    PK = tc_secret_key:public_key(SK),

    Cipher = tc_pubkey:encrypt(PK, Msg),
    true = tc_ciphertext:verify(Cipher),
    ok.

ciphertext_serde_test(_Config) ->
    SK = tc_secret_key:random(),
    Msg = <<"His name is Robert Paulson">>,
    PK = tc_secret_key:public_key(SK),

    Cipher = tc_pubkey:encrypt(PK, Msg),
    true = tc_ciphertext:verify(Cipher),

    SerCipher = tc_ciphertext:serialize(Cipher),
    DeserCipher = tc_ciphertext:deserialize(SerCipher),

    true = tc_ciphertext:verify(DeserCipher),

    ?assert(tc_ciphertext:cmp(Cipher, DeserCipher)),

    ok.

fr_serde_test(_Config) ->
    true = tc_fr:cmp(tc_fr:deserialize(tc_fr:serialize(tc_fr:into(0))), tc_fr:into(0)),
    true = tc_fr:cmp(tc_fr:deserialize(tc_fr:serialize(tc_fr:into(42))), tc_fr:into(42)),
    true = tc_fr:cmp(tc_fr:deserialize(tc_fr:serialize(tc_fr:into(-8))), tc_fr:into(-8)),
    true = tc_fr:cmp(tc_fr:deserialize(tc_fr:serialize(tc_fr:zero())), tc_fr:zero()),
    ok.

sk_share_serde_test(_Config) ->
    Fr1 = tc_fr:into(42),

    SKS = tc_secret_key_share:from_fr(Fr1),

    SerSKS = tc_secret_key_share:serialize(SKS),
    DeserSKS = tc_secret_key_share:deserialize(SerSKS),

    ?assert(tc_secret_key_share:cmp(SKS, DeserSKS)),

    ok.

signature_serde_test(_Config) ->
    SK = tc_secret_key:random(),
    Signature = tc_secret_key:sign(SK, <<"resistance is futile">>),

    SerSig = tc_signature:serialize(Signature),
    DeserSig = tc_signature:deserialize(SerSig),

    ?assert(tc_signature:cmp(Signature, DeserSig)),

    ok.
