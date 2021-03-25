-module(key_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        pk_size_test/1,
        signature_test/1,
        pk_set_test/1,
        pk_set_serde_test/1,
        pk_set_combine_test/1,
        pk_share_combine_test/1,
        sk_set_test/1,
        random_sk_set_test/1,
        verify_sig_test/1,
        verify_ciphertext_test/1,
        sk_share_combine_test/1,
        fr_serde_test/1,
        sk_share_serde_test/1
    ]
).

all() ->
    [
        pk_size_test,
        signature_test,
        pk_set_test,
        pk_set_serde_test,
        pk_set_combine_test,
        pk_share_combine_test,
        sk_set_test,
        random_sk_set_test,
        verify_sig_test,
        verify_ciphertext_test,
        sk_share_combine_test,
        fr_serde_test,
        sk_share_serde_test
    ].

init_per_testcase(_, Config) ->
    [{pk_size, 48}, {sig_size, 96}, {degree, 5} | Config].

end_per_testcase(_, Config) ->
    Config.

-include_lib("eunit/include/eunit.hrl").

pk_size_test(Config) ->
    PKSize = ?config(pk_size, Config),
    SK = secret_key:random(),
    PK = secret_key:public_key(SK),
    PKSize = byte_size(pubkey:to_bytes(PK)),
    ok.

signature_test(Config) ->
    SigSize = ?config(sig_size, Config),
    SK = secret_key:random(),
    Signature = secret_key:sign(SK, <<"resistance is futile">>),
    %% Parity = signature:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    SigSize = byte_size(signature:to_bytes(Signature)),
    ok.

pk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    Commitment = poly:commitment(RandomPoly),
    PKSet = public_key_set:from_commitment(Commitment),
    PK = public_key_set:public_key(PKSet),
    PKSize = byte_size(pubkey:to_bytes(PK)),
    Degree = public_key_set:threshold(PKSet),
    ok.

pk_set_serde_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    Commitment = poly:commitment(RandomPoly),
    PKSet = public_key_set:from_commitment(Commitment),

    SerPKSet = public_key_set:serialize(PKSet),
    DeserPKSet = public_key_set:deserialize(SerPKSet),

    PK = public_key_set:public_key(DeserPKSet),
    PKSize = byte_size(pubkey:to_bytes(PK)),
    Degree = public_key_set:threshold(DeserPKSet),

    ok.

pk_set_combine_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    Commitment = poly:commitment(RandomPoly),
    PKSet = public_key_set:from_commitment(Commitment),

    RandomPoly2 = poly:random(Degree),
    Commitment2 = poly:commitment(RandomPoly2),
    PKSet2 = public_key_set:from_commitment(Commitment2),

    PKSC = public_key_set:combine(PKSet, PKSet2),

    ct:pal("PKS1: ~p", [public_key_set:serialize(PKSet)]),
    ct:pal("PKS2: ~p", [public_key_set:serialize(PKSet2)]),
    ct:pal("PKSC: ~p", [public_key_set:serialize(PKSC)]),

    ok.

pk_share_combine_test(Config) ->
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    Commitment = poly:commitment(RandomPoly),
    PKSet = public_key_set:from_commitment(Commitment),

    PKS1 = public_key_set:public_key_share(PKSet, 1),
    PKS2 = public_key_set:public_key_share(PKSet, 2),
    PKSC = public_key_share:combine(PKS1, PKS2),

    ct:pal("PKS1: ~p", [public_key_share:reveal(PKS1)]),
    ct:pal("PKS2: ~p", [public_key_share:reveal(PKS2)]),
    ct:pal("PKSC: ~p", [public_key_share:reveal(PKSC)]),

    ok.

sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    SKSet = secret_key_set:from_poly(RandomPoly),
    PKSet = secret_key_set:public_keys(SKSet),
    PK = public_key_set:public_key(PKSet),
    PKSize = byte_size(pubkey:to_bytes(PK)),
    Degree = secret_key_set:threshold(SKSet),
    ok.

random_sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    SKSet = secret_key_set:random(Degree),
    PKSet = secret_key_set:public_keys(SKSet),
    PK = public_key_set:public_key(PKSet),
    PKSize = byte_size(pubkey:to_bytes(PK)),
    Degree = secret_key_set:threshold(SKSet),
    ok.

verify_sig_test(_Config) ->
    SK = secret_key:random(),
    Msg = <<"Say hello to my little friend">>,
    Sig = secret_key:sign(SK, Msg),
    PK = secret_key:public_key(SK),
    true = pubkey:verify(PK, Sig, Msg),
    ok.

sk_share_combine_test(_Config) ->
    Fr1 = fr:into(42),
    Fr2 = fr:into(666),

    SKS1 = secret_key_share:from_fr(Fr1),
    SKS2 = secret_key_share:from_fr(Fr2),

    SKSC = secret_key_share:combine(SKS1, SKS2),

    ct:pal("SKS1: ~p", [secret_key_share:reveal(SKS1)]),
    ct:pal("SKS2: ~p", [secret_key_share:reveal(SKS2)]),
    ct:pal("SKSC: ~p", [secret_key_share:reveal(SKSC)]),

    ok.

verify_ciphertext_test(_Config) ->
    SK = secret_key:random(),
    Msg = <<"His name is Robert Paulson">>,
    PK = secret_key:public_key(SK),

    Cipher = pubkey:encrypt(PK, Msg),
    true = ciphertext:verify(Cipher),
    ok.

fr_serde_test(_Config) ->
    true = fr:cmp(fr:deserialize(fr:serialize(fr:into(0))), fr:into(0)),
    true = fr:cmp(fr:deserialize(fr:serialize(fr:into(42))), fr:into(42)),
    true = fr:cmp(fr:deserialize(fr:serialize(fr:into(-8))), fr:into(-8)),
    true = fr:cmp(fr:deserialize(fr:serialize(fr:zero())), fr:zero()),
    ok.

sk_share_serde_test(_Config) ->
    Fr1 = fr:into(42),

    SKS = secret_key_share:from_fr(Fr1),

    SerSKS = secret_key_share:serialize(SKS),
    DeserSKS = secret_key_share:deserialize(SerSKS),

    ?assert(secret_key_share:cmp(SKS, DeserSKS)),

    ok.
