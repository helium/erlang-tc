-module(key_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        pk_size_test/1,
        signature_test/1,
        pk_set_test/1,
        sk_set_test/1,
        random_sk_set_test/1,
        verify_sig_test/1,
        verify_ciphertext_test/1
    ]
).

all() ->
    [
        pk_size_test,
        signature_test,
        pk_set_test,
        sk_set_test,
        random_sk_set_test,
        verify_sig_test,
        verify_ciphertext_test
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
    ?assertEqual(PKSize, byte_size(public_key:to_bytes(PK))).

signature_test(Config) ->
    SigSize = ?config(sig_size, Config),
    SK = secret_key:random(),
    Signature = secret_key:sign(SK, <<"resistance is futile">>),
    %% Parity = signature:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    ?assertEqual(SigSize, byte_size(signature:to_bytes(Signature))).

pk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    Commitment = poly:commitment(RandomPoly),
    PKSet = public_key_set:from_commitment(Commitment),
    PK = public_key_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(public_key:to_bytes(PK))),
    ?assertEqual(Degree, public_key_set:threshold(PKSet)).

sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = poly:random(Degree),
    SKSet = secret_key_set:from_poly(RandomPoly),
    PKSet = secret_key_set:public_keys(SKSet),
    PK = public_key_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(public_key:to_bytes(PK))),
    ?assertEqual(Degree, secret_key_set:threshold(SKSet)).

random_sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    SKSet = secret_key_set:random(Degree),
    PKSet = secret_key_set:public_keys(SKSet),
    PK = public_key_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(public_key:to_bytes(PK))),
    ?assertEqual(Degree, secret_key_set:threshold(SKSet)).

verify_sig_test(_Config) ->
    SK = secret_key:random(),
    Msg = <<"Say hello to my little friend">>,
    Sig = secret_key:sign(SK, Msg),
    PK = secret_key:public_key(SK),
    ?assert(public_key:verify(PK, Sig, Msg)).

verify_ciphertext_test(_Config) ->
    SK = secret_key:random(),
    Msg = <<"His name is Robert Paulson">>,
    PK = secret_key:public_key(SK),

    Cipher = public_key:encrypt(PK, Msg),
    ?assert(ciphertext:verify(Cipher)).
