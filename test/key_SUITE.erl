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
    SK = erlang_tc_sk:random(),
    PK = erlang_tc_sk:public_key(SK),
    ?assertEqual(PKSize, byte_size(erlang_tc_pk:to_bytes(PK))).

signature_test(Config) ->
    SigSize = ?config(sig_size, Config),
    SK = erlang_tc_sk:random(),
    Signature = erlang_tc_sk:sign(SK, <<"resistance is futile">>),
    %% Parity = erlang_tc_sig:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    ?assertEqual(SigSize, byte_size(erlang_tc_sig:to_bytes(Signature))).

pk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = erlang_tc_poly:random(Degree),
    Commitment = erlang_tc_poly:commitment(RandomPoly),
    PKSet = erlang_tc_pk_set:from_commitment(Commitment),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(erlang_tc_pk:to_bytes(PK))),
    ?assertEqual(Degree, erlang_tc_pk_set:threshold(PKSet)).

sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    RandomPoly = erlang_tc_poly:random(Degree),
    SKSet = erlang_tc_sk_set:from_poly(RandomPoly),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(erlang_tc_pk:to_bytes(PK))),
    ?assertEqual(Degree, erlang_tc_sk_set:threshold(SKSet)).

random_sk_set_test(Config) ->
    PKSize = ?config(pk_size, Config),
    Degree = ?config(degree, Config),
    SKSet = erlang_tc_sk_set:random(Degree),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(PKSize, byte_size(erlang_tc_pk:to_bytes(PK))),
    ?assertEqual(Degree, erlang_tc_sk_set:threshold(SKSet)).

verify_sig_test(_Config) ->
    SK = erlang_tc_sk:random(),
    Msg = <<"Say hello to my little friend">>,
    Sig = erlang_tc_sk:sign(SK, Msg),
    PK = erlang_tc_sk:public_key(SK),
    ?assert(erlang_tc_pk:verify(PK, Sig, Msg)).

verify_ciphertext_test(_Config) ->
    SK = erlang_tc_sk:random(),
    Msg = <<"His name is Robert Paulson">>,
    PK = erlang_tc_sk:public_key(SK),

    Cipher = erlang_tc_pk:encrypt(PK, Msg),
    ?assert(erlang_tc_ciphertext:verify(Cipher)).
