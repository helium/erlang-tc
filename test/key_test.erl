-module(key_test).

-include_lib("eunit/include/eunit.hrl").

-define(PK_SIZE, 48).
-define(SIG_SIZE, 96).
-define(DEGREE, 5).

pk_size_test() ->
    SK = erlang_tc_sk:random(),
    PK = erlang_tc_sk:public_key(SK),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))).

signature_test() ->
    SK = erlang_tc_sk:random(),
    Signature = erlang_tc_sk:sign(SK, <<"resistance is futile">>),
    %% Parity = erlang_tc_sig:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    ?assertEqual(?SIG_SIZE, byte_size(erlang_tc_sig:to_bytes(Signature))).

pk_set_test() ->
    RandomPoly = erlang_tc_poly:random(?DEGREE),
    Commitment = erlang_tc_poly:commitment(RandomPoly),
    PKSet = erlang_tc_pk_set:from_commitment(Commitment),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))),
    ?assertEqual(?DEGREE, erlang_tc_pk_set:threshold(PKSet)).

sk_set_test() ->
    RandomPoly = erlang_tc_poly:random(?DEGREE),
    SKSet = erlang_tc_sk_set:from_poly(RandomPoly),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))),

    ?assertEqual(?DEGREE, erlang_tc_sk_set:threshold(SKSet)).

random_sk_set_test() ->
    SKSet = erlang_tc_sk_set:random(?DEGREE),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))),

    ?assertEqual(?DEGREE, erlang_tc_sk_set:threshold(SKSet)).

verify_sig_test() ->
    SK = erlang_tc_sk:random(),
    Msg = <<"say hello to my little friend">>,
    Sig = erlang_tc_sk:sign(SK, Msg),
    PK = erlang_tc_sk:public_key(SK),
    ?assert(erlang_tc_pk:verify(PK, Sig, Msg)).

verify_ciphertext_test() ->
    SK = erlang_tc_sk:random(),
    Msg = <<"his name is Robert Paulson">>,
    PK = erlang_tc_sk:public_key(SK),

    Cipher = erlang_tc_pk:encrypt(PK, Msg),
    ?assert(erlang_tc_ciphertext:verify(Cipher)).
