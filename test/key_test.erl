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
    Signature = erlang_tc_sk:sign(SK, <<"hello">>),
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

    %% ?debugFmt("SKShare: ~p~n", [erlang_tc_sk_set:secret_key_share(SKSet, 1)]),
    %% ?debugFmt("PKSet: ~p~n", [PKSet]),

    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))),

    ?assertEqual(?DEGREE, erlang_tc_sk_set:threshold(SKSet)).
