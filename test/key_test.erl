-module(key_test).

-include_lib("eunit/include/eunit.hrl").

-define(PK_SIZE, 48).
-define(SIG_SIZE, 96).

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
    Degree = 5,
    RandomPoly = erlang_tc_poly:random(Degree),
    Commitment = erlang_tc_poly:commitment(RandomPoly),
    PKSet = erlang_tc_pk_set:from_commitment(Commitment),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(?PK_SIZE, byte_size(erlang_tc_pk:to_bytes(PK))),
    ?assertEqual(Degree, erlang_tc_pk_set:threshold(PKSet)).
