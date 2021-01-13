-module(key_test).

-include_lib("eunit/include/eunit.hrl").

pk_size_test() ->
    SK = erlang_tc_sk:random(),
    PK = erlang_tc_sk:public_key(SK),
    ?assertEqual(48, byte_size(erlang_tc_pk:to_bytes(PK))).

signature_test() ->
    SK = erlang_tc_sk:random(),
    Signature = erlang_tc_sk:sign(SK, <<"hello">>),
    %% Parity = erlang_tc_sig:parity(Signature),
    %% ?debugFmt("Parity: ~p~n", [Parity]),
    ?assertEqual(96, byte_size(erlang_tc_sig:to_bytes(Signature))).

pk_set_test() ->
    RandomPoly = erlang_tc_poly:random(5),
    Commitment = erlang_tc_poly:commitment(RandomPoly),
    PKSet = erlang_tc_pk_set:from_commitment(Commitment),
    PK = erlang_tc_pk_set:public_key(PKSet),
    ?assertEqual(48, byte_size(erlang_tc_pk:to_bytes(PK))).
