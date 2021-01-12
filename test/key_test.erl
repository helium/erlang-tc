-module(key_test).

-include_lib("eunit/include/eunit.hrl").

pk_size_test() ->
    SK = erlang_tc:sk_random(),
    PK = erlang_tc:sk_public_key(SK),
    ?assertEqual(48, byte_size(erlang_tc:pk_to_bytes(PK))).
