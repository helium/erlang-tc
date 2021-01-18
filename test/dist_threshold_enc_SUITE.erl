-module(dist_threshold_enc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-export([
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([
         threshold_one_test/1,
         threshold_ten_test/1,
         threshold_twenty_test/1,
         threshold_thirty_test/1,
         threshold_fifty_test/1,
         threshold_one_hundred_test/1
        ]).

%% common test callbacks

all() -> [
         threshold_one_test,
         threshold_ten_test,
         threshold_twenty_test,
         threshold_thirty_test,
         threshold_fifty_test
         %% threshold_one_hundred_test
         ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    %% The criteria for completion is that there must be threshold + 1 sigs
    {NumActors, Threshold} = case TestCase of
                                threshold_one_test -> {3, 1};
                                threshold_ten_test -> {23, 10};
                                threshold_twenty_test -> {30, 20};
                                threshold_thirty_test -> {40, 30};
                                threshold_fifty_test -> {60, 50};
                                threshold_one_hundred_test -> {120, 100}
                            end,
    Msg = <<"May the force be with you">>,
    {ok, SecretSociety} = dist_secret_society:start_link(NumActors, Threshold),

    [{msg, Msg}, {num_actors, NumActors}, {threshold, Threshold}, {society, SecretSociety} | Config].

end_per_testcase(_TestCase, _Config) ->
    dist_secret_society:stop().

threshold_one_test(Config) ->
    run(Config).

threshold_ten_test(Config) ->
    run(Config).

threshold_twenty_test(Config) ->
    run(Config).

threshold_thirty_test(Config) ->
    run(Config).

threshold_fifty_test(Config) ->
    run(Config).

threshold_one_hundred_test(Config) ->
    run(Config).

%% ------------------------------------------------------------------
%% Local Helper functions
%% ------------------------------------------------------------------
run(Config) ->
    Msg = ?config(msg, Config),
    Threshold = ?config(threshold, Config),
    PK = dist_secret_society:publish_public_key(),

    Ciphertext = erlang_tc_pk:encrypt(PK, Msg),

    Actors = lists:foldl(fun(I, Acc) ->
                                 {ok, Actor} = dist_secret_society:get_actor(I),
                                 [Actor | Acc]
                         end, [], lists:seq(1, Threshold + 1)),

    ok = dist_secret_society:start_decryption_meeting(),

    ok = lists:foreach(fun(Actor) ->
                               ok = dist_secret_society:send_msg({any, Actor}, Ciphertext),
                               ok = dist_secret_society:accept_decryption_share(Actor)
                       end, Actors),

    {ok, DecryptedMsg} = dist_secret_society:decrypt_msg(),
    ?assertEqual(DecryptedMsg, Msg),

    ok.
