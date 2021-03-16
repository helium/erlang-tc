-module(dist_threshold_sig_SUITE).

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
         threshold_forty_test/1,
         threshold_fifty_test/1,
         threshold_one_hundred_test/1
        ]).

%% common test callbacks

all() -> [
         threshold_one_test,
         threshold_ten_test,
         threshold_twenty_test,
         threshold_thirty_test,
         threshold_forty_test,
         threshold_fifty_test
         %% threshold_one_hundred_test
         ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    %% The criteria for completion is that there must be threshold + 1 sigs
    {NumNodes, Threshold} = case TestCase of
                                threshold_one_test -> {3, 1};
                                threshold_ten_test -> {23, 10};
                                threshold_twenty_test -> {30, 20};
                                threshold_thirty_test -> {40, 30};
                                threshold_forty_test -> {50, 40};
                                threshold_fifty_test -> {60, 50};
                                threshold_one_hundred_test -> {120, 100}
                            end,
    {ok, Network} = dist_chat_network:start_link(NumNodes, Threshold),

    [{num_nodes, NumNodes}, {threshold, Threshold}, {network, Network} | Config].

end_per_testcase(_TestCase, _Config) ->
    dist_chat_network:stop().

threshold_one_test(Config) ->
    run(Config).

threshold_ten_test(Config) ->
    run(Config).

threshold_twenty_test(Config) ->
    run(Config).

threshold_thirty_test(Config) ->
    run(Config).

threshold_forty_test(Config) ->
    run(Config).

threshold_fifty_test(Config) ->
    run(Config).

threshold_one_hundred_test(Config) ->
    run(Config).

%% ------------------------------------------------------------------
%% Local Helper functions
%% ------------------------------------------------------------------
run(Config) ->
    Threshold = ?config(threshold, Config),

    ValidatorNodes = lists:foldl(fun(I, Acc) ->
                                      Node = dist_chat_network:get_node(I),
                                      [Node | Acc]
                              end, [], lists:seq(1, Threshold + 1)),

    %% Register a new user, Alice, with the network. Alice wants to add a message to blocks.
    Alice = dist_chat_network:create_user("alice"),
    AliceID = dist_chat_network:user_id(Alice),
    AliceGreeting = <<"Alice here, hello!">>,

    ok = lists:foreach(fun(Node) ->
                          ok = dist_chat_network:send_msg(Alice, Node, AliceGreeting)
                  end, ValidatorNodes),

    ok = dist_chat_network:step(),

    Blocks = dist_chat_network:blocks(),

    ?assertEqual(1, length(Blocks)),
    ct:pal("Blocks: ~p", [Blocks]),

    %% Check that the blocks contain the block we expect
    [{AliceID, AliceGreeting, _}] = Blocks,

    ok.
