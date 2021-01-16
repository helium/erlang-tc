-module(threshold_sig_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    Network = chat_network:new_network(3, 1),
    %% ?debugFmt("Network: ~p", [Network]),

    {ok, Node1} = chat_network:get_node(Network, 1),
    {ok, Node2} = chat_network:get_node(Network, 2),
    %% ?debugFmt("Node1: ~p", [Node1]),
    %% ?debugFmt("Node2: ~p", [Node2]),

    {Alice, Network1} = chat_network:create_user(Network),
    %% ?debugFmt("Network1: ~p", [Network1]),
    AliceGreeting = <<"Alice here, hello!">>,

    Network2 = chat_network:send_msg(Network1, chat_network:user_id(Alice), Node1, AliceGreeting),
    %% ?debugFmt("Network2: ~p", [Network2]),
    Network3 = chat_network:step(Network2),
    %% ?debugFmt("Network3: ~p", [Network3]),

    ?assertEqual(0, length(chat_network:blocks(Network3))),

    Network4 = chat_network:send_msg(Network3, chat_network:user_id(Alice), Node2, AliceGreeting),
    %% ?debugFmt("Network4: ~p", [Network4]),
    Network5 = chat_network:step(Network4),
    %% ?debugFmt("Network5: ~p", [Network5]),

    ?assertEqual(1, length(chat_network:blocks(Network5))),

    ok.
