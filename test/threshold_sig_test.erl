-module(threshold_sig_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    %% Creates a new network of 3 nodes running our chat protocol. The protocol has a
    %% signing-threshold of 1. This means each message requires 2 validator signatures before it can be
    %% added to blocks.
    Network = chat_network:new_network(3, 1),

    {ok, Node1} = chat_network:get_node(Network, 1),
    {ok, Node2} = chat_network:get_node(Network, 2),

    %% Register a new user, Alice, with the network. Alice wants to add a message to blocks.
    {Alice, Network1} = chat_network:create_user(Network),
    AliceGreeting = <<"Alice here, hello!">>,

    %% Alice sends her message to a validator. The validator signs the message. Before Alice can
    %% send her message to a second validator, the network runs a round of consensus. Because
    %% Alice's message has only one validator signature, it is not added to blocks.
    Network2 = chat_network:send_msg(Network1, chat_network:user_id(Alice), Node1, AliceGreeting),
    Network3 = chat_network:step(Network2),

    ?assertEqual(0, length(chat_network:blocks(Network3))),

    %% Alice sends her message to a second validator. The validator signs the message. Alice's
    %% message now has two signatures (which is `threshold + 1` signatures). The network runs a
    %% round of consensus, which successfully creates a combined-signature for Alice's message.
    %% Alice's message is appended to blocks.
    Network4 = chat_network:send_msg(Network3, chat_network:user_id(Alice), Node2, AliceGreeting),
    Network5 = chat_network:step(Network4),

    ?assertEqual(1, length(chat_network:blocks(Network5))),

    ok.
