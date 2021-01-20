-module(chat_network).

-include_lib("eunit/include/eunit.hrl").

-export([
    new_network/2,
    create_user/1,
    get_node/2,
    step/1,
    send_msg/4,
    user_id/1,
    blocks/1
]).

-type user_id() :: non_neg_integer().
-type node_id() :: non_neg_integer().
-type msg() :: binary().

-record(network, {
    pk_set :: public_key_set:pk_set(),
    chat_nodes :: [chat_node()],
    blocks = [] :: blocks(),
    n_users = 0 :: non_neg_integer()
}).

-record(chat_node, {
    id :: node_id(),
    sk_share :: signature_share:sk_share(),
    pk_share :: public_key_share:pk_share(),
    pending = #{} :: pending()
}).

-record(node_signature, {node_id :: node_id(), sig :: signature_share:sig_share()}).

-record(user, {id :: user_id()}).

-type network() :: #network{}.
-type pending() :: #{user_id() => signed_msgs()}.
-type signed_msgs() :: #{msg() => [node_signature()]}.
-type node_signature() :: #node_signature{}.
-type chat_node() :: #chat_node{}.
-type block() :: {user_id(), msg(), signature:sig()}.
-type blocks() :: [block()].
-type user() :: #user{}.

-spec new_network(NumNodes :: non_neg_integer(), Threshold :: non_neg_integer()) -> network().
new_network(NumNodes, Threshold) ->
    SKSet = secret_key_set:random(Threshold),
    PKSet = secret_key_set:public_keys(SKSet),

    Nodes = lists:map(
        fun(ID) ->
            SKShare = secret_key_set:secret_key_share(SKSet, ID),
            PKShare = public_key_set:public_key_share(PKSet, ID),
            new_node(ID, SKShare, PKShare)
        end,
        lists:seq(1, NumNodes)
    ),
    #network{pk_set = PKSet, chat_nodes = Nodes}.

pk_set(Network) ->
    Network#network.pk_set.

users(Network) ->
    Network#network.n_users.

increment_users(Network) ->
    Network#network{n_users = users(Network) + 1}.

new_user(UserID) ->
    #user{id = UserID}.

chat_nodes(Network) ->
    Network#network.chat_nodes.

update_node(Network, Node) ->
    Nodes = chat_nodes(Network),
    NewNodes = setnth(node_id(Node), Nodes, Node),
    Network#network{chat_nodes = NewNodes}.

-spec get_node(Network :: network(), NodeID :: node_id()) -> {ok, chat_node()} | {error, not_found}.
get_node(Network, NodeID) ->
    try
        Node = lists:nth(NodeID, chat_nodes(Network)),
        {ok, Node}
    catch
        _W:_Why:_ST ->
            {error, not_found}
    end.

-spec step(Network :: network()) -> network().
step(Network) ->
    case run_consensus(Network) of
        undefined ->
            %% no block produced
            Network;
        Block ->
            add_block(Network, Block)
    end.

-spec run_consensus(Network :: network()) -> undefined | block().
run_consensus(Network) ->
    Res = create_block(Network),
    case Res of
        {done, undefined} -> undefined;
        {done, {error, cannot_combine}} -> undefined;
        {done, B} -> B
    end.

-spec blocks(Network :: network()) -> blocks().
blocks(Network) ->
    Network#network.blocks.

-spec add_block(Network :: network(), Block :: block()) -> network().
add_block(Network, Block) ->
    Blocks = blocks(Network),
    Network#network{blocks = Blocks ++ [Block]}.

-spec new_block(UserID :: user_id(), Msg :: msg(), Sig :: signature:sig()) -> block().
new_block(UserID, Msg, Sig) ->
    {UserID, Msg, Sig}.

-spec create_user(Network :: network()) -> {user(), network()}.
create_user(Network) ->
    UserID = users(Network),
    User = new_user(UserID),
    {User, increment_users(Network)}.

new_node(ID, SKShare, PKShare) ->
    #chat_node{id = ID, sk_share = SKShare, pk_share = PKShare}.

node_sk_share(Node) ->
    Node#chat_node.sk_share.

node_pk_share(Node) ->
    Node#chat_node.pk_share.

-spec node_id(Node :: chat_node()) -> node_id().
node_id(Node) ->
    Node#chat_node.id.

-spec node_signature(Node :: chat_node(), Msg :: msg()) -> node_signature().
node_signature(Node, Msg) ->
    #node_signature{
        node_id = node_id(Node),
        sig = secret_key_share:sign(node_sk_share(Node), Msg)
    }.

get_node_id(NodeSignature) ->
    NodeSignature#node_signature.node_id.

get_node_sig(NodeSignature) ->
    NodeSignature#node_signature.sig.

-spec add_pending(
    Pending :: pending(),
    {User :: user_id(), Msg :: msg(), NodeSignature :: node_signature()}
) -> pending().
add_pending(Pending, {UserID, Msg, NodeSignature}) ->
    Msgs = maps:get(UserID, Pending, #{}),
    Signatures = maps:get(Msg, Msgs, []),
    NewSignatures = Signatures ++ [NodeSignature],
    NewMsgs = maps:put(Msg, NewSignatures, Msgs),
    NewPending = maps:put(UserID, NewMsgs, Pending),
    NewPending.

-spec pending(Node :: chat_node()) -> pending().
pending(Node) ->
    Node#chat_node.pending.

-spec node_recv(Node :: chat_node(), UserID :: user_id(), Msg :: msg()) -> pending().
node_recv(Node, UserID, Msg) ->
    Sig = node_signature(Node, Msg),
    Pending = pending(Node),
    NewPending = add_pending(Pending, {UserID, Msg, Sig}),
    NewPending.

send_msg(Network, UserID, Node, Msg) ->
    Pending = node_recv(Node, UserID, Msg),
    NewNode = Node#chat_node{pending = Pending},
    update_node(Network, NewNode).

setnth(1, [_ | Rest], New) -> [New | Rest];
setnth(I, [E | Rest], New) -> [E | setnth(I - 1, Rest, New)].

user_id(User) ->
    User#user.id.

all_pending(Network) ->
    Nodes = chat_nodes(Network),

    lists:foldl(
        fun(Node, Acc) ->
            Pending = pending(Node),

            Fun = fun({UserID, SignedMsgs}, Acc2) ->
                UserMsgs = maps:get(UserID, Acc2, #{}),
                Fun2 = fun({Msg, Sigs}, Acc3) ->
                    NewSigs =
                        case Sigs of
                            [] -> Acc3;
                            S -> maps:get(Msg, Acc3, []) ++ S
                        end,
                    maps:put(UserID, maps:put(Msg, NewSigs, Acc3), Acc2)
                end,

                lists:foldl(Fun2, UserMsgs, maps:to_list(SignedMsgs))
            end,

            lists:foldl(Fun, Acc, maps:to_list(Pending))
        end,
        #{},
        Nodes
    ).

create_block(Network) ->
    PKSet = pk_set(Network),
    AllPending = all_pending(Network),
    lists:foldl(
        fun
            ({UserID, SignedMsgs}, not_done = Acc) ->
                lists:foldl(
                    fun
                        ({Msg, Sigs}, not_done) ->
                            Fun = fun(NodeSig) ->
                                NodeID = get_node_id(NodeSig),
                                NodeSigShare = get_node_sig(NodeSig),
                                {ok, Node} = get_node(Network, NodeID),
                                PKShare = node_pk_share(Node),
                                case public_key_share:verify(PKShare, NodeSigShare, Msg) of
                                    false ->
                                        false;
                                    true ->
                                        {true, {NodeID, NodeSigShare}}
                                end
                            end,
                            case lists:filtermap(Fun, Sigs) of
                                [] ->
                                    {done, undefined};
                                L ->
                                    %% try to combine
                                    case public_key_set:combine_signatures(PKSet, L) of
                                        {error, _}=E ->
                                            {done, E};
                                        Sig ->
                                            {done, new_block(UserID, Msg, Sig)}
                                    end
                            end;
                        (_, {done, R}) ->
                            {done, R}
                    end,
                    Acc,
                    maps:to_list(SignedMsgs)
                );
            (_, {done, Block}) ->
                Block
        end,
        not_done,
        lists:sort(maps:to_list(AllPending))
    ).

