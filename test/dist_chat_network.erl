-module(dist_chat_network).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([
    start_link/2,
    stop/0,
    create_user/1,
    get_node/1,
    step/0,
    send_msg/3,
    blocks/0,
    user_id/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    pk_set :: erlang_tc_pk_set:pk_set(),
    chat_nodes :: [chat_node()],
    blocks = [] :: blocks(),
    n_users = 0 :: non_neg_integer()
}).

-record(chat_node, {
    id :: node_id(),
    sk_share :: erlang_tc_sk_share:sk_share(),
    pk_share :: erlang_tc_pk_share:pk_share(),
    pending = #{} :: pending()
}).

-record(node_signature, {node_id :: node_id(), sig :: erlang_tc_sig_share:sig_share()}).

-record(user, {id :: user_id(), name :: string()}).

-type state() :: #state{}.
-type user_id() :: non_neg_integer().
-type node_id() :: non_neg_integer().
-type msg() :: binary().
-type pending() :: #{user_id() => signed_msgs()}.
-type signed_msgs() :: #{msg() => [node_signature()]}.
-type node_signature() :: #node_signature{}.
-type chat_node() :: #chat_node{}.
-type block() :: {user_id(), msg(), erlang_tc_sig:sig()}.
-type blocks() :: [block()].
-type user() :: #user{}.

%%%===================================================================
%%% API
%%%===================================================================
start_link(NumNodes, Threshold) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [NumNodes, Threshold], []).

stop() ->
    gen_server:call(?SERVER, stop).

-spec create_user(Name :: string()) -> user().
create_user(Name) ->
    gen_server:call(?SERVER, {create_user, Name}).

-spec get_node(NodeID :: node_id()) -> chat_node().
get_node(NodeID) ->
    gen_server:call(?SERVER, {get_node, NodeID}).

-spec send_msg(User :: user(), Node :: chat_node(), Msg :: binary()) -> ok.
send_msg(User, Node, Msg) ->
    gen_server:call(?SERVER, {send_msg, User, Node, Msg}).

-spec blocks() -> blocks().
blocks() ->
    gen_server:call(?SERVER, blocks, timer:seconds(120)).

-spec step() -> ok.
step() ->
    gen_server:cast(?SERVER, step).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([NumNodes, Threshold]) ->
    {ok, init_network(NumNodes, Threshold)}.

handle_call(blocks, _From, State) ->
    {reply, blocks(State), State};
handle_call({send_msg, User, Node, Msg}, _From, State) ->
    Pending = node_recv(Node, user_id(User), Msg),
    NewNode = Node#chat_node{pending = Pending},
    NewState = update_node(State, NewNode),
    {reply, ok, NewState};
handle_call({create_user, Name}, _From, State) ->
    UserID = State#state.n_users,
    NewUser = new_user(UserID, Name),
    {reply, NewUser, State#state{n_users = UserID + 1}};
handle_call({get_node, NodeID}, _From, State) ->
    Reply = internal_get_node(NodeID, State),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(step, State) ->
    NewState =
        case maybe_create_block(State) of
            undefined ->
                %% no block produced
                State;
            Block ->
                add_block(State, Block)
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec init_network(NumNodes :: non_neg_integer(), Threshold :: non_neg_integer()) -> state().
init_network(NumNodes, Threshold) ->
    SKSet = erlang_tc_sk_set:random(Threshold),
    PKSet = erlang_tc_sk_set:public_keys(SKSet),

    Nodes = lists:map(
        fun(ID) ->
            SKShare = erlang_tc_sk_set:secret_key_share(SKSet, ID),
            PKShare = erlang_tc_pk_set:public_key_share(PKSet, ID),
            new_node(ID, SKShare, PKShare)
        end,
        lists:seq(1, NumNodes)
    ),
    #state{pk_set = PKSet, chat_nodes = Nodes}.

new_node(ID, SKShare, PKShare) ->
    #chat_node{id = ID, sk_share = SKShare, pk_share = PKShare}.

new_user(UserID, Name) ->
    #user{id = UserID, name = Name}.

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

-spec node_signature(Node :: chat_node(), Msg :: msg()) -> node_signature().
node_signature(Node, Msg) ->
    #node_signature{
        node_id = node_id(Node),
        sig = erlang_tc_sk_share:sign(node_sk_share(Node), Msg)
    }.

-spec node_id(Node :: chat_node()) -> node_id().
node_id(Node) ->
    Node#chat_node.id.

node_sk_share(Node) ->
    Node#chat_node.sk_share.

user_id(User) ->
    User#user.id.

node_pk_share(Node) ->
    Node#chat_node.pk_share.

-spec update_node(State :: state(), Node :: chat_node()) -> state().
update_node(State, Node) ->
    Nodes = chat_nodes(State),
    NewNodes = setnth(node_id(Node), Nodes, Node),
    State#state{chat_nodes = NewNodes}.

chat_nodes(State) ->
    State#state.chat_nodes.

setnth(1, [_ | Rest], New) -> [New | Rest];
setnth(I, [E | Rest], New) -> [E | setnth(I - 1, Rest, New)].

-spec maybe_create_block(State :: state()) -> undefined | block().
maybe_create_block(State) ->
    Res = create_block(State),
    case Res of
        {done, undefined} -> undefined;
        {done, {error, cannot_combine}} -> undefined;
        {done, B} -> B
    end.

all_pending(State) ->
    Nodes = chat_nodes(State),

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

pk_set(State) ->
    State#state.pk_set.

create_block(State) ->
    PKSet = pk_set(State),
    AllPending = all_pending(State),
    lists:foldl(
        fun
            ({UserID, SignedMsgs}, not_done = Acc) ->
                lists:foldl(
                    fun
                        ({Msg, Sigs}, not_done) ->
                            Fun = fun(NodeSig) ->
                                NodeID = get_node_id(NodeSig),
                                NodeSigShare = get_node_sig(NodeSig),
                                {ok, Node} = internal_get_node(NodeID, State),
                                PKShare = node_pk_share(Node),
                                {Time0, Result} = timer:tc(
                                    fun() ->
                                        case
                                            erlang_tc_pk_share:verify(PKShare, NodeSigShare, Msg)
                                        of
                                            false ->
                                                false;
                                            true ->
                                                {true, {NodeID, NodeSigShare}}
                                        end
                                    end
                                ),
                                ct:pal("Actual sig share verify time: ~p", [Time0 div 1000]),
                                Result
                            end,
                            case lists:filtermap(Fun, Sigs) of
                                [] ->
                                    {done, undefined};
                                L ->
                                    {Time, Result} = timer:tc(
                                        fun() ->
                                            %% try to combine
                                            case erlang_tc_pk_set:combine_signatures(PKSet, L) of
                                                {error, _} = E ->
                                                    {done, E};
                                                Sig ->
                                                    {done, new_block(UserID, Msg, Sig)}
                                            end
                                        end
                                    ),
                                    ct:pal("Actual combine_signatures time: ~p", [Time div 1000]),
                                    Result
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

internal_get_node(NodeID, State) ->
    try
        Node = lists:nth(NodeID, chat_nodes(State)),
        {ok, Node}
    catch
        _W:_Why:_ST ->
            {error, not_found}
    end.

-spec new_block(UserID :: user_id(), Msg :: msg(), Sig :: erlang_tc_sig:sig()) -> block().
new_block(UserID, Msg, Sig) ->
    {UserID, Msg, Sig}.

get_node_id(NodeSignature) ->
    NodeSignature#node_signature.node_id.

get_node_sig(NodeSignature) ->
    NodeSignature#node_signature.sig.

-spec add_block(State :: state(), Block :: block()) -> state().
add_block(State, Block) ->
    Blocks = blocks(State),
    State#state{blocks = Blocks ++ [Block]}.

blocks(State) ->
    State#state.blocks.
