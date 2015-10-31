-module(tanodb_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, table_id, table_name}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    TableName = list_to_atom("tanodb_" ++ integer_to_list(Partition)),
    TableId = ets:new(TableName, [set, public, named_table,
                                  {write_concurrency, false},
                                  {read_concurrency, true}]),

    {ok, #state{partition=Partition, table_id=TableId,
                  table_name=TableName}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command({put, ReqId, Key, Value}, _Sender,
               State=#state{table_name=TableName, partition=Partition}) ->
    ets:insert(TableName, {Key, Value}),
    {reply, {ReqId, {ok, Partition}}, State};
handle_command({get, Key}, _Sender,
               State=#state{table_name=TableName, partition=Partition}) ->
    case ets:lookup(TableName, Key) of
        [] ->
            {reply, {not_found, Partition, Key}, State};
        [Value] ->
            {reply, {found, Partition, {Key, Value}}, State}
    end;
handle_command({delete, ReqId, Key}, _Sender,
               State=#state{table_name=TableName, partition=Partition}) ->
    case ets:lookup(TableName, Key) of
        [] ->
            {reply, {ReqId, {not_found, Partition, Key}}, State};
        [Value] ->
            true = ets:delete(TableName, Key),
            {reply, {ReqId, {found, Partition, {Key, Value}}}, State}
    end;
handle_command(Message, _Sender, State) ->
    lager:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State=#state{partition=Partition, table_name=TableName}) ->
    lager:info("fold req ~p", [Partition]),
    AccFinal = ets:foldl(fun ({Key, Val}, AccIn) ->
                                 lager:info("fold fun ~p: ~p", [Key, Val]),
                                 FoldFun(Key, Val, AccIn)
                         end, Acc0, TableName),
    {reply, AccFinal, State};

handle_handoff_command(Message, _Sender, State) ->
    lager:warning("handoff command ~p, ignoring", [Message]),
    {noreply, State}.

handoff_starting(TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff starting ~p: ~p", [Partition, TargetNode]),
    {true, State}.

handoff_cancelled(State=#state{partition=Partition}) ->
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff finished ~p: ~p", [Partition, TargetNode]),
    {ok, State}.

handle_handoff_data(BinData, State=#state{table_name=TableName}) ->
    TermData = binary_to_term(BinData),
    lager:info("handoff data received ~p", [TermData]),
    {Key, Value} = TermData,
    ets:insert(TableName, {Key, Value}),
    {reply, ok, State}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State=#state{table_name=TableName, partition=Partition}) ->
    IsEmpty = (ets:first(TableName) =:= '$end_of_table'),
    lager:info("is_empty ~p: ~p", [Partition, IsEmpty]),
    {IsEmpty, State}.

delete(State=#state{table_name=TableName, partition=Partition}) ->
    lager:info("delete ~p", [Partition]),
    ets:delete(TableName),
    {ok, State}.

handle_coverage({keys, Bucket}, _KeySpaces, {_, RefId, _},
                State=#state{table_name=TableName}) ->
    Keys0 = ets:match(TableName, {{Bucket, '$1'}, '_'}),
    Keys = lists:map(fun first/1, Keys0),
    {reply, {RefId, Keys}, State};
handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

% private functions
first([V|_]) -> V.
