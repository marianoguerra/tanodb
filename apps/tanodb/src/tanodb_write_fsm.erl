-module(tanodb_write_fsm).
-behavior(gen_fsm).

%% API
-export([start_link/7, write/6, delete/4]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

-ignore_xref([start_link/7, init/1, code_change/4, handle_event/3,
              handle_info/3, handle_sync_event/4, terminate/3, write/6,
              delete/4]).

-export([prepare/2, execute/2, waiting/2]).

-ignore_xref([prepare/2, execute/2, waiting/2]).

%% req_id: The request id so the caller can verify the response.
%% sender: The pid of the sender so a reply can be made.
%% prelist: The preflist for the given {Bucket, Key} pair.
%% num_w: The number of successful write replies.
-record(state, {req_id :: pos_integer(),
                from :: pid(),
                n :: pos_integer(),
                w :: pos_integer(),
                key :: string(),
                accum = [],
                action :: write | delete,
                data = undefined :: term() | undefined,
                preflist :: riak_core_apl:preflist2(),
                num_w = 0 :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Key, Data, N, W, Action) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Key, Data, N, W, Action],
                       []).

write(N, W, Key, Data, Pid, ReqID) ->
    tanodb_write_fsm_sup:start_write_fsm([ReqID, Pid, Key, Data, N, W, write]),
    {ok, ReqID}.

delete(N, Key, Pid, ReqID) ->
    tanodb_write_fsm_sup:start_write_fsm([ReqID, Pid, Key, nil, N, N, delete]),
    {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the state data.
init([ReqID, From, Key, Data, N, W, Action]) ->
    SD = #state{req_id=ReqID, from=From, action=Action, key=Key, data=Data,
                n=N, w=W},
    {ok, prepare, SD, 0}.

%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0=#state{n=N, key=Key}) ->
    DocIdx = riak_core_util:chash_key(Key),
    Preflist = riak_core_apl:get_apl(DocIdx, N, tanodb),
    SD = SD0#state{preflist=Preflist},
    {next_state, execute, SD, 0}.

%% @doc Execute the write request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, SD0=#state{req_id=ReqID, key=Key, action=Action, data=Data,
                            preflist=Preflist}) ->
    Command = case Action of
                  delete -> {delete, ReqID, Key};
                  write -> {put, ReqID, Key, Data}
              end,
    riak_core_vnode_master:command(Preflist, Command, {fsm, undefined, self()},
                                   tanodb_vnode_master),
    {next_state, waiting, SD0}.

%% @doc Wait for W write reqs to respond.
waiting({ReqID, Result}, SD0=#state{from=From, num_w=NumW0, w=W, accum=Accum}) ->
    NumW = NumW0 + 1,
    Accum1 = [Result|Accum],
    SD = SD0#state{num_w=NumW, accum=Accum1},
    if NumW =:= W ->
           From ! {ReqID, {ok, Accum1}},
           {stop, normal, SD};
       true -> {next_state, waiting, SD}
    end.

handle_info(Info, _StateName, StateData) ->
    lager:warning("got unexpected info ~p", [Info]),
    {stop, badmsg, StateData}.

handle_event(Event, _StateName, StateData) ->
    lager:warning("got unexpected event ~p", [Event]),
    {stop, badmsg, StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    lager:warning("got unexpected sync event ~p", [Event]),
    {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
terminate(_Reason, _SN, _SD) -> ok.
