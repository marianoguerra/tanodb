-module(tanodb).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, get/1, delete/1, put/2, keys/1]).

-ignore_xref([ping/0, get/1, delete/1, put/2]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    tanodb_metrics:core_ping(),
    send_to_one({<<"ping">>, term_to_binary(os:timestamp())}, ping).

get(Key) ->
    tanodb_metrics:core_get(),
    send_to_one(Key, {get, Key}).

delete(Key) ->
    tanodb_metrics:core_delete(),
    send_to_one(Key, {delete, Key}).

put(Key, Value) ->
    tanodb_metrics:core_put(),
    send_to_one(Key, {put, Key, Value}).

keys(Bucket) ->
    tanodb_metrics:core_keys(),
    Timeout = 5000,
    tanodb_coverage_fsm:start({keys, Bucket}, Timeout).

% private functions

send_to_one(Key, Cmd) ->
    DocIdx = riak_core_util:chash_key(Key),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, tanodb_vnode_master).
