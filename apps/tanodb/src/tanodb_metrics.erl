-module(tanodb_metrics).
-export([all/0, init/0]).

-export([core_ping/0, core_put/0, core_get/0, core_delete/0, core_keys/0]).

-define(ENDPOINTS, [<<"ping">>, <<"metrics">>]).
-define(METRIC_CORE_PING, [tanodb, core, ping]).
-define(METRIC_CORE_PUT, [tanodb, core, put]).
-define(METRIC_CORE_GET, [tanodb, core, get]).
-define(METRIC_CORE_DELETE, [tanodb, core, delete]).
-define(METRIC_CORE_KEYS, [tanodb, core, keys]).

all() ->
 [{tanodb, tanodb_stats()},
  {http, cowboy_exometer:stats(?ENDPOINTS)},
  {node, node_stats()},
  {core, core_stats()}].

core_stats() ->
    [
     {ping, unwrap_metric_value(?METRIC_CORE_PING)},
     {put, unwrap_metric_value(?METRIC_CORE_PUT)},
     {get, unwrap_metric_value(?METRIC_CORE_GET)},
     {keys, unwrap_metric_value(?METRIC_CORE_KEYS)},
     {delete, unwrap_metric_value(?METRIC_CORE_DELETE)}
    ].

core_ping() -> exometer:update(?METRIC_CORE_PING, 1).
core_put() -> exometer:update(?METRIC_CORE_PUT, 1).
core_get() -> exometer:update(?METRIC_CORE_GET, 1).
core_keys() -> exometer:update(?METRIC_CORE_KEYS, 1).
core_delete() -> exometer:update(?METRIC_CORE_DELETE, 1).

init() ->
    cowboy_exometer:init(?ENDPOINTS),
    exometer:new(?METRIC_CORE_PING, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_PUT, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_GET, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_KEYS, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_DELETE, spiral, [{time_span, 60000}]).

tanodb_stats() ->
    Stats = riak_core_stat:get_stats(),
    lists:map(fun metric_key_to_string/1, Stats).

node_stats() ->
    [{Abs1, Inc1}] = recon:node_stats_list(1, 0),
    [{abs, Abs1}, {inc, Inc1}].

metric_key_to_string({K, V}) ->
    StrKeyTokens = lists:map(fun to_string/1, tl(tl(K))),
    StrKey = string:join(StrKeyTokens, "_"),
    BinKey = list_to_binary(StrKey),
    {BinKey, V}.

to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V.

unwrap_metric_value(Key) ->
    case exometer:get_value(Key) of
        {ok, Val} -> Val;
        Other -> 
            lager:warning("Error getting endpoint value ~p: ~p",
                          [Key, Other]),
            []
    end.
