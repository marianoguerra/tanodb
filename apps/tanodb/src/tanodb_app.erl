-module(tanodb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tanodb_metrics:init(),
    init_http(),
    case tanodb_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, tanodb_vnode}]),
            ok = riak_core_node_watcher:service_up(tanodb, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

% private functions

routes() ->
    [
     {"/ping", tanodb_http_ping, []}
    ].

init_http() ->
    DispatchRoutes = routes(),
    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    CowboyOpts = [{env, [{dispatch, Dispatch}]}],
    ApiAcceptors = envd(http_acceptors, 100),
    ApiPort = envd(http_port, 8080),

    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                CowboyOpts).

env(App, Par, Def) -> application:get_env(App, Par, Def).
envd(Par, Def) -> env(tanodb, Par, Def).

