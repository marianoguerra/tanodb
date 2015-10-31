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
            setup_basic_users_and_groups(),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

% private functions

rcs_is_user_authorized(Req, #{user_ctx := _UserCtx}) ->
    ResultJson = [{token, <<"atoken">>}],
    ResultJsonBin = tanodb_json:encode(ResultJson),
    Req1 = cowboy_req:set_resp_body(ResultJsonBin, Req),
    {true, Req1}.

add_user(Username, Password, Groups) ->
    R = case riak_core_security:add_user(Username, [{"password", Password},
                                                    {"groups", [Groups]}]) of
            ok ->
                R0 = riak_core_security:add_source([Username],
                                                   {{127, 0, 0, 1}, 32},
                                                   password, []),
                lager:info("Add Source for ~p: ~p", [Username, R0]),
                ok;
            Other -> Other
        end,
    lager:info("Create user ~p: ~p", [Username, R]).

add_group(Name, Groups) ->
    R = riak_core_security:add_group(Name, Groups),
    lager:info("Create Group ~p: ~p", [Name, R]).

setup_basic_users_and_groups() ->
    add_group(<<"users">>, []),
    add_group(<<"admins">>, [{"groups", [<<"users">>]}]),

    add_user(<<"user">>, "secret", <<"users">>),
    add_user(<<"admin">>, "secret", <<"admins">>).

routes() ->
    JsonEncoder = fun tanodb_json:encode/1,
    JsonDecoder = fun tanodb_json:decode/1,
    IsAuthorizedFun = fun (Req, _Info) -> {true, Req} end,
    IsUserAuthorizedFun = fun rcs_is_user_authorized/2,
    RcsOpts = #{env_keys => [tanodb],
                json_encoder => JsonEncoder, json_decoder => JsonDecoder,
                is_user_authorized => IsUserAuthorizedFun,
                base_uri => "/admin", is_authorized => IsAuthorizedFun},

    [
     {"/ping", tanodb_http_ping, []},
     {"/metrics", tanodb_http_metrics, []},
     {"/store/:bucket", tanodb_http_store, []},
     {"/store/:bucket/:key", tanodb_http_store, []},
     {"/admin/:action", rcs_cowboy_handler, RcsOpts},
     {"/admin/:action/:param1", rcs_cowboy_handler, RcsOpts},
     {"/ui/[...]", cowboy_static,
      {priv_dir, tanodb, "ui", [{mimetypes, cow_mimetypes, all}]}}
    ].

init_http() ->
    DispatchRoutes = routes(),
    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),
    ApiMiddlewares = [cowboy_exometer, cowboy_router, cowboy_handler],

    CowboyOpts = [{env, [{dispatch, Dispatch}]},
                  {onresponse, fun cowboy_exometer:cowboy_response_hook/4},
                  {middlewares, ApiMiddlewares}],
    ApiAcceptors = envd(http_acceptors, 100),
    ApiPort = envd(http_port, 8080),

    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                CowboyOpts).

env(App, Par, Def) -> application:get_env(App, Par, Def).
envd(Par, Def) -> env(tanodb, Par, Def).

