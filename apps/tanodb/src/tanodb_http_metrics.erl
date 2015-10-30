-module(tanodb_http_metrics).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-record(state, {}).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.
rest_init(Req, _Opts) -> {ok, Req, #state{}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State) ->
    Metrics = tanodb_metrics:all(),
    Response = tanodb_json:encode(Metrics),
    {Response, Req, State}.

rest_terminate(_Req, _State) -> ok.
terminate(_Reason, _Req, _State) -> ok.
