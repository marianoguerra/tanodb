-module(tanodb_http_store).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         resource_exists/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

-record(state, {bucket, key, method, value}).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.
rest_init(Req, _Opts) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Key, Req2} = cowboy_req:binding(key, Req1),
    {Method, Req3} = method(Req2),
    {ok, Req3, #state{bucket=Bucket, key=Key, method=Method}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

resource_exists(Req, State=#state{bucket=Bucket, key=Key, method=Method})
        when Method =:= get; Method =:= delete ->
    case tanodb:Method({Bucket, Key}) of
        {not_found, _Partition, _BK} ->
            {false, Req, State};
        {found, _Partition, {_BK, {_BK1, Value}}} ->
            {true, Req, State#state{value=Value}}
    end;
resource_exists(Req, State=#state{}) ->
    {false, Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

to_json(Req, State=#state{value=Value}) ->
    Response = tanodb_json:encode(Value),
    {Response, Req, State}.

from_json(Req, State=#state{bucket=Bucket, key=Key}) ->
    {ok, BodyRaw, Req1} = cowboy_req:body(Req),
    try
        Body = tanodb_json:decode(BodyRaw),
        {ok, _Partition} = tanodb:put({Bucket, Key}, Body),
        {true, Req1, State}
    catch
        _T:_E -> {false, Req1, State}
    end.

delete_resource(Req, State=#state{}) -> {true, Req, State}.

rest_terminate(_Req, _State) -> ok.
terminate(_Reason, _Req, _State) -> ok.

% private functions

method(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    AMethod = case Method of
                  <<"POST">> -> post;
                  <<"GET">> -> get;
                  <<"PUT">> -> put;
                  <<"DELETE">> -> delete;
                  <<"OPTIONS">> -> options;
                  <<"HEAD">> -> head
              end,
    {AMethod, Req1}.
