-module(tanodb_json).
-export([decode/1, encode/1, is_json/1]).

decode(Obj)  -> jsx:decode(Obj, [return_maps]).
encode(Obj)  -> jsx:encode(Obj).
is_json(Obj) -> jsx:is_json(Obj).
