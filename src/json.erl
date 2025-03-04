-module(json).

%% Include files

%% Exported functions

-export([
    new/0,
    get_value/2,
    get_value/3,
    set_value/3,
    del_value/2,
    get_child/2,
    get_child/3
]).

-export_type([
    json/0,
    json_object/0,
    json_list/0,
    json_number/0,
    json_string/0,
    json_bool/0,
    json_null/0
]).

-type json_object() :: #{binary() => json()}.
-type json_list() :: [json()].
-type json_number() :: number().
-type json_string() :: binary().
-type json_bool() :: boolean().
-type json_null() :: 'null'.
-type json() :: json_number() | json_string() | json_bool() | json_null() | json_list() | json_object().

%% API

-spec new() ->  JsonObject :: json().

new() ->
    {[]}.

-spec get_value(Key, JsonObject) -> Value | 'undefined' when
      Key :: binary(),
      JsonObject :: json_object(),
      Value :: json().

%% @doc Returns the value of a simple key/value property in json object
%% @equiv get_value(Key, JsonObject, undefined)

get_value(Key, JsonObj) ->
    get_value(Key, JsonObj, undefined).

-spec get_value(Key, JsonObject, Default | 'undefined') -> Value | 'undefined' when
      Key :: binary(),
      JsonObject :: json_object(),
      Default :: json(),
      Value :: json().

%% @doc Returns the value of a simple key/value property in json object

get_value(Key, JsonObj, Default) ->
    maps:get(Key, JsonObj, Default).

-spec set_value(Key, JsonObject, Value) -> JsonObject when
      Key :: binary(),
      JsonObject :: json_object(),
      Value :: json().

set_value(Key, JsonObj, Value) ->
    maps:put(Key, Value, JsonObj).

-spec del_value(Key, JsonObject) -> JsonObject when
      Key :: binary(),
      JsonObject :: json_object().

del_value(Key, JsonObj) ->
    maps:remove(Key, JsonObj).

get_child(KeyList, JsonObj) ->
    get_child(KeyList, JsonObj, undefined).

-spec get_child(KeyList, JsonObject, Default | 'undefined') -> Value | 'undefined' when
      KeyList :: [binary()|integer()],
      JsonObject :: json(),
      Default :: json(),
      Value :: json().

get_child([N|Tail], List, Default) when is_integer(N), is_list(List) ->
    case get_nth(N, List) of
        undefined ->
            Default;
        ChildJsonObj ->
            get_child(Tail, ChildJsonObj, Default)
    end;
get_child([Key|Tail], Map, Default) when is_binary(Key), is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            Default;
        ChildJson ->
            get_child(Tail, ChildJson, Default)
    end;
get_child([], JsonObj, _Default) ->
    JsonObj.

%% Local functions

get_nth(0, [H|_]) ->
    H;
get_nth(N, [_|T]) when N > 0 ->
    get_nth(N - 1, T);
get_nth(_, []) ->
    undefined.
