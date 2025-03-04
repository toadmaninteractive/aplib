-module(remote_parse_transform_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([
    info/2,
    modify/2,
    call_singleton/1
]).

%% API

info(Id, Informer) ->
    P = get_state(Id),
    Informer(P).

modify(Id, Modifier) ->
    P = get_state(Id),
    P1 = Modifier(P),
    set_state(Id, P1),
    ok.

call_singleton(Fun) ->
    Fun(42).

name_test() ->
    remote_state:set_name@(1, "name"),
    ?assertEqual("name", remote_state:name@(1)).

value_test() ->
    remote_state:set_value@(2, key1, 1),
    ?assertEqual({ok, 1}, remote_state:value@(2, key1)).

singleton_test() ->
    ?assertEqual(43, remote_state:singleton1@()),
    ?assertEqual(50, remote_state:singleton2@(8)).

%% Local functions

get_state(Id) ->
    case get(Id) of
        undefined -> remote_state:new();
        P -> P
    end.

set_state(Id, P) ->
    put(Id, P).
