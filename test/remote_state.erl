-module(remote_state).

%% Include files

-include("remote.hrl").

%% Exported functions

-export([
    new/0,
    name/1,
    set_name/2,
    value/2,
    set_value/3
]).

-remote({remote_parse_transform_tests, modify, [set_name/2, set_value/3]}).
-remote({remote_parse_transform_tests, info, [name/1, value/2]}).

-remote_singleton({remote_parse_transform_tests, call_singleton, [singleton1/1, singleton2/2]}).

%% API

new() ->
    {undefined, dict:new()}.

name({Name,_}) ->
    Name.

set_name({_,Values}, NewName) ->
    {NewName, Values}.

value({_,Values}, Key) ->
    dict:find(Key, Values).

set_value({Name,Values}, Key, Value) ->
    NewValues = dict:store(Key, Value, Values),
    {Name, NewValues}.

singleton1(A) ->
    A + 1.

singleton2(A, B) ->
    A + B.

%% Local functions
