-module(script_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

eval_test() ->
    ?assertEqual(script:eval("2+2"), 4),
    ?assertEqual(script:eval(<<"round(4.6)">>), 5),
    ?assertError(empty, script:eval("")),
    ?assertError(empty, script:eval(<<>>)),
    ?assertEqual(script:eval("A or B", script:bindings([{'A', true}, {'B', false}])), true).

exec_test() ->
    ?assertEqual(undefined, script:exec("")).

%% Local functions
