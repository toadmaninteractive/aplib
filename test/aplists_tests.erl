-module(aplists_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

keyupdate_test() ->
    OldList = [{state, a1, data1, 1, 1.0}, {state, a2, data2, 2, 2.0}],
    List1 = [{state, a1, data3, 1, 1.0}, {state, a2, data2, 2, 2.0}],
    List2 = [{state, a1, data1, 1, 1.0}, {state, a2, data2, 2, 2.0}, {state, a3, data3, 3, 3.0}],
    Test1 = aplists:keyupdate(a1, 2, OldList, fun(A) -> setelement(3, A, data3) end, {state, undefined, undefined, undefined, undefined}),
    Test2 = aplists:keyupdate(a3, 2, OldList, fun(undefined) -> undefined end, {state, a3, data3, 3, 3.0}),
    ?assert(List1 =:= Test1),
    ?assert(List2 =:= Test2).

weight_select_test() ->
    ?assertEqual({a, 10}, aplists:weight_select(0.3, 2, [{a, 10}, {b, 20}])),
    ?assertEqual({b, 20}, aplists:weight_select(0.4, 2, [{a, 10}, {b, 20}])).

keysum_test() ->
    ?assertEqual(14, aplists:keysum(2, [{a, 3}, {b, 9}, {c, 2}])).

seq_zip_test() ->
    ?assertEqual([{1, a}, {2, b}], aplists:seq_zip([a, b])),
    ?assertEqual([], aplists:seq_zip([])),
    ?assertEqual([{3, a}, {4, b}], aplists:seq_zip([a, b], 3)),
    ?assertEqual([], aplists:seq_zip([], 3)).

match_test() ->
    ?assertEqual([{test, 5}, {test, [2,3]}], aplists:match({test, '_'}, [1, {test, 5}, {other, hello}, {test, [2, 3]}])).

%% Local functions
