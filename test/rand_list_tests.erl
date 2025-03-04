-module(rand_list_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

select_test() ->
    ?assertError(_, rand_list:select([])),
    ?assertEqual(rand_list:select([1]), 1),
    ?assertEqual(rand_list:select_list([], 0), []),
    ?assertError(_, rand_list:select([], 3)),
    ?assertEqual(rand_list:select_list([hi], 3), [hi, hi, hi]),
    ?assertEqual(rand_list:select_list(["hi", "hi", "hi"], 2), ["hi", "hi"]).

take_test() ->
    ?assertError(_, rand_list:take([])),
    ?assertEqual(rand_list:take([1.0]), {1.0, []}),
    ?assertEqual(rand_list:take_list([], 0), {[], []}),
    ?assertError(_, rand_list:take_list([a, b], 3)),
    ?assertEqual(rand_list:take_list([hi], 1), {[hi], []}),
    ?assertEqual(rand_list:take_list(["hi", "hi"], 1), {["hi"], ["hi"]}),
    ?assertEqual(rand_list:take_list([false, false, false], 3), {[false, false, false], []}).

shuffle_test() ->
    ?assertEqual(rand_list:shuffle([]), []),
    ?assertEqual(rand_list:shuffle([a]), [a]),
    ?assertEqual(rand_list:shuffle(["hi", "hi"]), ["hi", "hi"]),
    ?assert(lists:member(rand_list:shuffle([1,2,3]), perms([1,2,3]))).

shuffle_random_test() ->
    assert_random(fun rand_list:shuffle/1).

sublist_random_test() ->
    assert_random(fun rand_list:sublist/1).

take_random_test() ->
    assert_random(fun(List) -> {Item, _} = rand_list:take(List), Item end).

takeN_random_test() ->
    assert_random(fun(List) -> {Items, _} = rand_list:take_list(List, 2), Items end).

select_random_test() ->
    assert_random(fun rand_list:select/1).

selectN_random_test() ->
    assert_random(fun(List) -> Items = rand_list:select_list(List, 2), ordsets:size(ordsets:from_list(Items)) end),
    assert_random(fun(List) -> rand_list:select_list(List, 2) end).

weight_select_test() ->
    ?assertEqual(rand_list:weight_select(2, [{a, 0}]), {a, 0}),
    ?assertEqual(element(1, rand_list:weight_select(2, [{a, 0.85}, {a, 52}])), a),
    {A, B} = weight_calc(0, 0, 1000),
    ?assert(A =< B).

%% Local functions

weight_calc(A, B, 0) ->
    {A, B};
weight_calc(A, B, Count) ->
    case rand_list:weight_select(2, [{a, 1}, {b, 2}]) of
        {a, 1} -> weight_calc(A+1, B, Count-1);
        {b, 2} -> weight_calc(A, B+1, Count-1)
    end.

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

assert_random(Gen) ->
    List = lists:seq(1, 4),
    Variations = [  Gen(List) || _ <- lists:seq(1, 25) ],
    Count = ordsets:size(ordsets:from_list(Variations)),
    ?assert(Count > 1).
