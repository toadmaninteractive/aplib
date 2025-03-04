-module(json_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

child_test() ->
    Json = #{
        <<"key">> => <<"Value">>,
        <<"object">> =>
        #{
            <<"inner">> => <<"InnerValue">>
        }
    },
    ?assertEqual(json:get_child([<<"object">>, <<"inner">>], Json), <<"InnerValue">>),
    ?assertEqual(json:get_child([<<"key">>], Json), <<"Value">>),
    ?assertEqual(json:get_child([], Json), Json),
    ?assertEqual(json:get_child([<<"object">>, <<"unknown">>], Json), undefined),
    ok.

%% Local functions
