-module(aplib).

%% Include files

%% Exported functions

-export([
    start_apps/1,
    start_app_recursive/1,
    load_apps/1,
    load_apps_recursive/1,
    app_deps/1,
    app_deps_recursive/1,
    reload_config/0
]).

-export_type([
    sbyte/0,
    short/0,
    ushort/0,
    int/0,
    uint/0,
    long/0,
    ulong/0,

    predicate/1,
    modifier/1,
    modifier1/2,
    modifier2/3,
    action/1
]).

-type sbyte() :: -16#80 .. 16#7f.
-type short() :: -16#8000 .. 16#7fff.
-type ushort() :: 0 .. 16#ffff.
-type int() :: -16#80000000 .. 16#7fffffff.
-type uint() :: 0..16#ffffffff.
-type long() :: -16#8000000000000000 .. 16#7fffffffffffffff.
-type ulong() :: 0..16#ffffffffffffffff.

-type predicate(T) :: fun((T) -> boolean()).
-type modifier(T) :: fun((T) -> T).
-type modifier1(T, A1) :: fun((T, A1) -> T).
-type modifier2(T, A1, A2) :: fun((T, A1, A2) -> T).
-type action(T) :: fun((T) -> any()).

%% API

-spec start_apps(Apps) -> 'ok' when
      Apps :: [atom()].

%% @doc Ensures that all Apps has been successfully started.

start_apps(Apps) ->
    lists:foreach(
        fun(App) ->
            case (catch application:start(App)) of
                ok ->
                    ok;
                {error, {already_started, _}} ->
                    ok;
                Error ->
                    erlang:error({failed_starting, App, Error})
            end
        end, Apps).

start_app_recursive(App) ->
    List = app_deps_recursive(App),
    start_apps(lists:reverse(List)).

%% @doc Ensures that all Apps has been successfully loaded.

load_apps(Apps) ->
    lists:foreach(
        fun(App) ->
            case (catch application:load(App)) of
                ok ->
                    ok;
                {error, {already_loaded, _}} ->
                    ok;
                Error ->
                    erlang:error({failed_starting, App, Error})
            end
        end, Apps).

load_apps_recursive(App) ->
    List = aplib:app_deps_recursive(App),
    load_apps(lists:reverse(List)).

app_deps(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, App}} -> ok;
        {error, Error} -> erlang:error(Error)
    end,
    case application:get_key(App, applications) of
        {ok, Deps} -> Deps;
        undefined -> []
    end.

app_deps_recursive(App) ->
    {List, _} = app_deps_recursive(App, sets:new()),
    lists:reverse(List).

app_deps_recursive(App, AllApps) ->
    case sets:is_element(App, AllApps) of
        true -> {[], AllApps};
        false ->
            {List, NewAllApps} = lists:foldl(fun(A, {L, S}) ->
                {L1, S1} = app_deps_recursive(A, S),
                {L ++ L1, S1}
            end, {[], sets:add_element(App, AllApps)}, app_deps(App)),
            {List ++ [App], NewAllApps}
    end.

reload_config() ->
    {ok, [[ConfigFile | _]]} = init:get_argument(config),
    reload_config(ConfigFile).

reload_config(ConfigFile) ->
     {ok, [Config]} = file:consult(ConfigFile),
     Apps = [{application, A, element(2,application:get_all_key(A))}
             || {A,_,_} <- application:which_applications()],
     application_controller:change_application_data(Apps,Config).

%% Local functions
