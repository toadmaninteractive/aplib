-module(logevent).

%% Include files

%% Exported functions

-export([
    normalize/1
]).

-type log_record() :: #{
    data    := log_data(),
    level   := logger:level(),
    time    := binary()
}.

-type log_data() :: #{
    atom()  := term()
}.

-export_type([
    log_data/0,
    log_record/0
]).

%% API

-spec normalize(LogEvent) -> Record when
      LogEvent  :: logger:log_event(),
      Record    :: log_record().

% error_logger report? -> delegate to formatted payload event
normalize(#{msg := {report, #{args := Args, format := Format, label := _Label}}} = Event) when is_list(Args) ->
    normalize_error_logger_report(Event, Format, Args);

% OTP error info?
normalize(#{msg := {report, #{client_info := _ClientInfo} = Report}} = Event) ->
    Msg = on_otp_error(Report, #{}),
    normalize(Event#{level => error, msg => {report, Msg}});

% SASL report
% % NB: delegate to module-defined report_cb formatter?
% % TODO: validate the need
% normalize(#{meta := #{report_cb := Formatter} = Meta, msg := {report, Report}}) ->
%     Msg = iolist_to_binary(Formatter(Report, #{single_line => true})),
%     normalize(Event#{msg => {report, #{formatted => Msg}}});
normalize(#{msg := {report, #{report := Report}}} = Event) ->
    % NB: may override level based on report sense
    {Level, Msg} = on_sasl_report(Report),
    normalize(Event#{level => Level, msg => {report, Msg}});

% "normalized" map payload event -> format payload
normalize(#{level := Level, meta := Meta, msg := {report, Report}}) when is_map(Report) ->
    on_report(Meta#{level => Level}, Report);

% proplist payload event -> convert to map payload event
normalize(#{msg := {report, [{_, _} | _] = PropList}} = Event) ->
    normalize(Event#{msg => {report, maps:from_list(PropList)}});

% bare string payload event -> delegate to map payload event
normalize(#{msg := {string, String}} = Event) ->
    normalize(Event#{msg => {report, #{msg => sprintf("~ts", [String])}}});

% % formatted payload event. special case?
% % NB: example of how to beautify particular essential events
% % TODO: validate the need
% normalize(#{meta := Meta, msg := {"Error in process ~p on node ~p with exit value:~n~p~n", [Pid, Node, {RawReason, StackTrace}]}}) ->
%     {Label, Reason} = case RawReason of
%         {nocatch, TheReason} -> {throw, TheReason};
%         _ -> {{error}, RawReason}
%     end,
%     normalize(Meta#{level => error, msg => {report, #{
%         label => Label,
%         node => Node,
%         pid => Pid,
%         reason => Reason,
%         stacktrace => format_stacktrace(StackTrace)
%     }}});

% formatted payload event
normalize(#{msg := {Format, Args}} = Event) when is_list(Format), is_list(Args) ->
    normalize_error_logger_report(Event, Format, Args).

%% Local functions

%% Normalize well-known libraries events

normalize_error_logger_report(#{meta := Meta},
        "Ranch listener ~p, connection process ~p, stream ~p "
        "had its request process ~p exit with reason "
        "~999999p and stacktrace ~999999p~n",
        [Ref, _Self, StreamID, Pid, Reason, StackTrace]) ->
    on_report(Meta#{
        level => error,
        stacktrace => StackTrace
    }, #{
        label => {ranch, closed},
        connection => {Ref, StreamID, Pid},
        reason => Reason
    });
normalize_error_logger_report(Event, Format, Args) ->
    normalize(Event#{msg => {report, #{
        args => Args,
        format => sprintf("~ts", [Format])
        % NB: no label key provided
    }}}).

%% Process normalized event

%% Honor optional event context
on_report(#{ctx := Ctx} = Meta, Msg) when is_map(Ctx) ->
    on_report(maps:without([ctx], Meta), maps:merge(Ctx, Msg));
on_report(#{caption := Caption} = Meta, Msg) ->
    on_report(maps:without([caption], Meta), Msg#{caption => Caption});
on_report(#{pid := _} = Meta, #{pid := _} = Msg) ->
    on_report(maps:without([pid], Meta), Msg);
on_report(#{pid := Pid} = Meta, Msg) ->
    on_report(maps:without([pid], Meta), Msg#{pid => Pid});
on_report(#{stacktrace := StackTrace} = Meta, Msg) when is_list(StackTrace) ->
    on_report(maps:without([stacktrace], Meta), Msg#{stacktrace => format_stacktrace(StackTrace)});

%% Return normalized form
on_report(#{level := Level, time := Time} = Meta, Msg) ->
    % omit standard metadata fields (and level)
    % https://erlang.org/doc/man/logger.html#type-metadata
    UserMeta = maps:without([pid, gl, time, mfa, file, line, domain, error_logger, report_cb, level], Meta),
    % concatenate metadata and msg fields for better templated format support
    maps:merge(Meta, Msg#{
        data => maps:merge(Msg, UserMeta),
        level => Level,
        time => Time
    }).

%% Format SASL report
%% https://erlang.org/doc/apps/sasl/error_logging.html

%% Crash report
%% NB: does depend on report proplist key order
on_sasl_report([[
        {initial_call, {M, F, A}},
        {pid, Pid},
        {registered_name, _},
        {error_info, {Class, Reason, StackTrace}} | _], _]) ->
    Proc = #{
        error => Class,
        pid => Pid,
        stacktrace => format_stacktrace(StackTrace) ++ [format_function(M, F, A)]
    },
    {error, parse_reason(Reason, Proc#{label => {proc, exit}})};

%% Progress report: application started
on_sasl_report([{application, App}, {started_at, Node}]) ->
    {info, #{label => {app, start}, app => App, node => Node}};

%% Progress report: application exited
on_sasl_report([{application, App}, {exited, Reason}, {type, Type}]) ->
    {Level, Proc} = on_sasl_app_exited(Reason),
    {Level, Proc#{label => {app, exit}, app => App, type => Type}};

%% Progress report: supervisor starts or restarts a child
on_sasl_report([{supervisor, {_, Sup}}, {started, ChildSpec}]) ->
    Proc = format_child_spec(ChildSpec, Sup),
    {info, Proc#{label => {proc, start}}};

%% Progress report: supervised child terminates unexpectedly
on_sasl_report([{supervisor, {_, Sup}}, {errorContext, Error}, {reason, Reason}, {offender, ChildSpec}]) ->
    Proc = format_child_spec(ChildSpec, Sup),
    {error, parse_reason(Reason, Proc#{label => {proc, exit}, error => Error})}.

%% Format SASL report cases

on_sasl_app_exited(stopped) ->
    {notice, #{reason => stopped}};
on_sasl_app_exited(shutdown) ->
    {notice, #{reason => shutdown}};
on_sasl_app_exited({{shutdown, {Reason, _, _}}, {M, F, A}}) ->
    {error, parse_reason(Reason, #{stacktrace => [format_function(M, F, A)]})};
on_sasl_app_exited(Reason) ->
    % NB: alert to notify of format discrepancy!
    {alert, parse_reason(Reason, #{})}.

%% Format OTP errors

on_otp_error(#{label := {gen_statem, terminate}, reason := {Class, Reason, StackTrace}, state := State} = Report, Acc) ->
    on_otp_error(Report#{error => Class, reason => Reason, stacktrace => format_stacktrace(StackTrace)}, Acc#{state => State});
on_otp_error(#{last_message := LastMessage} = Report, Acc) ->
    on_otp_error(maps:without([last_message], Report), Acc#{last_message => LastMessage});
on_otp_error(#{client_info := ClientInfo, label := Label, name := Name, reason := Reason} = _Report, Acc) ->
    on_otp_error_client_info(ClientInfo, parse_reason(Reason, Acc#{label => Label, id => Name})).

on_otp_error_client_info({Pid, {Pid, StackTrace}}, Acc) ->
    Acc#{client => #{pid => Pid, stacktrace => format_stacktrace(StackTrace)}};
on_otp_error_client_info({Pid, {Id, StackTrace}}, Acc) ->
    Acc#{client => #{id => Id, pid => Pid, stacktrace => format_stacktrace(StackTrace)}};
on_otp_error_client_info({Pid, State}, Acc) when is_atom(State) ->
    Acc#{client => #{pid => Pid, state => State}};
on_otp_error_client_info(undefined, Acc) ->
    Acc.

%% Parse helpers

% TODO: elaborate
parse_reason({badmatch, A}, Acc) ->
    Acc#{args => A, reason => badmatch};
parse_reason({shutdown, {Reason, Id, Cause}}, Acc) ->
    Acc#{cause => Cause, id => Id, reason => Reason};
parse_reason({already_started, _Pid} = Reason, Acc) ->
    Acc#{reason => Reason};
parse_reason({error, {not_started, _App} = Reason}, Acc) ->
    Acc#{reason => Reason};
parse_reason({listen_error, _Id, eaddrinuse} = Reason, Acc) ->
    Acc#{reason => Reason};
parse_reason({Reason, {_M, _F, A}}, Acc) when is_list(A) ->
    parse_reason(Reason, Acc#{args => A});
parse_reason({assert, [_, _, {expression, Expr}, {expected, Expected}, {value, Actual}]}, Acc) ->
    Acc#{reason => {assert, Expr, Expected, Actual}};
parse_reason({Reason, StackTrace}, Acc) when is_list(StackTrace) ->
    parse_reason(Reason, Acc#{stacktrace => format_stacktrace(StackTrace)});

parse_reason(Reason, Acc) when is_atom(Reason) ->
    Acc#{reason => Reason};
parse_reason(Reason, Acc) ->
    % NB: set attention flag to notify of unparsed reason!
    Acc#{attention => true, reason => Reason}.

%% Format helpers

format_child_spec([{pid, Pid}, {mfa, MFA}], Sup) ->
    format_child_spec(Pid, unknown, MFA, Sup);
format_child_spec([{pid, Pid}, {id, Id}, {mfargs, MFA} | _], Sup) ->
    format_child_spec(Pid, Id, MFA, Sup);
format_child_spec([{nb_children, _}, {id, Id}, {mfargs, MFA} | _], Sup) ->
    format_child_spec(unknown, Id, MFA, Sup);
format_child_spec([{pid, Pid}, {mod, _Mod}], Sup) ->
    format_child_spec(Pid, unknown, undefined, Sup).

format_child_spec(Pid, Id, {M, F, A}, Sup) ->
    format_child_spec(Pid, Id, format_function(M, F, A), Sup);
format_child_spec(Pid, Id, undefined, Sup) ->
    #{id => Id, pid => Pid, sup => Sup};
format_child_spec(Pid, Id, Mod, Sup) ->
    #{id => Id, pid => Pid, stacktrace => [Mod], sup => Sup}.

format_stacktrace([]) -> [];
format_stacktrace([{M, F, A, [{file, File}, {line, Line}]} | T]) ->
    [format_location(M, F, A, File, Line) | format_stacktrace(T)];
format_stacktrace([{M, F, A, L} | T]) when is_list(L) ->
    [format_location(M, F, A) | format_stacktrace(T)].

format_location(M, F, A) ->
    sprintf("~s", [format_function(M, F, A)]).
format_location(M, F, A, File, Line) ->
    sprintf("~s at ~s:~w", [format_function(M, F, A), File, Line]).

format_function(M, F, A) when is_list(A) ->
    format_function(M, F, length(A));
format_function(M, F, A) ->
    sprintf("~s:~s/~w", [M, F, A]).

sprintf(Fmt, Args) -> list_to_binary(io_lib:format(Fmt, Args)).

%% Internal tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_function_test() ->
    ?assertEqual(
        <<"m:f/2">>,
        format_function(m, f, 2)
    ),
    ?assertEqual(
        <<"m:f/3">>,
        format_function(m, f, [a, b, c])
    ),
    ?assertEqual(
        <<"inet_gethost_native:init/1">>,
        format_function(inet_gethost_native, init, [[]])
    ),
    ok.

format_location_test() ->
    ?assertEqual(
        <<"m:f/2">>,
        format_location(m, f, 2)
    ),
    ?assertEqual(
        <<"m:f/2 at ogogo.erl:123">>,
        format_location(m, f, 2, "ogogo.erl", 123)
    ),
    ok.

format_stacktrace_test() ->
    ?assertEqual(
        [],
        format_stacktrace([])
    ),
    ?assertEqual(
        [<<"m1:f1/2">>],
        format_stacktrace([{m1, f1, 2, []}])
    ),
    ?assertEqual(
        [<<"m1:f1/2 at file1.erl:123">>],
        format_stacktrace([{m1, f1, 2, [{file, "file1.erl"}, {line, 123}]}])
    ),
    ?assertEqual(
        [<<"m2:f2/3 at file2.erl:321">>, <<"m1:f1/2 at file1.erl:123">>],
        format_stacktrace([
            {m2, f2, [a,b,c], [{file, "file2.erl"}, {line, 321}]},
            {m1, f1, 2, [{file, "file1.erl"}, {line, 123}]}
        ])
    ),
    ok.

format_child_spec_test() ->
    ?assertEqual(
        #{id => unknown, pid => self(), stacktrace => [<<"m:f/1">>], sup => sup},
        format_child_spec([{pid, self()}, {mfa, {m, f, 1}}], sup)
    ),
    ?assertEqual(
        #{id => id, pid => self(), stacktrace => [<<"m:f/1">>], sup => sup},
        format_child_spec([{pid, self()}, {id, id}, {mfargs, {m, f, 1}}], sup)
    ),
    ?assertEqual(
        #{id => id, pid => unknown, stacktrace => [<<"m:f/1">>], sup => sup},
        format_child_spec([{nb_children, 2}, {id, id}, {mfargs, {m, f, 1}}], sup)
    ),
    ?assertEqual(
        #{id => unknown, pid => self(), sup => sup},
        format_child_spec([{pid, self()}, {mod, mod1}], sup)
    ),
    ok.

on_otp_error_client_info_test() ->
    ?assertEqual(
        #{client => #{pid => self(), stacktrace => [<<"m2:f2/3 at file2.erl:321">>, <<"m1:f1/2 at file1.erl:123">>]}},
        on_otp_error_client_info({self(), {self(), [
            {m2, f2, [a,b,c], [{file, "file2.erl"}, {line, 321}]},
            {m1, f1, 2, [{file, "file1.erl"}, {line, 123}]}
        ]}}, #{})
    ),
    ?assertEqual(
        #{client => #{id => cid, pid => self(), stacktrace => [<<"m2:f2/3 at file2.erl:321">>, <<"m1:f1/2 at file1.erl:123">>]}},
        on_otp_error_client_info({self(), {cid, [
            {m2, f2, [a,b,c], [{file, "file2.erl"}, {line, 321}]},
            {m1, f1, 2, [{file, "file1.erl"}, {line, 123}]}
        ]}}, #{})
    ),
    ?assertEqual(
        #{client => #{pid => self(), state => dead}},
        on_otp_error_client_info({self(), dead}, #{})
    ),
    ?assertEqual(
        #{client => #{pid => self(), state => remote}},
        on_otp_error_client_info({self(), remote}, #{})
    ),
    ok.

-endif.
