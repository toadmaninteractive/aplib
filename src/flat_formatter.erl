-module(flat_formatter).

%% Include files

%% Exported functions

-export([
    format/2
]).

%% API

format(LogEvent, Config) ->
    #{level := Level, meta := Metadata, msg := Message} = LogEvent,
    Result = format_msg(Message, Metadata),
    create_log(Level, Result, Config).

%% Local functions

format_msg({report, ReportMap}, Metadata) when is_map(ReportMap) ->
    case maps:get(report, ReportMap, undefined) of
        undefined -> format_custom_msg(ReportMap, Metadata);
        Report -> format_report_msg(Report, Metadata)
    end;
format_msg({report, ReportList}, _Metadata) when is_list(ReportList) ->
    report_list_to_string(ReportList);
format_msg({string, Message}, Metadata) when is_binary(Message) ->
    format_msg({string, binary_to_list(Message)}, Metadata);
format_msg({string, Message}, Metadata) when is_list(Message) ->
    format_msg({"~s", [Message]}, Metadata);
format_msg({MessageFmt, Args}, #{caption := Caption} = Metadata) when is_list(MessageFmt) ->
    format_msg({"~s: " ++ MessageFmt, [to_plain_text(Caption)|Args]}, maps:without([caption], Metadata));
format_msg({MessageFmt, Args}, #{stacktrace := StackTrace} = Metadata) when is_list(MessageFmt), is_list(StackTrace) ->
    format_msg({MessageFmt ++ "~nStack trace:~n~s", Args ++ [format_stacktrace(StackTrace)]}, maps:without([stacktrace], Metadata));
format_msg({MessageFmt, Args}, _Metadata) when is_list(MessageFmt), is_list(Args) ->
    io_lib:format(MessageFmt, Args);
format_msg(Report, Metadata) ->
    io_lib:format("Unknown log: ~p, meta: ~p", [Report, Metadata]).

%% FIXME: fix pattern matching!
format_report_msg(Report, _Metadata) ->
    case Report of
        [{application, Application}, {started_at, Where}] ->
            io_lib:format("Application ~p started on node ~s", [Application, Where]);
        [{supervisor, {_, Supervisor}}, {started, [{pid, Pid}, {mfa, {M, F, A}}]}] ->
            io_lib:format("Supervisor ~p started ~p:~p/~p at pid ~p", [Supervisor, M, F, A, Pid]);
        [{supervisor, {_, Supervisor}}, {started, Started}] ->
            {M, F, Args} = proplists:get_value(mfargs, Started),
            io_lib:format("Supervisor ~p started ~p:~p/~p at pid ~p", [Supervisor, M, F, length(Args), proplists:get_value(pid, Started)]);
        [{supervisor, {_, Supervisor}}, {errorContext, _}, {reason, Reason}, {offender, _}] ->
            {What, [{M, F, A, _}, {WhereM, WhereF, WhereA, [_, {line, Line}]} | _]} = Reason,
            io_lib:format("Supervisor ~p stopped ~p with reason: ~p ~p:~p/~p~n    in function ~p:~p/~p at line ~p",
                          [Supervisor, M, What, M, F, A, WhereM, WhereF, WhereA, Line]);
        [{application, Application}, {exited, {_, {_, {_, {Reason, [{M, F, A, _}|_]}}}}}, _] ->
            io_lib:format("Application ~p exited with reason: ~p ~p:~p/~p", [Application, Reason, M, F, length(A)]);
        [{application, Application}, {exited, {{shutdown, Shutdown}, _}}, {type, _}] ->
            {What, _, {Reason, [{M, F, A, _}, {WhereM, WhereF, WhereA, [_, {line, Line}]} | _]}} = Shutdown,
            io_lib:format("Application ~p exited with reason: ~p ~p:~p/~p in function ~p:~p/~p at line ~p (~p)",
                          [Application, Reason, M, F, length(A), WhereM, WhereF, WhereA, Line, What]);
        [[{initial_call, _} | Tail], _] ->
            Pid = proplists:get_value(pid, Tail),
            ErrorInfo = proplists:get_value(error_info, Tail),
            case ErrorInfo of
                {error, Reason, Stacktrace} -> format_crash(Pid, Reason, Stacktrace);
                {exit, {{shutdown, {_, _, {Reason, Stacktrace}}}, _}, _} -> format_crash(Pid, Reason, Stacktrace);
                _ -> io_lib:format("~p", [Report]) % Unknown format (if new crash format found, add it here)
            end;
        _ -> io_lib:format("~p", [Report]) % Unknown format (if new message format found, add it here)
    end.

format_custom_msg(ReportMap, Metadata) ->
    case maps:get(mfa, Metadata, undefined) of
        {_M, _F, _A} ->
            ReportInfo = get_report_info(ReportMap),
            Fmt = case ReportInfo of
                B when is_binary(B) -> "~s";
                S when is_list(S) -> "~s";
                _ -> "~p"
            end,
            io_lib:format(Fmt, [ReportInfo]);
        undefined -> io_lib:format("~p", [ReportMap])
    end.

get_report_info(ReportMap) ->
    case maps:get(what, ReportMap, undefined) of
        undefined -> maps:get(label, ReportMap, undefined);
        Value -> Value
    end.

report_list_to_string(ReportList) ->
    [io_lib:format("~p: ~p; ", [Key, Value]) || {Key, Value} <- ReportList].

create_log(Level, Result, Config) ->
    #{format := Format} = Config,
    iolist_to_binary(set_params(Format, Level, Result)).

set_params(Format, Level, Result) ->
    [set_param(Element, Level, Result) || Element <- Format].

set_param(Element, Level, Result) ->
    case Element of
        datetime ->
            {Y, M, D, HH, MM, SS, MS} = datetime_ms(),
            io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w", [Y, M, D, HH, MM, SS, MS]);
        date ->
            {Y, M, D, _, _, _, _} = datetime_ms(),
            io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D]);
        time ->
            {_, _, _, HH, MM, SS, MS} = datetime_ms(),
            io_lib:format("~2..0w:~2..0w:~2..0w.~3..0w", [HH, MM, SS, MS]);
        severity -> io_lib:format("~p", [Level]);
        message -> Result;
        Value -> list_to_binary(Value)
    end.

format_crash(Pid, Reason, Stacktrace) ->
    StackLength = length(Stacktrace),
    Caption = io_lib:format("CRASH REPORT Process ~p exited with reason: ~p ", [Pid, Reason]),
    StackList = [format_stacktrace_item(Item, Index, StackLength) || {Item, Index} <- lists:zip(Stacktrace, lists:seq(1, StackLength))],
    Caption ++ StackList.

format_stacktrace(Stracktrace) ->
    StackLength = length(Stracktrace),
    [format_stacktrace_item(Item, Index, StackLength) || {Item, Index} <- lists:zip(Stracktrace, lists:seq(1, StackLength))].

format_stacktrace_item(StacktraceItem, Index, StackLength) ->
    {M, F, A, L} = StacktraceItem,
    Arity = case A of
        Arguments when is_list(Arguments) -> length(Arguments);
        Integer -> Integer
    end,
    Line = proplists:get_value(line, L),
    case Index of
        1 -> io_lib:format("~p:~p/~p at line ~p~n", [M, F, Arity, Line]);
        StackLength -> io_lib:format("    in function ~p:~p/~p at line ~p", [M, F, Arity, Line]);
        _ -> io_lib:format("    in function ~p:~p/~p at line ~p~n", [M, F, Arity, Line])
    end.

to_plain_text(Binary) when is_binary(Binary) -> Binary;
to_plain_text(String) when is_list(String) -> String;
to_plain_text(Other) -> io_lib:format("~p", [Other]).

datetime_ms() ->
    {{Y, M, D}, {H, MM, S}} = calendar:local_time(),
    {Y, M, D, H, MM, S, os:system_time(millisecond) rem 1000}.
