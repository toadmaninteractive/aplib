-module(logevent_formatter).

%% NB: the module tries to follow the standard https://erlang.org/doc/man/logger_formatter.html

%% Include files

%% Exported functions

-export([
    format/2
]).

-type config() :: #{
    chars_limit     => integer(),
    depth           => integer(),
    max_size        => integer(),
    single_line     => boolean(),
    template        => template(),
    time_designator => byte(),
    time_offset     => integer() | [byte()],
    style           =>
          'raw'             % stringified map without limits and without end of line
        | 'map'             % stringified map with limits and end of line
        | 'line'            % stringified map of data field
        | 'pretty'          % human-friendly message plus pretty-printed data field and stacktrace
}.

-type template() :: [metakey() | {metakey(), template(), template()} | string()].
-type metakey() :: field_name() | [atom()].
-type field_name() ::
          atom()            % event record map key
        | 'level'           % event log level
        | 'msg'             % event log message
        | 'caption'         % event log caption
        | 'pid'             % pid
        | 'severity'        % synonym of 'level'
        | 'datetime'        % rfc3339 timestamp of event
        | 'time'            % 'datetime' without date and timezone parts
        | 'record'          % the whole record per se
        | 'what'            % what from log report
    .

-define(nl_subst, <<4, 4, 4, 4>>).

%% API

-spec format(Event, Config) -> unicode:chardata() when
      Event     :: logger:log_event(),
      Config    :: config().

format(Event, UserConfig) ->
    % compose formatting config
    #{style := Style} = Config = maps:merge(default_config(), UserConfig),
    % compose normalized log record
    Record = convert(Style, logevent:normalize(Event), Config),
    {Format, Args} = format_template(Record, Config),
    % serialize to (possibly truncated) binary
    Result = single_line(sprintf(Format, Args, Config), Config),
    truncate(unpack_nl(Result, Config), Config).

%% Local functions

%% Formatter

-spec default_config() -> config().

default_config() ->
    #{
        chars_limit => -1,
        depth => -1,
        max_size => -1,
        single_line => false,
        style => pretty,
        template => [time, " [", level, "] ", {caption, [caption, ": "], ""}, msg, data, stacktrace, "\n"],
        time_designator => $T,
        time_offset => ""
    }.

-spec convert(Style, Record, Config) -> Record2 when
        Style   :: atom(),
        Record  :: logevent:log_record(),
        Config  :: config(),
        Record2 :: logevent:log_record().

convert(raw, Record, _Config) ->
    Record;

convert(map, #{time := Time} = Record, Config) when is_integer(Time) ->
    Record#{time => format_datetime(Time, Config)};

convert(pretty, #{data := Data0, level := Level} = Record, Config) ->
    Data = prettify(Data0, Config),
    StackTrace = maps:get(stacktrace, Data, []),
    ShouldPrintStackTrace = case logger:compare_levels(Level, info) of
        gt -> length(StackTrace) > 0;
        _ -> length(StackTrace) > 1
    end,
    {Payload, FormattedStackTrace} = case ShouldPrintStackTrace of
        true -> {maps:without([stacktrace], Data), format_stacktrace(StackTrace, Config)};
        false -> {Data, []}
    end,

    WellKnownFields = maps:with([caption, pid, what], Payload),
    Payload2 = maps:without([caption, pid, what], Payload),

    {FormattedMessage, Fields} = case maps:take(msg, Payload2) of
        error -> {"", Payload2};
        T2 -> T2
    end,
    FormattedFields = format_payload(Fields, Config),
    maps:merge(WellKnownFields, Record#{
        data => FormattedFields,
        msg => FormattedMessage,
        stacktrace => FormattedStackTrace
    });

convert(_, Record, _Config) ->
    Record.

prettify(#{label := {app, start}, app := App, node := Node}, Config) ->
    prettify(#{msg => sprintf("Application '~s' started at ~w", [App, Node], Config)}, Config);
prettify(#{label := {app, exit}, app := App, reason := Reason, type := _Type}, Config) ->
    prettify(#{msg => sprintf("Application '~s' exited with reason '~w'", [App, Reason], Config)}, Config);
prettify(#{label := {proc, start}, id := {acceptor, _, N}, pid := Pid, stacktrace := [MFA], sup := ranch_acceptors_sup}, Config) ->
    prettify(#{msg => sprintf("Supervisor 'ranch_acceptors_sup' started acceptor #~b as ~p via ~s", [N, Pid, MFA], Config)}, Config);
prettify(#{label := {proc, start}, id := {ranch_listener_sup, Name}, pid := Pid, stacktrace := [MFA], sup := ranch_sup}, Config) ->
    prettify(#{msg => sprintf("Supervisor 'ranch_sup' started listener supervisor '~s' as ~p via ~s", [Name, Pid, MFA], Config)}, Config);
prettify(#{label := {proc, start}, id := Id, pid := Pid, stacktrace := [MFA], sup := Sup}, Config) ->
    prettify(#{msg => sprintf("Supervisor '~w' started '~p' as ~p via ~s", [Sup, Id, Pid, MFA], Config)}, Config);
prettify(#{label := {proc, exit}, error := Error, pid := Pid, reason := Reason} = Data, Config) ->
    prettify(maps:merge(maps:without([error, label, pid, reason], Data), #{
        msg => sprintf("Process ~p exited with error '~256p', reason '~256p'", [Pid, Error, Reason], Config)
    }), Config);
prettify(#{format := Format, args := Args} = Data, Config) ->
    % try to format
    try sprintf(Format, Args, Config) of
        String -> prettify(maps:without([args, format], Data#{
            msg => String
        }), Config)
    % error during formatting?
    catch error:badarg ->
        % log format and arguments
        maps:without([format, args], Data#{
            attention => true,
            msg => <<"** Could not io_lib:format">>,
            original => #{args => Args, format => Format}
        })
    end;
% % TODO: FIXME: use this instead of upper clause if you do not want to interpolate the message
% prettify(#{format := Format, args := _Args} = Data, Config) ->
%     prettify(maps:without([format], Data#{
%         msg => Format
%     }), Config);
prettify(#{logger := got_unexpected_message, message := Message, process := logger}, Config) ->
    prettify(#{msg => sprintf("logger got unexpected message ~p", [Message], Config)}, Config);
prettify(#{label := {ranch, closed}, connection := Conn, reason := Reason}, Config) ->
    prettify(#{msg => sprintf("Ranch connection '~p' closed with reason '~p'", [Conn, Reason], Config)}, Config);
% FIXME: print map as a map
% prettify(#{caption := {ecouch, ECouch}, database := Database, seq_id := SeqIdPlusHash, what := What}, Config) ->
%     [SeqId | _] = binary:split(SeqIdPlusHash, <<"-">>),
%     prettify(#{msg => sprintf("ECouch '~p' made ~s on database '~s', seq_id=~s", [ECouch, What, Database, SeqId], Config)}, Config);
% prettify(#{caption := {ecouch, ECouch}, db := {db, _, Database}, what := What}, Config) ->
%     prettify(#{msg => sprintf("ECouch '~p' made ~s on database '~s'", [ECouch, What, Database], Config)}, Config);
prettify(#{} = Data, _Config) ->
    Data.

%% Helpers

-spec format_template(Fields, Config) -> {Format, Args} when
        Fields  :: map(),
        Config  :: config(),
        Format  :: string(),
        Args    :: [string()].

format_template(Fields, #{style := raw} = _Config) ->
    {"~ts", [sprintf("~0tp", [Fields], #{chars_limit => -1})]};

format_template(Fields, #{style := map} = Config) ->
    {_, Arg} = format_val(record, Fields, Config),
    {"~ts~n", [Arg]};

format_template(#{data := Data}, #{style := chronos}) ->
    {"~ts", [sprintf("~0tp", [Data], #{chars_limit => -1})]};

format_template(Fields, #{template := Template} = Config) ->
    case Template of
        [Char | _] when is_integer(Char) ->
            {"~ts", [Template]};
        Bin when is_binary(Bin) ->
            {"~ts", [Template]};
        Atom when is_atom(Atom) ->
            format_template(Fields, Config#{template => [Template]});
        List when is_list(List) ->
            {Format, Args} = lists:unzip([format_rule(Rule, Fields, Config) || Rule <- Template]),
            {lists:concat(Format), Args};
        _ ->
            {"=== bad format template: ~0tp", [Template]}
    end.

format_rule({Selector, IfExists, Else}, Fields, Config) when is_map(Fields) ->
    Template = case map_drill(Selector, Fields) of
        {_, _} ->
            IfExists;
        undefined ->
            Else
    end,
    {Format, Args} = format_template(Fields, Config#{template => Template}),
    {"~ts", [sprintf(Format, Args, Config)]};
format_rule([Key | _T] = Selector, Fields, Config) when is_atom(Key), is_map(Fields) ->
    case map_drill(Selector, Fields) of
        undefined ->
            {"~s", ""};
        {LeafKey, Val} ->
            format_val(LeafKey, Val, Config)
    end;
format_rule(Str, _Fields, _Config) when is_binary(Str); is_list(Str) ->
    {"~ts", Str};
format_rule(record, Fields, Config) ->
    format_val(record, Fields, Config);
format_rule(severity, Fields, Config) ->
    format_rule(level, Fields, Config);
format_rule(datetime, #{time := DateTime}, Config) when is_integer(DateTime) ->
    format_val(datetime, DateTime, Config);
format_rule(Key, Fields, Config) when is_atom(Key), is_map(Fields) ->
    format_rule([Key], Fields, Config);
format_rule(_, _Fields, _Config) ->
    {"~s", ""}.

format_val(time, Val, Config) when is_integer(Val) ->
    format_val(time, format_time(Val, Config), Config);
format_val(datetime, Val, Config) when is_integer(Val) ->
    format_val(datetime, format_datetime(Val, Config), Config);
format_val(_Key, Val, _Config) when is_binary(Val); is_list(Val) ->
    {"~ts", Val};
format_val(Key, Val, #{single_line := true} = Config) when is_map(Val) ->
    format_val(Key, format_map_as_line(Val, Config), Config);
format_val(Key, Val, #{depth := Depth} = Config) when is_integer(Depth) ->
    format_val(Key, sprintf("~0tP", [Val, Depth], Config), Config);
format_val(Key, Val, Config) ->
    format_val(Key, sprintf("~0tp", [Val], Config), Config).

format_stacktrace([], _Config) -> [];
format_stacktrace(StackTrace, Config) ->
    % TODO: binary:copy from config
    Padding = <<"          ">>,
    Header  = <<"~~~~~~~~~~">>,
    Trailer = <<"^^^^^^^^^^">>,
    Formatted = pad([Header, StackTrace, Trailer], Padding),
    pack_nl(Formatted, Config).

% Do not add padding elements when single_line is active
format_payload(Fields, #{single_line := true} = Config) when map_size(Fields) > 0 ->
    {_, FormattedFields} = format_val(data, Fields, Config),
    FormattedFields;

format_payload(Fields, Config) when map_size(Fields) > 0 ->
    % TODO: binary:copy from config
    Padding = <<"          ">>,
    Header  = <<"~~~~~~~~~~">>,
    Trailer = <<"^^^^^^^^^^">>,
    {_, FormattedFields} = format_val(data, Fields, Config),
    pad([Header, FormattedFields, Trailer], Padding);
format_payload(_, _Config) ->
    "".

format_time(N, Config) when is_integer(N) ->
    binary:part(format_datetime(N, Config), 11, 15).

format_datetime(N, #{time_designator := TD, time_offset := TO}) when is_integer(N) ->
    list_to_binary(calendar:system_time_to_rfc3339(N, [
        {offset, TO},
        {time_designator, TD},
        {unit, microsecond}
    ])).

format_prefix(<<>>, _Prefix) -> <<>>;
format_prefix(_, Prefix) -> Prefix.

format_map_as_line(Map, Config) ->
    format_map_as_line(maps:to_list(Map), <<>>, Config).

format_map_as_line([], Acc, _Config) -> Acc;
format_map_as_line([{Key, Val} | Tail], Acc, #{depth := Depth} = Config) when is_integer(Depth) ->
    Acc1 = sprintf("~s~s~0tP: ~0tP", [Acc, format_prefix(Acc, ", "), Key, Depth, Val, Depth], Config),
    format_map_as_line(Tail, Acc1, Config);
format_map_as_line([{Key, Val} | Tail], Acc, Config) ->
    Acc1 = sprintf("~s~s~0tp: ~0tp", [Acc, format_prefix(Acc, ", "), Key, Val], Config),
    format_map_as_line(Tail, Acc1, Config).

map_drill(Key, Map) when is_atom(Key), is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Val} ->
            {Key, Val};
        _ ->
            undefined
    end;
map_drill([Key | T], Map) when is_atom(Key), is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Map2} when length(T) > 0 ->
            map_drill(T, Map2);
        {ok, Val} ->
            {Key, Val};
        _ ->
            undefined
    end;
map_drill(_, _) ->
    undefined.

truncate(Bin, #{max_size := N}) when is_integer(N), N > 0, byte_size(Bin) > N ->
    iolist_to_binary([binary:part(Bin, 0, N - 1), binary:part(Bin, byte_size(Bin), -1)]);
truncate(Bin, _Config) ->
    Bin.

single_line(Bin, #{single_line := true}) ->
    re:replace(Bin, <<"\n\s*(?:.*\n)">>, <<", ">>, [global, {return, binary}]);
single_line(Bin, _Config) ->
    Bin.

pad(Bin, Padding) when is_binary(Bin) ->
    binary:replace(Bin, <<$\n>>, Padding, [global, {insert_replaced, 0}]);
pad(List, Padding) when is_list(List) ->
    pad(iolist_to_binary([[$\n, Line] || Line <- lists:flatten(List)]), Padding).

sprintf(Fmt, Args, #{chars_limit := CharsLimit}) ->
    list_to_binary(io_lib:format(Fmt, Args, [{chars_limit, CharsLimit}])).

pack_nl(Bin, #{single_line := true} = _Config) when is_binary(Bin) ->
    binary:replace(Bin, <<$\n>>, ?nl_subst, [global]);
pack_nl(List, #{single_line := true} = Config) when is_list(List) ->
    pack_nl(iolist_to_binary(List), Config);
pack_nl(Other, _Config) ->
    Other.

unpack_nl(Bin, #{single_line := true} = _Config) when is_binary(Bin) ->
    binary:replace(Bin, ?nl_subst, <<$\n>>, [global]);
unpack_nl(List, #{single_line := true} = Config) when is_list(List) ->
    unpack_nl(iolist_to_binary(List), Config);
unpack_nl(Other, _Config) ->
    Other.

%% Internal tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

format_time_test() ->
    ?assertEqual(
        <<"00:00:00.000000">>,
        format_time(0, #{time_designator => " ", time_offset => 0})
    ),
    ?assertEqual(
        <<"03:00:00.000000">>,
        format_time(0, #{time_designator => "T", time_offset => 10800000000})
    ),
    ok.

format_datetime_test() ->
    ?assertEqual(
        <<"1970-01-01 00:00:00.000000+00:00">>,
        format_datetime(0, #{time_designator => " ", time_offset => 0})
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00">>,
        format_datetime(0, #{time_designator => "T", time_offset => 10800000000})
    ),
    ok.

format_rule_test() ->
    Config = #{style => pretty, chars_limit => -1},
    ?assertEqual(
        {"~ts", <<"1">>},
        format_rule([a, b, c], #{a => #{b => #{c => 1}}}, Config)
    ),
    ?assertEqual(
        {"~s", []},
        format_rule([a, b, c], #{a => #{b => b}}, Config)
    ),
    ?assertEqual(
        {"~s", []},
        format_rule([a, b, d], #{a => #{b => #{c => 1}}}, Config)
    ),
    ?assertEqual(
        {"~ts", [<<"a.b.c is 1">>]},
        format_rule({[a, b, c], ["a.b.c is ", [a, b, c]], "---"}, #{a => #{b => #{c => 1}}}, Config)
    ),
    ?assertEqual(
        {"~ts", [<<"---">>]},
        format_rule({[a, b, c], ["a.b.c is ", [a, b, c]], "---"}, #{a => #{b => #{d => 1}}}, Config)
    ),
    ?assertEqual(
        {"~ts", [<<"---">>]},
        format_rule({[a, b, c], ["a.b.c is ", [a, b, c]], <<"---">>}, #{a => #{b => #{d => 1}}}, Config)
    ),
    ok.

map_drill_test() ->
    ?assertEqual(
        {a, #{b => #{c => 1}}},
        map_drill(a, #{a => #{b => #{c => 1}}})
    ),
    ?assertEqual(
        {c, 1},
        map_drill([a, b, c], #{a => #{b => #{c => 1}}})
    ),
    ?assertEqual(
        undefined,
        map_drill([a, b, c], #{a => #{b => b}})
    ),
    ?assertEqual(
        undefined,
        map_drill([a, b, d], #{a => #{b => #{c => 1}}})
    ),
    ok.

single_line_test() ->
    ?assertEqual(
        <<"a, b">>,
        single_line(<<"a\n  \nb">>, #{single_line => true})
    ),
    ?assertEqual(
        <<"a, b\n">>,
        single_line(<<"a\n  \nb\n">>, #{single_line => true})
    ),
    ok.

truncate_test() ->
    ?assertEqual(
        <<"aaaaa\n">>,
        truncate(<<"aaaaa\n">>, #{max_size => -1})
    ),
    ?assertEqual(
        <<"aaaaa\n">>,
        truncate(<<"aaaaa\n">>, #{max_size => 15})
    ),
    ?assertEqual(
        <<"aaaa\n">>,
        truncate(<<"aaaaa\n">>, #{max_size => 5})
    ),
    ?assertEqual(
        <<"aaaaa\n">>,
        truncate(<<"aaaaa\n">>, #{max_size => 0})
    ),
    ?assertEqual(
        <<"">>,
        truncate(<<"">>, #{max_size => 5})
    ),
    ok.

pad_test() ->
    ?assertEqual(
        <<"">>,
        pad([], <<"  ">>)
    ),
    ?assertEqual(
        <<"\n  a\n  b\n  c">>,
        pad([<<"a">>, <<"b">>, <<"c">>], <<"  ">>)
    ),
    ok.

pack_nl_test() ->
    ?assertEqual(
        <<"a", ?nl_subst/binary, "b">>,
        pack_nl(<<"a\nb">>, #{single_line => true})
    ),
    ?assertEqual(
        <<"a\nb">>,
        pack_nl(<<"a\nb">>, #{})
    ),
    ok.

unpack_nl_test() ->
    ?assertEqual(
        <<"a\nb">>,
        unpack_nl(<<"a", ?nl_subst/binary, "b">>, #{single_line => true})
    ),
    ?assertEqual(
        <<"a", ?nl_subst/binary, "b">>,
        unpack_nl(<<"a", ?nl_subst/binary, "b">>, #{})
    ),
    ok.

format_prefix_test() ->
    ?assertEqual(
        <<"">>,
        format_prefix(<<"">>, <<", ">>)
    ),
    ?assertEqual(
        <<", ">>,
        format_prefix(<<"a, b">>, <<", ">>)
    ),
    ok.

format_map_as_line_test() ->
    ?assertEqual(
        <<"a: 'Hello', b: true, bs: <<\"Data\">>, n: 1, s: \"World\"">>,
        format_map_as_line(#{n => 1, b => true, a => 'Hello', s => "World", bs => <<"Data">>}, #{chars_limit => -1})
    ),
    ?assertEqual(
        <<"a: 'Hello', b: true, bs: <<\"Data\">>, n: 1, s: \"World\"">>,
        format_map_as_line(#{n => 1, b => true, a => 'Hello', s => "World", bs => <<"Data">>}, #{depth => -1, chars_limit => -1})
    ),
    ok.

% TODO: more tests

-endif.
