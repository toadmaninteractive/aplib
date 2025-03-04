-module(iso8601).

%% Include files

%% Exported functions

-export([
    format_datetime_zone/2,
    format_datetime/1,
    format_date/1,
    format_time/1,
    format_zone/1,

    parse_datetime/1,
    parse_date/1,
    parse_time/1,
    parse_timems/1,
    parse_datetimems/1,
    parse_datetimems_zone/1
]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-type datetime() :: calendar:datetime().
-type date() :: calendar:date().
-type time() :: calendar:time().
-type zone() :: {integer(), minute()}.
-type timems():: {hour(), minute(), float()}.
-type datetimems() :: {calendar:date(), timems()}.

-record(state, {
    year = 0    :: non_neg_integer(),
    month = 1   :: 1..12,
    day = 1     :: 1..31,
    hour = 0    :: 0..23,
    minute = 0  :: 0..59,
    second = 0  :: 0..59,
    milisecond = 0 :: non_neg_integer(),
    zone = {0, 0} :: zone(),
    separator = undefined :: boolean() | 'undefined'
}).

-define(date_fmt, "~4.10.0B-~2.10.0B-~2.10.0B").
-define(time_fmt, "~2.10.0B:~2.10.0B:~2.10.0B").

%% API

-spec format_time(time() | timems()) -> binary().

format_time({H, Mn, S}) when is_integer(S) ->
    IsoStr = io_lib:format(?time_fmt, [H, Mn, S]),
    list_to_binary(IsoStr);
format_time({H, Mn, Sf}) when is_float(Sf) ->
    S = trunc(Sf),
    Ms = trunc(Sf * 1000) rem 1000,
    IsoStr = io_lib:format(?time_fmt".~3.10.0B", [H, Mn, S, Ms]),
    list_to_binary(IsoStr).

-spec format_date(date()) -> binary().

format_date({Y, Mo, D}) ->
    IsoStr = io_lib:format(?date_fmt, [Y, Mo, D]),
    list_to_binary(IsoStr).

format_zone(utc) -> <<"Z">>;
format_zone({0, 0}) -> <<"Z">>;
format_zone({HH, 0}) ->
    IsoStr = io_lib:format("~c~2.10.0B", [int_to_sign(HH), nosign(HH)]),
    list_to_binary(IsoStr);
format_zone({HH, MM}) ->
    IsoStr = io_lib:format("~c~2.10.0B:~2.10.0B", [int_to_sign(HH), nosign(HH), MM]),
    list_to_binary(IsoStr).

-spec format_datetime_zone(datetime() | datetimems(), zone() | utc) -> binary().

format_datetime_zone({Date, Time}, Zone) ->
    B1 = format_date(Date),
    B2 = format_time(Time),
    B3 = format_zone(Zone),
    <<B1/binary, "T", B2/binary, B3/binary>>.

-spec format_datetime(datetime() | datetimems()) -> binary().

format_datetime({Date, Time}) ->
    B1 = format_date(Date),
    B2 = format_time(Time),
    <<B1/binary, "T", B2/binary>>.

-spec parse_datetime(binary()) -> datetime().

parse_datetime(String) ->
    State = year(String, #state{}),
    to_datetime(State).

-spec parse_date(binary()) -> date().

parse_date(String) ->
    State = year(String, #state{}),
    to_date(State).

-spec parse_time(binary()) -> time().

parse_time(String) ->
    State = hour(String, #state{}),
    to_time(State).

-spec parse_timems(binary()) -> timems().

parse_timems(String) ->
    State = hour(String, #state{}),
    to_timems(State).

-spec parse_datetimems(binary()) -> datetimems().

parse_datetimems(String) ->
    State = year(String, #state{}),
    to_datetimems(State).

-spec parse_datetimems_zone(binary()) -> {datetimems(), zone()}.

parse_datetimems_zone(String) ->
    State = year(String, #state{}),
    {to_datetimems(State), State#state.zone}.

%% Local functions

to_datetime(State) -> {to_date(State), to_time(State)}.
to_datetimems(State) -> {to_date(State), to_timems(State)}.
to_date(#state{year = Y, month = M, day = D}) -> {Y, M, D}.
to_time(#state{hour = H, minute = M, second = S}) -> {H, M, S}.
to_timems(#state{hour = H, minute = M, second = S, milisecond = MS}) -> {H, M, S + MS * 0.001}.

make_int([], _Power, Acc) ->
    Acc;
make_int([A|Tail], Power, Acc) when $0 =< A, A =< $9 ->
    make_int(Tail, Power * 10, Acc + Power * (A - $0));
make_int(_A, _, _) ->
    error(badarg).

year(<<Y1, Y2, Y3, Y4, Rest/binary>>, Acc) ->
    Year = make_int([Y4, Y3, Y2, Y1], 1, 0),
    month(Rest, Acc#state{year = Year});
year(_, _) ->
    error(badarg).

month(<<$-, M1, M2, Rest/binary>>, Acc) ->
    Month = make_int([M2, M1], 1, 0),
    day(Rest, Acc#state{month = Month, separator = true});
month(<<M1, M2, Rest/binary>>, Acc) ->
    Month = make_int([M2, M1], 1, 0),
    day(Rest, Acc#state{month = Month, separator = false});
month(_, _) ->
    error(badarg).

day(<<$-, D1, D2, Rest/binary>>, Acc) when Acc#state.separator =:= true ->
    Day = make_int([D2, D1], 1, 0),
    hour_join(Rest, Acc#state{day = Day});
day(<<D1, D2, Rest/binary>>, Acc) when Acc#state.separator =:= false ->
    Day = make_int([D2, D1], 1, 0),
    hour_join(Rest, Acc#state{day = Day});
day(<<>>, Acc) ->
    Acc;
day(_, _) ->
    error(badarg).

hour_join(<<$T, Rest/binary>>, Acc) ->
    hour(Rest, Acc);
hour_join(Rest, Acc) ->
    rest(Rest, Acc).

hour(<<H1, H2, Rest/binary>>, Acc) ->
    Hour = make_int([H2, H1], 1, 0),
    minute(Rest, Acc#state{hour = Hour});
hour(_, _) ->
    error(badarg).

minute(<<$:, M1, M2, Rest/binary>>, Acc) when Acc#state.separator =/= false ->
    Minute = make_int([M2, M1], 1, 0),
    second(Rest, Acc#state{minute = Minute, separator = true});
minute(<<M1, M2, Rest/binary>>, Acc) when Acc#state.separator =/= true ->
    Minute = make_int([M2, M1], 1, 0),
    second(Rest, Acc#state{minute = Minute, separator = false});
minute(_, _) ->
    error(badarg).

second(<<$:, S1, S2, Rest/binary>>, Acc) when Acc#state.separator =:= true ->
    Second = make_int([S2, S1], 1, 0),
    milisecond(Rest, Acc#state{second = Second});
second(<<S1, S2, Rest/binary>>, Acc) when Acc#state.separator =:= false ->
    Second = make_int([S2, S1], 1, 0),
    rest(Rest, Acc#state{second = Second});
second(<<>>, Acc) ->
    Acc;
second(_, _) ->
    error(badarg).

milisecond(<<$., Rest/binary>>, Acc) ->
    parse_millisecond(Rest, Acc, [], 1);
milisecond(Rest, Acc) ->
    rest(Rest, Acc).

parse_millisecond(<<X, Rest/binary>>, Acc, Ms, Mod) when X >= $0, X =< $9 ->
    parse_millisecond(Rest, Acc, [X|Ms], Mod * 10);
parse_millisecond(Rest, Acc, Ms, Mod) ->
    MilliSecond = 1000 * make_int(Ms, 1, 0) div Mod,
    rest(Rest, Acc#state{milisecond = MilliSecond}).

rest(<<Sign, H1, H2, $:, M1, M2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#state{zone = make_zone(Sign, H1, H2, M1, M2)};
rest(<<Sign, H1, H2, M1, M2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#state{zone = make_zone(Sign, H1, H2, M1, M2)};
rest(<<Sign, H1, H2>>, Acc) when (Sign =:= $+) or (Sign =:= $-) ->
    Acc#state{zone = make_zone(Sign, H1, H2, $0, $0)};
rest(<<$Z>>, Acc) ->
    Acc#state{zone = {0, 0}};
rest(<<>>, Acc) ->
    Acc;
rest(_Str, _Acc) ->
    error(badarg).

make_zone(Sign, H1, H2, M1, M2) ->
    HH = make_int([H2, H1], 1, 0),
    MM = make_int([M2, M1], 1, 0),
    {sign_to_int(Sign)*HH, MM}.

nosign(A) when A < 0 -> -A;
nosign(A) -> A.

sign_to_int($+) -> 1;
sign_to_int($-) -> -1.

int_to_sign(A) when A < 0 -> $-;
int_to_sign(_A) -> $+.
