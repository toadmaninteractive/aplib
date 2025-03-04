-module(time).

%% @doc Works with Erlang time
%% @doc shift_date from https://github.com/dweldon/edate

%% To find date & time use calendar:universal_time()
%% To find unix timestamp use time:seconds()

%% Include files

%% Exported functions

-export([
    milliseconds/0,
    milliseconds/1,
    seconds/0,
    seconds/1,
    minutes/0,
    minutes/1,
    days/0,
    time/0,
    time/1,
    datetime_to_seconds/1,
    seconds_to_datetime/1,
    time_to_week_end/0,
    time_to_day_end/0,
    time_to_month_end/0,
    shift_date/3,
    find_valid_date/1
]).

-export_type([
    time/0,
    span/0,
    milliseconds/0,
    seconds/0,
    minutes/0,
    days/0
]).

-type seconds() :: integer().
-type milliseconds() :: integer().
-type minutes() :: integer().
-type days() :: integer().
-type time() :: number().
-type span() :: number().

%% API

-spec milliseconds() -> milliseconds().

milliseconds() ->
    milliseconds(os:timestamp()).

-spec milliseconds(Now) -> milliseconds() when
      Now :: erlang:timestamp().

milliseconds(Now) ->
    {MegaSecs, Secs, MicroSecs} = Now,
    (MicroSecs div 1000) + Secs*1000 + MegaSecs*1000000000.

-spec seconds() -> seconds().

seconds() ->
    seconds(os:timestamp()).

-spec seconds(Now) -> seconds() when
      Now :: erlang:timestamp().

seconds(Now) ->
    {MegaSecs, Secs, _MicroSecs} = Now,
    MegaSecs*1000000+Secs.

-spec minutes() -> minutes().

minutes() ->
    minutes(os:timestamp()).

-spec minutes(Now) -> minutes() when
      Now :: erlang:timestamp().

minutes(Now) ->
    seconds(Now) div 60.

-spec days() -> days().

%% @doc This function computes the number of gregorian days starting
%% with year 0 and ending UTC today.

days() ->
    {Date, _Time} = calendar:universal_time(),
    calendar:date_to_gregorian_days(Date).

-spec time() -> float().

time() ->
    time(os:timestamp()).

-spec time(Now) -> float() when
      Now :: erlang:timestamp().

time(Now) ->
    {MegaSecs, Secs, MicroSecs} = Now,
    MegaSecs*1000000 + Secs + MicroSecs/1000000.

-spec datetime_to_seconds(DateTime) -> seconds() when
      DateTime :: calendar:datetime().

%% @doc Converts Erlang date_time() term to Unix timestamp (number seconds passed from 0:0:0 1.1.1970)

datetime_to_seconds(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

-spec seconds_to_datetime(Seconds) -> calendar:datetime() when
      Seconds :: seconds().

%% @doc Converts Unix timestamp (number seconds passed from 0:0:0 1.1.1970) to Erlang date_time() term

seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})).

-spec time_to_week_end() -> Seconds :: seconds().

time_to_week_end() ->
    {Date, Time} = calendar:universal_time(),
    Day = calendar:day_of_the_week(Date),
    TimeSeconds = calendar:time_to_seconds(Time),
    (8 - Day) * 3600 * 24 - TimeSeconds.

-spec time_to_month_end() -> Seconds :: seconds().

time_to_month_end() ->
    {Date, Time} = calendar:universal_time(),
    {Year, Month, Day} = Date,
    LastDay = calendar:last_day_of_the_month(Year, Month),
    TimeSeconds = calendar:time_to_seconds(Time),
    (LastDay - Day + 1) * 3600 * 24 - TimeSeconds.

-spec time_to_day_end() -> Seconds :: seconds().

time_to_day_end() ->
    {_Date, Time} = calendar:universal_time(),
    TimeSeconds = calendar:time_to_seconds(Time),
    3600 * 24 - TimeSeconds.

-spec shift_date(calendar:date(), integer(), Period) -> calendar:date() when
    Period :: day | week | month | year.

%% @doc Returns a new date after shifting `Date' by `N' periods.
%% > time:shift_date({2000,1,1}, -2, day).
%% {1999,12,30}
%% > time:shift_date({2010,2,27}, 1, week).
%% {2010,3,6}

shift_date(Date, N, day) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + N);
shift_date(Date, N, week) ->
    shift_date(Date, 7 * N, day);
shift_date(Date, N, year) ->
    shift_date(Date, 12 * N, month);
shift_date({Y, M, D}, N, month) ->
    %% in order for the modular arithmetic to work, months in this function range
    %% from 0 to 11 (January to December)
    TotalMonths = 12 * Y + M - 1 + N,
    case TotalMonths >= 0 of
        true ->
            Month = TotalMonths rem 12,
            Year = (TotalMonths - Month) div 12,
            %% add one back to the month to fix our mod 12 shenanigans
            find_valid_date({Year, Month + 1, D});
        false ->
            error(out_of_bounds)
    end.

-spec find_valid_date(calendar:date()) -> calendar:date().

%% @doc Returns `Date' if valid. Otherwise, backward searches for a valid date.

find_valid_date({Y, M, D} = Date) ->
    case calendar:valid_date(Date) of
        true ->
            Date;
        false when D > 0->
            find_valid_date({Y, M, D - 1});
        false ->
            error(out_of_bounds)
    end.

%% Local functions
