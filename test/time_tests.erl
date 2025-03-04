-module(time_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

-import(time, [
    shift_date/3
]).

%% API

shift_test() ->
    {Date, _} = calendar:universal_time(),
    [?assertEqual(Date, shift_date(Date, 0, day)),
     ?assertEqual(Date, shift_date(Date, 0, day)),
     ?assertEqual(Date, shift_date(Date, 0, month)),
     ?assertEqual(Date, shift_date(Date, 0, year)),
     %% simple day addition
     ?assertEqual({2000,1,2}, shift_date({2000,1,1}, 1, day)),
     ?assertEqual({2000,1,3}, shift_date({2000,1,1}, 2, day)),
     %% simple week addition
     ?assertEqual({2000,1,8}, shift_date({2000,1,1}, 1, week)),
     ?assertEqual({2000,1,15}, shift_date({2000,1,1}, 2, week)),
     %% simple month addition
     ?assertEqual({2000,2,1}, shift_date({2000,1,1}, 1, month)),
     ?assertEqual({2000,3,1}, shift_date({2000,1,1}, 2, month)),
     %% simple year addition
     ?assertEqual({2003,1,1}, shift_date({2000,1,1}, 3, year)),
     %% simple year subtraction
     ?assertEqual({1997,1,1}, shift_date({2000,1,1}, -3, year)),
     %% day subtraction at year boundary
     ?assertEqual({1999,12,31}, shift_date({2000,1,1}, -1, day)),
     ?assertEqual({1999,12,30}, shift_date({2000,1,1}, -2, day)),
     %% week subtraction at year boundary
     ?assertEqual({1999,12,25}, shift_date({2000,1,1}, -1, week)),
     ?assertEqual({1999,12,18}, shift_date({2000,1,1}, -2, week)),
     %% month subtraction at year boundary
     ?assertEqual({1999,12,1}, shift_date({2000,1,1}, -1, month)),
     ?assertEqual({1999,11,1}, shift_date({2000,1,1}, -2, month)),
     %% month subtraction before year 0
     ?assertError(out_of_bounds, shift_date({0, 1, 1}, -1, month)),
     %% 1 year = 12 month = 365 day (in a non-leap year)
     ?assertEqual(shift_date({2001,5,10}, 1, year), shift_date({2001,5,10}, 12, month)),
     ?assertEqual(shift_date({2001,5,10}, 1, year), shift_date({2001,5,10}, 365, day)),
     %% date rounding from month addition and subtraction
     ?assertEqual({2001,2,28}, shift_date({2001,1,31}, 1, month)),
     ?assertEqual({2001,2,28}, shift_date({2001,3,31}, -1, month)),
     %% leap year
     ?assertEqual({2012,2,29}, shift_date({2012,1,31}, 1, month)),
     ?assertEqual({2012,2,29}, shift_date({2012,4,30}, -2, month)),
     ?assertEqual({2013,2,28}, shift_date({2012,2,29}, 1, year))].

%% Local functions
