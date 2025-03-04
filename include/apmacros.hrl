-define(yesno(Condition, YesValue, NoValue), case Condition of true -> YesValue; false -> NoValue end).
-define(doif(Condition, Statement), case Condition of true -> Statement; _ -> ignore end).
-define(ifdef(Value, Statement), case Value of undefined -> ignore; _ -> Statement end).
-define(def(Value, Default), (fun() -> case Value of undefined -> Default; __Other__ -> __Other__ end end)()).

-define(assert(Check, Exception), case (Check) of true -> ignore; false -> erlang:throw(Exception) end).
-define(verify(Check, Exception), case (Check) of true -> ignore; false -> erlang:error(Exception) end).

-ifdef(debug).
-define(ifdebug(Do), Do).
-define(ifdebug(Do, Else), Do).
-else.
-define(ifdebug(_Do), ignore).
-define(ifdebug(Do, Else), Else).
-endif.
