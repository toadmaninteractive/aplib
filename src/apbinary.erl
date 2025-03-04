-module(apbinary).

%% Include files

%% Exported functions

-export([
    join/2
]).

%% API

-spec join(Separator, ListOfBinaries) -> Result when
    Separator :: binary(),
    ListOfBinaries :: [binary()],
    Result :: binary().

join(Separator, ListOfBinaries) ->
    iolist_to_binary(lists:join(Separator, ListOfBinaries)).

%% Local functions
