-module(aplog).

%% Include files

%% Exported functions

-export([
    append/2,
    append/3
]).

%% API

-spec append(FileName, Format) -> 'ok' when
      FileName :: file:filename(),
      Format :: io:format().

append(FileName, Format) ->
    append(FileName, Format, []).

-spec append(FileName, Format, Args) -> 'ok' when
      FileName :: file:filename(),
      Format :: io:format(),
      Args :: [term()].

append(FileName, Format, Args) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, Format, Args),
    file:close(File),
    'ok'.

%% Local functions
