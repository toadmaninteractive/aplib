-module(remote_parse_transform).

%% Include files

%% Exported functions

-export([
    parse_transform/2
]).

-define(ATOM(Line,T), {atom, Line, T}).
-define(VAR(Line,V), {var, Line, V}).
-define(CALL(Line,F,A), {call, Line, ?ATOM(Line, F), A}).
-define(APPLY(Line,M,F,A), {call, Line, {remote, Line, ?ATOM(Line, M), ?ATOM(Line, F)}, A}).
-define(FUNCTION(Line,Name,Args,Body), {function, Line, Name, length(Args), [{clause, Line, Args, [], Body}]}).
-define(FUN(Line,Args,Body), {'fun', Line, {clauses, [{clause, Line, Args, [], Body}]}}).
-define(EXPORT(Line,N,A), {attribute, Line, export, [{N, A}]}).

-record(func, {
    attribute,
    wrap_module,
    wrap_name,
    name,
    arity,
    line,
    args
}).

%% API

parse_transform(Forms, _Options) ->
    try
        implement_modify(Forms)
    catch E:R:Stacktrace ->
        io:format("Remote transform error: ~p:~p~n~p~n", [E, R, Stacktrace])
    end.

implement_modify(Forms) ->
    case [ #func{attribute = Attr, wrap_module = MMod, wrap_name = MName, name = N, arity = A, line = Line, args = Args}
                ||  {attribute, AttrLine, Attr, {MMod,MName,Funs}} <- Forms,
                    Attr =:= remote orelse Attr =:= remote_singleton,
                    {N,A} <- Funs,
                    {Line, Args} <- [find_line_args(Forms, N, A, AttrLine)] ] of
        [] ->
            Forms;
        Ms ->
            {H,T} = lists:splitwith(fun is_head/1, Forms),
            H ++ a_exports(Ms) ++ f_wrappers(Ms) ++ T
    end.

%% Local functions

is_head(T) ->
    not lists:member(element(1,T), [function, eof]).

wrapper_name(FunName) ->
    list_to_atom(atom_to_list(FunName) ++ "@").

find_line_args(Forms, Name, Arity, Default) ->
    case [ {L, Vars} || {function, L, N, A, Vars} <- Forms, N =:= Name, A =:= Arity ] of
        [{Line, [{clause, _, V, _, _} | _]}] -> {Line, V};
        [] -> {Default, undefined}
    end.

arity(#func{attribute = remote, arity = SourceArity}) -> SourceArity;
arity(#func{attribute = remote_singleton, arity = SourceArity}) -> SourceArity - 1.

a_exports(Ms) ->
    [ ?EXPORT(Line, wrapper_name(F), arity(Func)) || #func{name = F, line = Line} = Func <- Ms ].

f_wrappers(Ms) ->
    [ f_wrapper(M) || M <- Ms ].

f_wrapper(#func{attribute = Attr, wrap_module = MMod, wrap_name = MName, name = Fun, arity = SourceArity, line = Line, args = Args}) ->
    Args1 = [ arg_name(Args, I, Line) || I <- lists:seq(2, SourceArity) ],
    P = ?VAR(Line, 'P'),
    case Attr of
        remote ->
            RefArg = ?VAR(Line, list_to_atom("Ref")),
            ?FUNCTION(Line, wrapper_name(Fun), [RefArg | Args1], [
                ?APPLY(Line,MMod,MName,[RefArg, ?FUN(Line, [P], [?CALL(Line, Fun, [P | Args1])])])
            ]);
        remote_singleton ->
            ?FUNCTION(Line, wrapper_name(Fun), Args1, [
                ?APPLY(Line,MMod,MName,[?FUN(Line, [P], [?CALL(Line, Fun, [P | Args1])])])
            ])
    end.

arg_name(Args, I, Line) when length(Args) >= I ->
    case lists:nth(I, Args) of
        {var, _, Var} ->
            ?VAR(Line, Var);
        _ ->
            default_arg_name(I, Line)
    end;
arg_name(_, I, Line) ->
    default_arg_name(I, Line).

default_arg_name(I, Line) ->
    ?VAR(Line, list_to_atom("Arg" ++ integer_to_list(I))).
