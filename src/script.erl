-module(script).

%% Include files

%% Exported functions

-export([
    compile/1,
    eval/1,
    eval/2,
    eval/3,
    exec/1,
    exec/2,
    exec/3,
    bindings/1,
	parse_term/1
]).

-type name() :: term().
-type value() :: term().
-type bindings() :: [{name(), value()}].
-type script() :: binary() | string().
-type compiled_script() :: erl_parse:abstract_expr().
-type function_handler() :: fun((Name :: atom(), Arguments :: [term()]) -> Value :: value()).

%% API

-spec bindings(Bindings) -> BindingStruct when
      Bindings :: bindings(),
      BindingStruct :: erl_eval:binding_struct().

bindings(Bindings) ->
    lists:foldl(fun({Name, Value}, Acc) -> erl_eval:add_binding(Name, Value, Acc) end, erl_eval:new_bindings(), Bindings).

-spec compile(Script) -> CompiledScript when
      Script :: script(),
      CompiledScript :: compiled_script().

compile(Script) ->
    parse(scan(Script)).

-spec eval(Script) -> Value when
      Script :: script() | compiled_script(),
      Value :: value().

eval(Form) ->
    do_eval(Form, erl_eval:new_bindings(), none).

-spec eval(Script, Bindings) -> Value when
      Script :: script() | compiled_script(),
      Bindings :: erl_eval:binding_struct(),
      Value :: value().

eval(Form, Bindings) ->
    do_eval(Form, Bindings, none).

-spec eval(Script, Bindings, Handler) -> Value when
      Script :: script() | compiled_script(),
      Bindings :: erl_eval:binding_struct(),
      Handler :: function_handler(),
      Value :: value().

eval(Form, Bindings, Handler) ->
    do_eval(Form, Bindings, {value, Handler}).

-spec exec(Script) -> Value when
      Script :: script() | compiled_script(),
      Value :: value().

exec(Form) ->
    do_exec(Form, erl_eval:new_bindings(), none).

-spec exec(Script, Bindings) -> Value when
      Script :: script() | compiled_script(),
      Bindings :: erl_eval:binding_struct(),
      Value :: value().

exec(Form, Bindings) ->
    do_exec(Form, Bindings, none).

-spec exec(Script, Bindings, Handler) -> Value when
      Script :: script() | compiled_script(),
      Bindings :: erl_eval:binding_struct(),
      Handler :: function_handler(),
      Value :: value().

exec(Form, Bindings, Handler) ->
    do_exec(Form, Bindings, {value, Handler}).

-spec parse_term(String) -> term() when
      String :: script().

%% @doc Parses string or binary containig Erlang term into Erlang term.
%% Note: term string should contain period at the end.
%% Example:
%% > parse_term("{[1,2], undefined}.").
%% > {[1,2], undefined}

parse_term(String) when is_binary(String) ->
    parse_term(binary_to_list(String));
parse_term(String) when is_list(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

%% Local functions

do_eval(Form, Bindings, Handler) ->
    erl_eval:expr(form(Form), Bindings, Handler, none, value).

do_exec(Form, Bindings, Handler) ->
    try
        do_eval(Form, Bindings, Handler)
    catch
        error:empty ->
            undefined
    end.

form(Form) when is_tuple(Form) ->
    Form;
form(Script) when is_list(Script); is_binary(Script) ->
    compile(Script).

scan(Binary) when is_binary(Binary) ->
    scan_string(binary_to_list(Binary));
scan(String) when is_list(String) ->
    scan_string(String).

parse(Tokens) ->
    case erl_parse:parse_exprs(Tokens) of
        {ok, [Expr]} ->
            Expr;
        {ok, [_|_] = Exprs} ->
            {block, 1, Exprs};
        {error, ErrorInfo} ->
            erlang:error({parse_error, ErrorInfo})
    end.

scan_string(String) ->
    case erl_scan:string(String) of
        {ok, [], _} ->
            erlang:error(empty);
        {ok, Tokens, EndLocation} ->
            case lists:last(Tokens) of
                {dot, _} ->
                    Tokens;
                _ ->
                    Tokens ++ [{dot, EndLocation}]
            end;
        {error, Error, _} ->
            erlang:error({scan_errror, Error})
    end.
