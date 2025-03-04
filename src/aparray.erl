-module(aparray).

%% Include files

%% Exported functions

-export([
    index_of_empty/1,
    find_index/2,
    index_of/2,
    sparse_count/1
]).

-type index() :: non_neg_integer().

%% API

-spec index_of_empty(array:array()) -> index() | 'undefined'.

%% @doc Returns the index of the first default element in Array
%% or 'undefined' if there're no default elements.

index_of_empty(Array) ->
    Default = array:default(Array),
    index_of(Default, Array).

-spec find_index(aplib:predicate(term()), array:array()) -> index() | 'undefined'.

%% @doc Returns the index of the first element in Array satisfying
%% predicate Pred or 'undefined' if there're no such elements.

find_index(Pred, Array) ->
    try
        array:foldl(
            fun(I, Value, Acc) ->
                    case Pred(Value) of
                        true -> throw(I);
                        false -> Acc
                    end
            end, undefined, Array)
    catch
        I when is_integer(I) -> I
    end.

-spec index_of(term(), array:array()) -> index() | 'undefined'.

%% @doc Searches for the index of the first occurrence of Elem in Array.
%% Returns 'undefined' if not found.

index_of(Elem, Array) ->
    find_index(fun(Item) -> Item =:= Elem end, Array).

-spec sparse_count(array:array()) -> non_neg_integer().

%% @doc Returns the count of non-default elements in array.

sparse_count(Array) ->
    array:sparse_foldl(fun(_, _, Acc) -> Acc + 1 end, 0, Array).

%% Local functions
