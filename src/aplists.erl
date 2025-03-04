-module(aplists).

%% Include files

%% Exported functions

-export([
    toggle/2,
    union/2,
    intersection/2,
    keyupdate/5,
    keyupdate/4,
    fseq/3,
    index_of/2,
    set_value/3,
    keysum/2,
    weight_select/3,
    weight_select/4,
    seq_zip/1,
    seq_zip/2,
    match/2
]).

%% API

-spec toggle(Elem, List) -> NewList when
      Elem :: T,
      List :: [T],
      NewList :: [T].

toggle(Elem, List) ->
    case lists:member(Elem, List) of
        true ->
            lists:delete(Elem, List);
        false ->
            [Elem | List]
    end.

-spec union(List1, List2) -> UnionList when
      List1 :: [T],
      List2 :: [T],
      UnionList :: [T].

union(List1, List2) ->
    lists:umerge(lists:sort(List1), lists:sort(List2)).

-spec intersection(List1, List2) -> IntersectionList when
      List1 :: [T],
      List2 :: [T],
      IntersectionList :: [T].

intersection(List1, List2) ->
    [ Item || Item <- List1, lists:member(Item, List2) ].

-spec keyupdate(Key, N, TupleList1, Fun, Initial) -> TupleList2 when
      Key :: term(),
      N :: pos_integer(),
      Fun :: fun((Value1 :: term()) -> Value2 :: term()),
      TupleList1 :: [tuple()],
      TupleList2 :: [tuple()],
      Initial :: tuple().

keyupdate(Key, N, TupleList1, Fun, Initial) when is_integer(N), N > 0, is_function(Fun, 1), is_tuple(Initial) ->
    keyupdate2(Key, N, TupleList1, Fun, Initial).

keyupdate2(Key, Pos, [Tup|Tail], Fun, _Initial) when element(Pos, Tup) == Key ->
    [Fun(Tup)|Tail];
keyupdate2(Key, Pos, [H|T], Fun, Initial) ->
    [H|keyupdate2(Key, Pos, T, Fun, Initial)];
keyupdate2(_, _, [], _, Initial) -> [Initial].

keyupdate(Key, N, TupleList1, Fun) when is_integer(N), N > 0, is_function(Fun, 1) ->
    keyupdate2(Key, N, TupleList1, Fun).

keyupdate2(Key, Pos, [Tup|Tail], Fun) when element(Pos, Tup) == Key ->
    [Fun(Tup)|Tail];
keyupdate2(Key, Pos, [H|T], Fun) ->
    [H|keyupdate2(Key, Pos, T, Fun)];
keyupdate2(_, _, [], _) -> [].

-spec fseq(From, To, Delta) -> Seq when
      From :: number(),
      To :: number(),
      Delta :: number(),
      Seq :: [number()].

fseq(First, Last, Delta) ->
    if First < Last -> fseq_loop(Last, First, -Delta, []);
       Last < First -> fseq_loop(First, Last, Delta, []);
       true -> [Last]
    end.

fseq_loop(From, To, Delta, Acc) when From >= To ->
    fseq_loop(From + Delta, To, Delta, [From|Acc]);
fseq_loop(_From, _To, _Delta, Acc) ->
    Acc.

-spec index_of(Elem, List) -> Index | 'false' when
      Elem :: term(),
      List :: [Elem],
      Index :: pos_integer().

index_of(Elem, List) -> index_of(Elem, List, 1).

index_of(_, [], _) -> false;
index_of(Elem, [Elem|_], Index) -> Index;
index_of(Elem, [_|T], Index) -> index_of(Elem, T, Index+1).

-spec set_value(Key, List, Value) -> List when
      List :: [{Key, Value}],
      Key :: term(),
      Value :: term().

set_value(Key, List, Value) ->
    lists:keystore(Key, 1, List, {Key, Value}).

-spec keysum(Index, List) -> number() when
      Index :: pos_integer(),
      List :: [tuple()].

keysum(Index, L)          -> keysum(Index, L, 0).

keysum(Index, [H|T], Sum) -> keysum(Index, T, Sum + element(Index, H));
keysum(_Index, [], Sum)    -> Sum.

-spec weight_select(Weight, N, [T]) -> T when
      Weight :: number(),
      N :: pos_integer(),
      T :: tuple().

weight_select(Weight, N, WeightList) ->
    Sum = keysum(N, WeightList),
    weight_select(Weight, N, WeightList, Sum).

-spec weight_select(Weight, N, [T], Sum) -> T when
      Weight :: number(),
      N :: pos_integer(),
      Sum :: number(),
      T :: tuple().

weight_select(Weight, N, WeightList, Sum) ->
    weight_select2(N, WeightList, 0, Weight * Sum).

-spec seq_zip(List1) -> List2 when
      List1 :: [T],
      List2 :: [{integer(), T}],
      T :: term().

seq_zip(List1) ->
    seq_zip(List1, 1).

-spec seq_zip(List1, StartN) -> List2 when
      List1 :: [T],
      List2 :: [{integer(), T}],
      StartN :: integer(),
      T :: term().

seq_zip(List1, StartN) ->
    lists:zip(lists:seq(StartN, StartN + length(List1) - 1), List1).

-spec match(term(), [T]) -> [T].

match(Pattern, List) ->
    MatchSpec = ets:match_spec_compile([{Pattern, [], ['$_']}]),
    ets:match_spec_run(List, MatchSpec).

%% Local functions

weight_select2(N, [H|T], Iter, Random) ->
    NextIter = Iter + element(N, H),
    if Random =< NextIter -> H;
       true -> weight_select2(N, T, NextIter, Random)
    end.
