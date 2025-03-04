-module(rand_list).

%% Include files

%% Exported functions

-export([
    reset/0,
    take/1,
    take_list/2,
    select/1,
    select_list/2,
    sublist/1,
    shuffle/1,
    weight_select/2,
    weight_select/3
]).

%% API

-spec reset() -> 'ok'.

reset() ->
    rand:seed(exrop),
    ok.

-spec take(List) -> {RandomItem, NewList} when
      List :: [T],
      RandomItem :: T,
      NewList :: [T].

take(List) ->
    Len = length(List),
    take_item(List, Len).

-spec take_list(List, Count) -> {RandomItems, NewList} when
      List :: [T],
      Count :: non_neg_integer(),
      RandomItems :: [T],
      NewList :: [T].

take_list(List, Count) ->
    take2(List, length(List), [], Count).

-spec select(List) -> RandomItem when
      List :: [T],
      RandomItem :: T.

select(List) ->
    lists:nth(rand:uniform(length(List)), List).

-spec select_list(List, Count) -> RandomItems when
      List :: [T],
      Count :: non_neg_integer(),
      RandomItems :: [T].

select_list(List, Count) ->
    select2(List, length(List), Count, []).

-spec sublist(List) -> RandomSublist when
      List :: [T],
      RandomSublist :: [T].

sublist([]) ->
    [];
sublist(List) ->
    N = length(List),
    B = crypto:strong_rand_bytes(N div 8 + 1),
    sublist1(List, B, []).

-spec shuffle(List) -> ShuffledList when
      List :: [T],
      ShuffledList :: [T].

%% @doc [http://erlang.org/pipermail/erlang-questions/2010-May/051580.html]

shuffle(List) ->
    unwrap(lists:sort(wrap(List, [])), []).

-spec weight_select(N, [T]) -> T when
      N :: pos_integer(),
      T :: tuple().

weight_select(N, WeightList) ->
    aplists:weight_select(rand:uniform(), N, WeightList).

-spec weight_select(N, [T], Sum) -> T when
      N :: pos_integer(),
      T :: tuple(),
      Sum :: number().

weight_select(N, WeightList, Sum) ->
    aplists:weight_select(rand:uniform(), N, WeightList, Sum).

%% Local functions

select2(_List, _Len, 0, Items) ->
    Items;
select2(List, Len, Count, Items) ->
    Item = lists:nth(rand:uniform(Len), List),
    select2(List, Len, Count - 1, [Item|Items]).

take2(List, _Len, Items, 0) ->
    {Items, List};
take2(List, Len, Items, ItemCount) ->
    {Item, NewList} = take_item(List, Len),
    take2(NewList, Len - 1, [Item|Items], ItemCount - 1).

take_item(_List, 0) ->
    error(not_enough_items);
take_item(List, Len) ->
    Rand = rand:uniform(Len),
    take_item2([], List, 1, Rand).

take_item2(ListLeft, [Item|ListRight], Index, Rand) when Index =:= Rand ->
    {Item, ListLeft ++ ListRight};
take_item2(ListLeft, [Item|ListRight], Index, Rand) ->
    take_item2([Item|ListLeft], ListRight, Index + 1, Rand);
take_item2(_ListLeft, [], _Index, _Rand) ->
    false.

sublist1([], _, R) ->
    lists:reverse(R);
sublist1([H|T], <<1:1,B/bits>>, R) ->
    sublist1(T, B, [H|R]);
sublist1([_|T], <<0:1,B/bits>>, R) ->
    sublist1(T, B, R).

wrap([X|Xs], L) -> wrap(Xs, [{rand:uniform(),X}|L]);
wrap([], L) -> L.

unwrap([{_,X}|L], Xs) -> unwrap(L, [X|Xs]);
unwrap([], Xs) -> Xs.
