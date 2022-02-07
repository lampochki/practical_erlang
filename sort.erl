-module(sort).

-export([merge_sort/1, merge_two_list/2, merge_two_list/3]).

merge_two_list (Left, Right) -> merge_two_list(Left, Right, []).
merge_two_list([], Right, Acc) ->  lists:reverse(Acc) ++ Right;
merge_two_list(Left, [], Acc)->  lists:reverse(Acc) ++ Left;
merge_two_list([Head1|Tail1], [Head2|Tail2], Acc) when Head1 < Head2 ->
    Acc1 = [Head1|Acc],
    merge_two_list(Tail1, [Head2|Tail2], Acc1);
merge_two_list([Head1|Tail1], [Head2|Tail2], Acc) when Head1 > Head2 ->
    Acc1 = [Head2|Acc],
    merge_two_list([Head1|Tail1], Tail2, Acc1);
merge_two_list([Head1|Tail1], [Head2|Tail2], Acc) when Head1 == Head2 ->
    Acc1 = [Head1|[Head2|Acc]],
    merge_two_list(Tail1,Tail2, Acc1).

merge_sort(List) when length(List) == 1 -> List;
merge_sort(List) ->
    Middle = length(List) div 2,
    {List1, List2} = lists:split(Middle, List),
    Left = merge_sort(List1),
    Right = merge_sort(List2),
    merge_two_list(Left, Right).
