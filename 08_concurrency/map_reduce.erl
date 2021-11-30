-module(map_reduce).

-include_lib("eunit/include/eunit.hrl").

-export([start/1, count/1, read_file/1, reduce/1]).


start(Files) ->
  G = fun(X) ->  io:format("~p~n", [read_file(X)]) end,
  [spawn(fun() -> G(X) end) || X <- Files].


reduce(File) -> ok.



read_file(File) ->
  {ok, Data} = file:read_file(File),
  List = string:tokens(erlang:binary_to_list(Data), "\n "),
  count(List).


count(List) -> count(List, maps:new()).
count([], Map) -> Map;
count([First|Rest], Map) ->
  case maps:find(First, Map) of
    error ->  Num_elements = length([X || X <- [First|Rest], X =:= First]),
      New_map = maps:put(First, Num_elements, Map),
      count(Rest, New_map);
    {ok, _} -> count(Rest, Map)
  end.


file5_test() ->
  Data = map_reduce:start(["data1.txt",
    "data2.txt",
    "data3.txt",
    "data4.txt",
    "data5.txt"]),
  ?assertEqual(3, maps:get(<<"а"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
  ?assertEqual(4, maps:get(<<"в"/utf8>>, Data)),
  ?assertEqual(2, maps:get(<<"вслух"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"царя"/utf8>>, Data)),
  ok.


file2_test() ->
  Data = map_reduce:start(["data1.txt", "data2.txt"]),
  ?assertEqual(2, maps:get(<<"а"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
  ?assertEqual(2, maps:get(<<"в"/utf8>>, Data)),
  ?assertEqual(error, maps:find(<<"вслух"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"царя"/utf8>>, Data)),
  ok.


file1_test() ->
  Data = map_reduce:start(["data1.txt", "data777.txt"]),
  ?assertEqual(error, maps:find(<<"а"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
  ?assertEqual(2, maps:get(<<"в"/utf8>>, Data)),
  ?assertEqual(error, maps:find(<<"вслух"/utf8>>, Data)),
  ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
  ?assertEqual(error, maps:find(<<"царя"/utf8>>, Data)),
  ok.