-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1, create_short_test/0, get_long_test/0]).

-include_lib("eunit/include/eunit.hrl").

%%% module API

init() ->
  %% init randomizer
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  rand:seed(exsp, {A,B,C}),
  State = [],
  State.


create_short(LongLink, State) ->
  case proplists:get_value(LongLink, State) of
    undefined -> State1 = [{LongLink, string:join([LongLink, rand_str(7)], "/")} | State],
      {proplists:get_value(LongLink, State1), State1};
    _ -> {proplists:get_value(LongLink, State), State}
  end.


get_long_keys(State) ->
  proplists:get_keys(State).

get_long(ShortLink, State) -> get_long(ShortLink, State, get_long_keys(State)).
get_long(_, _, []) -> {error, not_found};
get_long(ShortLink, State, [First|Rest]) ->
  case proplists:get_value(First, State) == ShortLink of
    true -> {ok, First};
    _ -> get_long(ShortLink,State,Rest)
  end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
  lists:map(fun(Char) when Char > 83 -> Char + 13;
    (Char) when Char > 57 -> Char + 7;
    (Char) -> Char
            end,
    [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).


create_short_test() ->
  State1 = short_link:init(),
  {Short1, State2} = short_link:create_short("http://hexlet.io", State1),
  {Short2, State3} = short_link:create_short("http://lenta.ru", State2),
  {Short3, State4} = short_link:create_short("http://ya.ru", State3),
  {Short4, State5} = short_link:create_short("http://facebook.com", State4),
  ?assertMatch({Short1, _}, short_link:create_short("http://hexlet.io", State5)),
  ?assertMatch({Short2, _}, short_link:create_short("http://lenta.ru", State5)),
  ?assertMatch({Short3, _}, short_link:create_short("http://ya.ru", State5)),
  ?assertMatch({Short4, _}, short_link:create_short("http://facebook.com", State5)),
  ok.


get_long_test() ->
  State0 = short_link:init(),
  ?assertEqual({error, not_found}, short_link:get_long("foobar", State0)),

  {Short1, State1} = short_link:create_short("http://hexlet.io", State0),
  ?assertEqual({ok, "http://hexlet.io"}, short_link:get_long(Short1, State1)),

  {Short2, State2} = short_link:create_short("http://lenta.ru", State1),
  ?assertEqual({ok, "http://lenta.ru"}, short_link:get_long(Short2, State2)),

  {Short3, State3} = short_link:create_short("http://ya.ru", State2),
  ?assertEqual({ok, "http://ya.ru"}, short_link:get_long(Short3, State3)),

  {Short4, State4} = short_link:create_short("http://facebook.com", State3),
  ?assertEqual({ok, "http://facebook.com"}, short_link:get_long(Short4, State4)),

  ?assertEqual({ok, "http://hexlet.io"}, short_link:get_long(Short1, State4)),
  ?assertEqual({ok, "http://lenta.ru"}, short_link:get_long(Short2, State4)),
  ?assertEqual({ok, "http://ya.ru"}, short_link:get_long(Short3, State4)),
  ?assertEqual({ok, "http://facebook.com"}, short_link:get_long(Short4, State4)),
  ?assertEqual({error, not_found}, short_link:get_long("bla-bla-bla", State4)),

  ok.