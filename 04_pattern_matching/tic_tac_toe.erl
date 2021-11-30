-module(tic_tac_toe).

-export([new_game/0, win/1, move/3, win_test/0, move_test/0, move2_test/0]).

-include_lib("eunit/include/eunit.hrl").

new_game() ->
  {{f, f, f},
    {f, f, f},
    {f, f, f}}.


win({{X1, _, _}, {Y1, _, _}, {Z1, _, _}}) when X1 == Y1 andalso X1 == Z1 andalso Z1 =/= f -> {win, X1};
win({{_, X2, _}, {_, Y2, _}, {_, Z2, _}}) when X2 == Y2 andalso X2 == Z2 andalso Z2 =/= f -> {win, X2};
win({{_, _, X3}, {_, _, Y3}, {_, _, Z3}}) when X3 == Y3 andalso X3 == Z3 andalso Z3 =/= f -> {win, X3};
win({{X1, _, _}, {_, Y2, _}, {_, _, Z3}}) when X1 == Y2 andalso X1 == Z3 andalso X1 =/= f -> {win, X1};
win({{_, _, X3}, {_, Y2, _}, {Z1, _, _}}) when Z1 == Y2 andalso Z1 == X3 andalso Z1 =/= f -> {win, Z1};
win({{X1, X2, X3}, _, _}) when X1 == X2 andalso X1 == X3 andalso X1 =/= f -> {win, X1};
win({_ ,{Y1, Y2, Y3},  _}) when Y1 == Y2 andalso Y1 == Y3 andalso Y1 =/= f -> {win, Y1};
win({_, _, {Z1, Z2, Z3}}) when Z1 == Z2 andalso Z1 == Z3 andalso Z1 =/= f -> {win, Z1};
win({_, _, _}) -> no_win.


move(1, Player, {{X1, X2, X3}, Y, Z}) when X1 =/= o andalso X1 =/= x-> {ok,{{Player, X2, X3}, Y, Z}};
move(2, Player, {{X1, X2, X3}, Y, Z}) when X2 =/= o andalso X2 =/= x-> {ok,{{ X1, Player, X3}, Y, Z}};
move(3, Player, {{X1, X2, X3}, Y, Z}) when X3 =/= o andalso X3 =/= x-> {ok,{{ X1, X2, Player}, Y, Z}};
move(4, Player, {X, {Y1, Y2, Y3}, Z}) when Y1 =/= o andalso Y1 =/= x-> {ok,{X, {Player, Y2, Y3},Z}};
move(5, Player, {X, {Y1, Y2, Y3}, Z}) when Y2 =/= o andalso Y2 =/= x-> {ok,{X, { Y1, Player, Y3},Z}};
move(6, Player, {X, {Y1, Y2, Y3}, Z}) when Y3 =/= o andalso Y3 =/= x-> {ok,{X, { Y1, Y2, Player},Z}};
move(7, Player, {X, Y, {Z1, Z2, Z3}}) when Z1 =/= o andalso Z1 =/= x-> {ok,{X, Y, {Player, Z2, Z3}}};
move(8, Player, {X, Y, {Z1, Z2, Z3}}) when Z2 =/= o andalso Z2 =/= x-> {ok,{X, Y, { Z1, Player, Z3}}};
move(9, Player, {X, Y, {Z1, Z2, Z3}}) when Z3 =/= o andalso Z3 =/= x-> {ok,{X, Y, { Z1, Z2, Player}}};
move(_,_,_) -> {error, invalid_move}.


win_test() ->
  ?assertEqual(no_win, tic_tac_toe:win(tic_tac_toe:new_game())),
  ?assertEqual(no_win, tic_tac_toe:win({{x,o,x},
    {o,x,o},
    {o,x,o}})),
  ?assertEqual({win, x}, tic_tac_toe:win({{x,x,x},
    {f,f,f},
    {f,f,f}})),
  ?assertEqual({win, o}, tic_tac_toe:win({{f,f,x},
    {o,o,o},
    {f,x,f}})),
  ?assertEqual({win, x}, tic_tac_toe:win({{o,o,f},
    {f,o,f},
    {x,x,x}})),
  ?assertEqual({win, o}, tic_tac_toe:win({{o,f,f},
    {o,x,f},
    {o,x,x}})),
  ?assertEqual({win, x}, tic_tac_toe:win({{f,x,f},
    {o,x,o},
    {f,x,o}})),
  ?assertEqual({win, o}, tic_tac_toe:win({{x,x,o},
    {o,x,o},
    {x,f,o}})),
  ?assertEqual({win, x}, tic_tac_toe:win({{x,f,o},
    {f,x,o},
    {f,o,x}})),
  ?assertEqual({win, o}, tic_tac_toe:win({{x,x,o},
    {x,o,x},
    {o,f,f}})),
  ok.


move_test() ->
  G0 = tic_tac_toe:new_game(),

  {ok, G1} = tic_tac_toe:move(1, x, G0),
  ?assertEqual({{x,f,f},{f,f,f},{f,f,f}}, G1),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(1, x, G1)),

  {ok, G2} = tic_tac_toe:move(5, o, G1),
  ?assertEqual({{x,f,f},{f,o,f},{f,f,f}}, G2),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(5, o, G2)),

  {ok, G3} = tic_tac_toe:move(4, x, G2),
  ?assertEqual({{x,f,f},{x,o,f},{f,f,f}}, G3),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(4, x, G3)),

  {ok, G4} = tic_tac_toe:move(7, o, G3),
  ?assertEqual({{x,f,f},{x,o,f},{o,f,f}}, G4),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(7, o, G4)),

  {ok, G5} = tic_tac_toe:move(2, x, G4),
  ?assertEqual({{x,x,f},{x,o,f},{o,f,f}}, G5),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(2, x, G5)),

  {ok, G6} = tic_tac_toe:move(3, o, G5),
  ?assertEqual({{x,x,o},{x,o,f},{o,f,f}}, G6),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(3, o, G6)),

  ?assertEqual({win, o}, tic_tac_toe:win(G6)),

  ok.


move2_test() ->
  ?assertEqual({ok, {{x,f,f},{f,f,f},{f,f,f}}}, tic_tac_toe:move(1, x, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,x,f},{f,f,f},{f,f,f}}}, tic_tac_toe:move(2, x, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,x},{f,f,f},{f,f,f}}}, tic_tac_toe:move(3, x, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{x,f,f},{f,f,f}}}, tic_tac_toe:move(4, x, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{f,x,f},{f,f,f}}}, tic_tac_toe:move(5, x, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{f,f,o},{f,f,f}}}, tic_tac_toe:move(6, o, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{f,f,f},{o,f,f}}}, tic_tac_toe:move(7, o, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{f,f,f},{f,o,f}}}, tic_tac_toe:move(8, o, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({ok, {{f,f,f},{f,f,f},{f,f,o}}}, tic_tac_toe:move(9, o, {{f,f,f},{f,f,f},{f,f,f}})),
  ?assertEqual({error, invalid_move}, tic_tac_toe:move(10, o, {{f,f,f},{f,f,f},{f,f,f}})),
  ok.


