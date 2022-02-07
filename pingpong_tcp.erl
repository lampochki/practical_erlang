-module(pingpong_tcp).

-export([tcp_server/0,loop/1,tcp_client/1, seq_loop/1]).

tcp_server() ->
    {ok, Listen} = gen_tcp:listen(1234, [binary, {packet, 4},
        {reuseaddr, true},
        {active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server received ~p~n",[Str]),
            case Str of
                "ping" -> Reply = "pong";
                "pong" -> Reply = "ping";
                _ -> Reply = "This is not ping or pong"
            end,
            io:format("Server replying = ~p~n",[Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

tcp_client(Str) ->
    {ok, Socket} =
        gen_tcp:connect("localhost", 1234,
            [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp,Socket,Bin} ->
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n",[Val]),
            gen_tcp:close(Socket)
    end.
