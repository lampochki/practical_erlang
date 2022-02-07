-module(pingpong_udp).

-export([udp_server/0,loop/1,udp_client/1]).

udp_server() ->
    {ok, Socket} = gen_udp:open(1235, [binary]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port,Bin} ->
            Str = binary_to_term(Bin),
            io:format("Server received ~p~n",[Str]),
            case Str of
                "ping" -> Reply = "pong";
                "pong" -> Reply = "ping";
                _ -> Reply = "This is not ping or pong"
            end,
            io:format("Server replying = ~p~n",[Reply]),
            gen_udp:send(Socket, Host, Port, term_to_binary(Reply)),
            loop(Socket);
        {udp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

udp_client(Str) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    ok = gen_udp:send(Socket, "localhost", 1235, term_to_binary(Str)),
    Value = receive
            {udp, Socket, _, _, Bin} ->
                Val = binary_to_term(Bin),
                io:format("Client result = ~p~n",[Val])
            after 200 -> 0
            end,
    gen_udp:close(Socket),
    Value.
