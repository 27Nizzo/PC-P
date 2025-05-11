-module(client_session).
-export([start/2, reply/2]).

start(Sock, Handler) ->
    spawn(fun() -> loop(Sock, Handler) end).

reply(Sock, Msg) ->
    gen_tcp:send(Sock, term_to_binary(Msg)).

loop(Sock, Handler) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Msg = binary_to_term(Data),
            Handler(Sock, Msg),
            loop(Sock, Handler);
        {error, closed} ->
            io:format("Client disconnected~n")
    end.