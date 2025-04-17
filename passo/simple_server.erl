-module(simple_server).
-export([start/0, accept_loop/1, handle_client/1]).

start() ->
    % Abre um socket TCP na porta 1234
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor a correr na porta 1234~n"),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).

handle_client(Socket) ->
    io:format("Cliente conectado~n"),
    gen_tcp:send(Socket, <<"Olá cliente!~n">>),
    gen_tcp:close(Socket),
    io:format("Cliente desconectado~n").

loop(Socket) -> 
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Recebido: ~p~n", [Data]),
            gen_tcp:send(socket, Data),
            loop(Socket);
        {error, closed} ->
            ok
    end.
