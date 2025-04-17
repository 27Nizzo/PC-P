-module(server).
-export([start/0, accept_loop/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [
        binary,
        {packet, line},    
        {active, false},    
        {reuseaddr, true}   
    ]),
    io:format("Servidor a correr na porta 1234~n"),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_session:start(Socket) end),
    accept_loop(ListenSocket).
