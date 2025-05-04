-module(duel).
-export([start/2]).

start(Player1, Player2) ->
    spawn(fun() -> loop(Player1, Player2) end).

loop(P1, P2) ->
    receive
        {end_duel, From} ->
            io:format("Duelo entre ~s e ~s terminado por ~s~n", [P1, P2, From]),
            %% Mais tarde: enviar mensagem aos clientes
            ok;
        _ ->
            loop(P1, P2)
    end.
