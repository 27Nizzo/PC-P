-module(duel).
-export([start/2, report_result/2]).

start(Player1, Player2) ->
    spawn(fun() -> loop(Player1, Player2) end).

loop(P1, P2) ->
    receive
        {end_duel, From} ->
            io:format("Duelo entre ~s e ~s terminado por ~s~n", [P1, P2, From]),
            ok;
        _ ->
            loop(P1, P2)
    end.

report_result(Username, Result) ->
    io:format("Resultado do duelo de ~s: ~p~n", [Username, Result]),
    account_server:update_stats(Username, Result),
    ok.
