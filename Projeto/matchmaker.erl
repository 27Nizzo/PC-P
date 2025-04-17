-module(matchmaker).
-export([start/0, enter_queue/1]).

%%% Interface %%%

enter_queue(Username) ->
    ?MODULE ! {self(), {enter, Username}},
    receive
        Response -> Response
    end.

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(?MODULE, Pid).

%%% Estado: lista de usernames à espera %%%

loop(Queue) ->
    receive
        {Pid, {enter, Username}} ->
            NewQueue = Queue ++ [Username],
            case NewQueue of
                [P1, P2 | Rest] ->
                    %% Emparelhar dois jogadores
                    io:format("Duelo iniciado entre ~s e ~s~n", [P1, P2]),
                    %% Aqui futuramente: spawn(duel, ...)
                    Pid ! {ok, waiting},
                    loop(Rest);
                _ ->
                    Pid ! {ok, queued},
                    loop(NewQueue)
            end
    end.
