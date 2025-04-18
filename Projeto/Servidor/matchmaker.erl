-module(matchmaker).
-export([start/0, enter_queue/1, leave_queue/1, end_duel/1]).

%%% Interface %%%

enter_queue(Username) ->
    ?MODULE ! {self(), {enter, Username}},
    receive
        Response -> Response
    end.

leave_queue(Username) ->
    ?MODULE ! {self(), {leave, Username}},
    receive
        Response -> Response
    end.

end_duel(Username) ->
    ?MODULE ! {self(), {end_duel, Username}},
    receive Response -> Response end.

%%% Início do processo

start() ->
    Pid = spawn(fun() -> loop([], #{}) end),
    register(?MODULE, Pid).

%%% loop/2: Queue = jogadores em espera, Duels = jogadores em duelo

loop(Queue, Duels) ->
    receive

        %% Jogador entra na fila
        {Pid, {enter, Username}} ->
            case login_manager:is_logged_in(Username) of
                true ->
                    NewQueue = Queue ++ [Username],
                    case NewQueue of
                        [P1, P2 | Rest] ->
                            case P1 =:= P2 of
                                true ->
                                    io:format("Tentativa de duelo com o mesmo user, ignorada para ~s~n", [P1]),
                                    Pid ! {error, same_user},
                                    %% Mantém um dos jogadores na fila
                                    loop(Rest ++ [P1], Duels);
                                false ->
                                    DuelPid = duel:start(P1, P2),
                                    NewDuels = Duels#{P1 => DuelPid, P2 => DuelPid},
                                    io:format("Duelo iniciado entre ~s e ~s~n", [P1, P2]),
                                    Pid ! {ok, waiting},
                                    loop(Rest, NewDuels)
                            end;
                        _ ->
                            Pid ! {ok, queued},
                            loop(NewQueue, Duels)
                    end;
                false ->
                    Pid ! {error, not_logged_in},
                    loop(Queue, Duels)
            end;

        %% Jogador quer sair da fila
        {Pid, {leave, Username}} ->
            case lists:member(Username, Queue) of
                true ->
                    NewQueue = lists:delete(Username, Queue),
                    Pid ! ok,
                    loop(NewQueue, Duels);
                false ->
                    Pid ! not_in_queue,
                    loop(Queue, Duels)
            end;

        %% Jogador quer terminar o duelo onde está
        {Pid, {end_duel, Username}} ->
            case maps:find(Username, Duels) of
                {ok, DuelPid} ->
                    DuelPid ! {end_duel, Username},
                    %% Remove todos os jogadores associados a esse DuelPid
                    NewDuels = maps:filter(
                        fun(_, P) -> P =/= DuelPid end,
                        Duels
                    ),
                    Pid ! {ok, ended},
                    loop(Queue, NewDuels);
                error ->
                    Pid ! {error, not_in_duel},
                    loop(Queue, Duels)
            end

    end.
