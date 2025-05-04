-module(matchmaker).
-export([start/0, enter_queue/1, end_duel/1]).

start() ->
    ets:new(matchmaker_table, [named_table, set, public]),
    spawn(fun() -> loop([], []) end),
    true.

enter_queue(Username) ->
    %% Confirma se o utilizador está autenticado
    case account_server:is_logged_in(Username) of
        true ->
            %% Envia mensagem ao processo principal
            whereis(?MODULE) ! {enter_queue, Username},
            {ok, waiting};
        false ->
            {error, not_logged_in}
    end.

end_duel(Username) ->
    whereis(?MODULE) ! {end_duel, Username},
    {ok, ended}.

loop(Queue, Duels) ->
    receive
        {enter_queue, Username} ->
            case lists:member(Username, Queue) orelse ets:lookup(matchmaker_table, Username) =/= [] of
                true ->
                    loop(Queue, Duels); % já na fila ou em duelo
                false ->
                    ets:insert(matchmaker_table, {Username, in_queue}),
                    case Queue of
                        [] ->
                            loop([Username], Duels);
                        [Opponent | RestQueue] ->
                            %% Confirmar se ambos estão ainda em queue
                            case {ets:lookup(matchmaker_table, Username), ets:lookup(matchmaker_table, Opponent)} of
                                {[{Username, in_queue}], [{Opponent, in_queue}]} ->
                                    ets:insert(matchmaker_table, {Username, in_duel}),
                                    ets:insert(matchmaker_table, {Opponent, in_duel}),
                                    Pid = duel:start(Username, Opponent),
                                    loop(RestQueue, [{Username, Pid}, {Opponent, Pid} | Duels]);
                                _ ->
                                    loop([Username | Queue], Duels)
                            end
                    end
            end;

        {end_duel, Username} ->
            case lists:keyfind(Username, 1, Duels) of
                {Username, Pid} ->
                    Pid ! {end_duel, Username},
                    ets:delete(matchmaker_table, Username),
                    %% Remover ambos os jogadores do duelo
                    {_, Pid2} = lists:keyfind(Username, 1, Duels),
                    Remaining = lists:filter(fun({_, P}) -> P =/= Pid2 end, Duels),
                    loop(Queue, Remaining);
                false ->
                    io:format("~s não está em duelo~n", [Username]),
                    loop(Queue, Duels)
            end;

        _Other ->
            loop(Queue, Duels)
    end.
