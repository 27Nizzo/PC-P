-module(matchmaker).
-export([start/0, stop/0, enter_queue/1, leave_queue/1, get_queue/0, get_duels/0, duel_ended/2]).

-define(MATCHMAKER_INTERVAL, 1000). % 1 segundo entre verificações
-define(MAX_LEVEL_DIFF, 1).        % Máxima diferença de nível entre jogadores

start() ->
    % Força a limpeza de tabelas existentes antes de criar novas
    stop(),
    
    % Cria novas tabelas ETS
    ets:new(matchmaker_queue, [named_table, ordered_set, public, {keypos, 1}]),
    ets:new(active_duels, [named_table, set, public]),
    
    % Inicia o processo do matchmaker
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn_link(fun() -> matchmaking_loop() end),
            register(?MODULE, Pid),
            ok;
        _ ->
            ok
    end.

stop() ->
    % Limpeza explícita das tabelas e processo
    catch ets:delete(matchmaker_queue),
    catch ets:delete(active_duels),
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> exit(Pid, normal)
    end,
    ok.

enter_queue(Username) ->
    try
        case account_server:is_logged_in(Username) of
            true ->
                case account_server:get_stats(Username) of
                    {ok, #{nvl := Nvl}} ->
                        case ets:match_object(matchmaker_queue, {'_', Username}) of
                            [] ->
                                ets:insert(matchmaker_queue, {Nvl, Username}),
                                {ok, queued};
                            [_] ->
                                {error, already_queued}
                        end;
                    _ ->
                        {error, stats_unavailable}
                end;
            false ->
                {error, not_logged_in}
        end
    catch
        _:_ -> {error, system_error}
    end.

leave_queue(Username) ->
    try
        case ets:match_object(matchmaker_queue, {'_', Username}) of
            [] -> {error, not_in_queue};
            [Match] ->
                ets:delete_object(matchmaker_queue, Match),
                {ok, left_queue}
        end
    catch
        _:_ -> {error, system_error}
    end.

matchmaking_loop() ->
    timer:sleep(?MATCHMAKER_INTERVAL),
    try
        case ets:info(matchmaker_queue) of
            undefined -> 
                matchmaking_loop();
            _ ->
                Queue = ets:tab2list(matchmaker_queue),
                match_players(lists:sort(Queue)),
                matchmaking_loop()
        end
    catch
        Error:Reason ->
            io:format("Error in matchmaking_loop: ~p:~p~n", [Error, Reason]),
            matchmaking_loop()
    end.

match_players([{Nvl1, P1}, {Nvl2, P2} | Rest]) when abs(Nvl1 - Nvl2) =< ?MAX_LEVEL_DIFF ->
    case {account_server:is_logged_in(P1), account_server:is_logged_in(P2)} of
        {true, true} ->
            ets:delete_object(matchmaker_queue, {Nvl1, P1}),
            ets:delete_object(matchmaker_queue, {Nvl2, P2}),
            start_duel(P1, P2),
            match_players(Rest);
        _ ->
            % Remove jogadores que não estão mais logados
            ets:match_delete(matchmaker_queue, {'_', P1}),
            ets:match_delete(matchmaker_queue, {'_', P2}),
            match_players(Rest)
    end;
match_players([_ | Rest]) ->
    match_players(Rest);
match_players(_) ->
    ok.

start_duel(P1, P2) ->
    try
        DuelPid = duel:start(P1, P2),
        ets:insert(active_duels, [{P1, DuelPid}, {P2, DuelPid}]),
        notify_players(P1, P2, DuelPid)
    catch
        _:_ -> 
            % Se não conseguir iniciar o duelo, coloca os jogadores de volta na fila
            ets:insert(matchmaker_queue, [{account_server:get_level(P1), P1}, 
                                         {account_server:get_level(P2), P2}])
    end.

notify_players(P1, P2, DuelPid) ->
    lists:foreach(fun(Username) ->
        case get_client_pid(Username) of
            {ok, Pid} -> Pid ! {match_found, get_opponent(Username, P1, P2), DuelPid};
            _ -> ok
        end
    end, [P1, P2]).

get_opponent(Username, P1, P2) ->
    case Username of
        P1 -> P2;
        P2 -> P1
    end.

get_queue() ->
    case ets:info(matchmaker_queue) of
        undefined -> [];
        _ -> ets:tab2list(matchmaker_queue)
    end.

get_duels() ->
    case ets:info(active_duels) of
        undefined -> [];
        _ -> ets:tab2list(active_duels)
    end.

get_client_pid(Username) ->
    try
        case ets:lookup(player_sockets, Username) of
            [{Username, Sock}] -> {ok, Sock};
            [] -> {error, not_found}
        end
    catch
        _:_ -> {error, system_error}
    end.

duel_ended(P1, P2) ->
    try
        case ets:lookup(active_duels, P1) of
            [{_, _DuelPid}] ->
                ets:delete(active_duels, P1),
                ets:delete(active_duels, P2),
                ok;
            [] ->
                {error, duel_not_found}
        end
    catch
        _:_ -> {error, system_error}
    end.