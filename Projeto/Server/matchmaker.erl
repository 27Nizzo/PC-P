-module(matchmaker).
-export([start/0, enter_queue/1, leave_queue/1, get_queue/0, get_duels/0, duel_ended/2]).

start() ->
    ets:new(matchmaker_queue, [named_table, ordered_set, public]),
    ets:new(active_duels, [named_table, set, public]),
    Pid = spawn(fun() -> matchmaking_loop() end),
    register(?MODULE, Pid),
    ok.

enter_queue(Username) ->
    case account_server:is_logged_in(Username) of
        true ->
            case account_server:get_stats(Username) of
                {ok, #{nvl := Nvl}} ->
                    ets:insert(matchmaker_queue, {Nvl, Username}),
                    {ok, queued};
                _ ->
                    {error, stats_unavailable}
            end;
        false ->
            {error, not_logged_in}
    end.

leave_queue(Username) ->
    case ets:match_object(matchmaker_queue, {'_', Username}) of
        [] -> {error, not_in_queue};
        [Match] ->
            ets:delete_object(matchmaker_queue, Match),
            {ok, left_queue}
    end.

matchmaking_loop() ->
    timer:sleep(1000),
    case ets:tab2list(matchmaker_queue) of
        [] -> 
            matchmaking_loop();
        Queue ->
            match_players(Queue),
            matchmaking_loop()
    end.

match_players([{Nvl1, P1}, {Nvl2, P2} | Rest]) when abs(Nvl1 - Nvl2) =< 1 ->
    ets:delete_object(matchmaker_queue, {Nvl1, P1}),
    ets:delete_object(matchmaker_queue, {Nvl2, P2}),
    start_duel(P1, P2),
    match_players(Rest);
match_players([_ | Rest]) ->
    match_players(Rest);
match_players(_) ->
    ok.

start_duel(P1, P2) ->
    DuelPid = duel:start(P1, P2),
    ets:insert(active_duels, [{P1, DuelPid}, {P2, DuelPid}]),
    notify_players(P1, P2, DuelPid).

notify_players(P1, P2, DuelPid) ->
    case get_client_pid(P1) of
        {ok, Pid1} -> Pid1 ! {match_found, P2, DuelPid};
        _ -> ok
    end,
    case get_client_pid(P2) of
        {ok, Pid2} -> Pid2 ! {match_found, P1, DuelPid};
        _ -> ok
    end.

notify_timeout(DuelPid) ->
    duel:force_end(DuelPid).

get_queue() ->
    ets:tab2list(matchmaker_queue).

get_duels() ->
    ets:tab2list(active_duels).

get_client_pid(Username) ->
    case ets:lookup(player_sockets, Username) of
        [{Username, Sock}] -> {ok, Sock};
        [] -> {error, not_found}
    end.

duel_ended(P1, P2) ->
    case ets:lookup(active_duels, P1) of
        [{_, DuelPid}] ->
            ets:delete(active_duels, P1),
            ets:delete(active_duels, P2),
            notify_timeout(DuelPid),
            ok;
        [] ->
            {error, duel_not_found}
    end.
