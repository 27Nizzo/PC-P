-module(duel).
-export([start/2, add_point/3, get_state/1, end_duel/1, force_end/1]).

-record(duel_state, {
    player1,
    player2,
    score1 = 0,
    score2 = 0,
    start_time = erlang:system_time(second),
    duration = 120, % 2 minutos em segundos
    timer_ref
}).

start(P1, P2) ->
    State = #duel_state{player1 = P1, player2 = P2},
    {ok, TimerRef} = timer:send_after(State#duel_state.duration * 1000, timeout),
    Pid = spawn(fun() -> duel_loop(State#duel_state{timer_ref = TimerRef}) end),
    Pid.

duel_loop(State) ->
    receive
        {add_point, From, Player, Points} ->
            NewState = case Player of
                P when P =:= State#duel_state.player1 ->
                    State#duel_state{score1 = State#duel_state.score1 + Points};
                P when P =:= State#duel_state.player2 ->
                    State#duel_state{score2 = State#duel_state.score2 + Points}
            end,
            duel_loop(NewState);
            
        {get_state, From} ->
            From ! {duel_state, State},
            duel_loop(State);
            
        {end_duel, From} ->
            timer:cancel(State#duel_state.timer_ref),
            report_results(State, normal),
            ok;
            
        timeout ->
            report_results(State, timeout),
            ok;
            
        Unknown ->
            io:format("Received unknown message: ~p~n", [Unknown]),
            duel_loop(State)
    end.

report_results(#duel_state{player1 = P1, player2 = P2, score1 = S1, score2 = S2} = State, EndType) ->
    io:format("Duel ended between ~p (~p) and ~p (~p)~n", [P1, S1, P2, S2]),
    
    case {EndType, S1 =:= S2} of
        {timeout, true} ->  % Empate
            notify_players(P1, P2, {draw, S1, S2});
        {_, false} ->       % Tem vencedor
            {Winner, Loser, WScore, LScore} = case S1 > S2 of
                true -> {P1, P2, S1, S2};
                false -> {P2, P1, S2, S1}
            end,
            account_server:update_stats(Winner, win),
            account_server:update_stats(Loser, loss),
            notify_players(P1, P2, {winner, Winner, WScore, LScore})
    end,
    matchmaker:duel_ended(P1, P2).

notify_players(P1, P2, Result) ->
    [case matchmaker:get_client_pid(Player) of
        {ok, Pid} -> Pid ! {duel_ended, Result};
        _ -> ok
     end || Player <- [P1, P2]].

add_point(DuelPid, Player, Points) ->
    DuelPid ! {add_point, self(), Player, Points},
    ok.

get_state(DuelPid) ->
    DuelPid ! {get_state, self()},
    receive
        {duel_state, State} -> {ok, State}
    after 1000 -> {error, timeout}
    end.

end_duel(DuelPid) ->
    DuelPid ! {end_duel, self()},
    ok.

force_end(DuelPid) ->
    DuelPid ! timeout,
    ok.