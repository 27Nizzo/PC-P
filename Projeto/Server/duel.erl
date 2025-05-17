-module(duel).
-export([start/2, add_point/3, get_duel/1, get_players/1]).

-record(duel_state, {
    player1,
    player2,
    score1 = 0,
    score2 = 0,
    pid
}).

start(Player1, Player2) ->
    State = #duel_state{
        player1 = Player1,
        player2 = Player2,
        pid = self()
    },
    Pid = spawn(fun() -> loop(State) end),
    register_duel(Player1, Player2, Pid),
    Pid.

register_duel(P1, P2, Pid) ->
    ets:insert(duels_table, [{P1, Pid}, {P2, Pid}]).

loop(State) ->
    receive
        {add_point, From, Player, Points} ->
            NewState = add_points(State, Player, Points),
            loop(NewState);
        {get_players, From} ->
            From ! {players, [State#duel_state.player1, State#duel_state.player2]},
            loop(State);
        _ ->
            loop(State)
    end.

add_points(State, Player, Points) when Player == State#duel_state.player1 ->
    State#duel_state{score1 = State#duel_state.score1 + Points};
add_points(State, Player, Points) when Player == State#duel_state.player2 ->
    State#duel_state{score2 = State#duel_state.score2 + Points}.

add_point(DuelPid, Player, Points) ->
    DuelPid ! {add_point, self(), Player, Points},
    ok.

get_duel(Player) ->
    case ets:lookup(duels_table, Player) of
        [{Player, Pid}] -> {ok, Pid};
        [] -> {error, not_in_duel}
    end.

get_players(Player) ->
    case get_duel(Player) of
        {ok, Pid} ->
            Pid ! {get_players, self()},
            receive
                {players, Players} -> Players
            after 1000 -> []
            end;
        _ -> []
    end.