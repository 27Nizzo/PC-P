-module(server).
-export([start/0, accept_loop/1, handle_client/1]).

-define(PORT, 1234).

start() ->
    player_state:start(),
    modifiers:start(),
    spawn(fun game_loop/0),
    projectiles:start(),
    collisions:start(),
    account_server:start(),
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Server started on port ~p~n", [?PORT]),
    spawn(?MODULE, accept_loop, [LSock]).

game_loop() ->
    projectiles:update_all(),
    collisions:check_all(),
    timer:sleep(50), % 20FPS
    game_loop().

accept_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, handle_client, [Sock]),
    gen_tcp:controlling_process(Sock, Pid),
    accept_loop(LSock).

handle_client(Sock) ->
    client_session:start(Sock, fun handle_message/2).

handle_message(Sock, {login, Username, Password}) ->
    case account_server:login(Username, Password) of
        {ok, logged_in} ->
            client_session:reply(Sock, {login_success});
        {error, Reason} ->
            client_session:reply(Sock, {login_failed, Reason})
    end;

handle_message(Sock, {register, Username, Password}) ->
    case account_server:create_account(Username, Password) of
        {ok, created} ->
            client_session:reply(Sock, {register_success});
        {error, Reason} ->
            client_session:reply(Sock, {register_failed, Reason})
    end;

handle_message(Sock, {join_queue, Username}) ->
    matchmaker:add_player(Username, Sock),
    client_session:reply(Sock, {queue_joined});

handle_message(Sock, {duel_result, Username, Result}) ->
    duel:report_result(Username, Result),
    client_session:reply(Sock, {result_recorded});

handle_message(Sock, {info, Username}) ->
    case account_server:get_stats(Username) of
        {ok, Stats} ->
            client_session:reply(Sock, {info, Stats});
        {error, not_found} ->
            client_session:reply(Sock, {error, "User not found"})
    end;

handle_message(Sock, {move, Username, Direction}) ->
    case {player_data:get_position(Username), player_data:get_velocity(Username)} of
        {{ok, {X, Y}}, {ok, {Vx, Vy}}} ->
            Acceleration = 0.2,
            MaxSpeed = 2.0,

            {Ax, Ay} = case Direction of
                up -> {0.0, Acceleration};
                down -> {0.0, -Acceleration};
                left -> {-Acceleration, 0.0};
                right -> {Acceleration, 0.0}
            end,

            NVx0 = Vx + Ax,
            NVy0 = Vy + Ay,

            Speed = math:sqrt(NVx0 * NVx0 + NVy0 * NVy0),

            Factor = if Speed > MaxSpeed -> MaxSpeed / Speed; true -> 1.0 end,

            NVx = NVx0 * Factor,
            NVy = NVy0 * Factor,

            Nx = X + NVx,
            Ny = Y + NVy,

            player_data:set_velocity(Username, {NVx, NVy}),
            player_data:set_position(Username, {Nx, Ny}),

            client_session:reply(Sock, {moved, {Nx, Ny}, {NVx, NVy}});
        _ ->
            client_session:reply(Sock, {error, user_not_found})
    end;

handle_message(Sock, {pickup_mod, Username}) ->
    case player_data:get_position(Username) of
        {ok, Pos} -> 
            case modifiers:get_at(Pos) of
                {ok, Type} ->
                    modifiers:remove(Pos),
                    mod_active(Username, Type),
                    client_session:reply(Sock, {mod_active, Type});
                {error, not_found} -> 
                    client_session:reply(Sock, {no_powerup})
            end;
        {error, not_found} ->
            client_session:reply(Sock, {error, user_not_found})
    end;

handle_message(Sock, {fire, Username, {TargetX, TargetY}}) ->
    case player_state:get_position(Username) of
        {ok, {X, Y}} ->
            Dx = TargetX - X,
            Dy = TargetY - Y,
            Dist = math:sqrt(Dx*Dx + Dy*Dy),
            NormDx = Dx / Dist,
            NormDy = Dy / Dist,

            case can_fire(Username) of
                true ->
                    projectiles:fire(Username, {X, Y}, {NormDx, NormDy}, 5.0),
                    set_cooldown(Username),
                    client_session:reply(Sock, {fired});
                false ->
                    client_session:reply(Sock, {error, cooldown})
            end;
        _ -> 
            client_session:reply(Sock, {error, user_not_found})
    end;

handle_message(Sock, _) ->
    client_session:reply(Sock, {error, "Invalid command"}).

mod_active(Username, green) ->
    player_data:set_effect(Username, projectile_speed, 2.0);
mod_active(Username, orange) ->
    player_data:set_effect(Username, projectile_speed, 0.5);
mod_active(Username, blue) ->
    player_data:set_effect(Username, projectile_speed, 0.5);
mod_active(Username, red) ->
    player_data:set_effect(Username, projectile_speed, 2.0).

set_cooldown(Username) ->
    ets:insert(player_effects, {Username++"_cooldown", erlang:system_time(millisecond)}).

can_fire(Username) ->
    case ets:lookup(player_effects, Username++"_cooldown") of
        [{_, Timestamp}] ->
            (erlang:system_time(millisecond) - Timestamp) > 1000; % 1 segundo
        [] -> true
    end.