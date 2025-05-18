-module(server).
-export([start/0, accept_loop/1, handle_client/1]).

-define(PORT, 1234).

start() ->
    % Inicializa todas as tabelas ETS necessárias
    ets:new(player_sockets, [named_table, set, public]),
    ets:new(player_effects, [named_table, set, public]),
    
    % Inicia todos os módulos necessários
    player_state:start(),
    modifiers:start(),
    projectiles:start(),
    collisions:start(),
    account_server:start(),
    matchmaker:start(),
    
    % Inicia o game loop
    spawn(fun game_loop/0),
    
    % Configura o socket do servidor
    {ok, LSock} = gen_tcp:listen(?PORT, [
        binary, 
        {packet, 0}, 
        {active, false}, 
        {reuseaddr, true}
    ]),
    io:format("Server started on port ~p~n", [?PORT]),
    
    % Inicia o loop de aceitação de conexões
    spawn(?MODULE, accept_loop, [LSock]),
    ok.

game_loop() ->
    projectiles:update_all(),
    collisions:check_all(),
    timer:sleep(50), % ~20 FPS
    game_loop().

accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            Pid = spawn(?MODULE, handle_client, [Sock]),
            gen_tcp:controlling_process(Sock, Pid),
            accept_loop(LSock);
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            accept_loop(LSock)
    end.

handle_client(Sock) ->
    client_session:start(Sock, fun handle_message/2).

handle_message(Sock, {login, Username, Password}) ->
    case account_server:login({Username, Password}) of
        {ok, logged_in} ->
            ets:insert(player_sockets, {Username, Sock}),
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
    case account_server:is_logged_in(Username) of
        true ->
            case matchmaker:enter_queue(Username) of
                {ok, queued} ->
                    client_session:reply(Sock, {queue_joined});
                Error ->
                    client_session:reply(Sock, Error)
            end;
        false ->
            client_session:reply(Sock, {error, not_logged_in})
    end;

handle_message(Sock, {leave_queue, Username}) ->
    case matchmaker:leave_queue(Username) of
        {ok, left_queue} ->
            client_session:reply(Sock, {queue_left});
        Error ->
            client_session:reply(Sock, Error)
    end;

handle_message(Sock, {move, Username, Direction}) ->
    case {player_state:get_position(Username), player_state:get_velocity(Username)} of
        {{ok, {X, Y}}, {ok, {Vx, Vy}}} ->
            {Ax, Ay} = case Direction of
                up -> {0.0, 0.2};
                down -> {0.0, -0.2};
                left -> {-0.2, 0.0};
                right -> {0.2, 0.0}
            end,
            NVx = clamp(Vx + Ax, -2.0, 2.0),
            NVy = clamp(Vy + Ay, -2.0, 2.0),
            Nx = X + NVx,
            Ny = Y + NVy,
            player_state:set_velocity(Username, {NVx, NVy}),
            player_state:set_position(Username, {Nx, Ny}),
            client_session:reply(Sock, {moved, {Nx, Ny}, {NVx, NVy}});
        _ ->
            client_session:reply(Sock, {error, user_not_found})
    end;

handle_message(Sock, {fire, Username, {TargetX, TargetY}}) ->
    case player_state:get_position(Username) of
        {ok, {X, Y}} ->
            Dx = TargetX - X,
            Dy = TargetY - Y,
            Dist = math:sqrt(Dx*Dx + Dy*Dy),
            case can_fire(Username) andalso Dist > 0 of
                true ->
                    NormDx = Dx / Dist,
                    NormDy = Dy / Dist,
                    projectiles:fire(Username, {X, Y}, {NormDx, NormDy}, 5.0),
                    set_cooldown(Username),
                    client_session:reply(Sock, {fired});
                false ->
                    client_session:reply(Sock, {error, cooldown})
            end;
        _ ->
            client_session:reply(Sock, {error, user_not_found})
    end;

handle_message(Sock, {get_duel_time, Username}) ->
    case matchmaker:get_duel(Username) of
        {ok, DuelPid} ->
            case duel:get_state(DuelPid) of
                {ok, DuelState} ->
                    Start = element(2, DuelState),
                    Dur = element(3, DuelState),
                    Remaining = max(0, Start + Dur - erlang:system_time(second)),
                    client_session:reply(Sock, {duel_time, Remaining});
                _ ->
                    client_session:reply(Sock, {error, invalid_duel})
            end;
        _ ->
            client_session:reply(Sock, {error, not_in_duel})
    end;

handle_message(Sock, {duel_action, Username, Action}) ->
    case matchmaker:get_duel(Username) of
        {ok, DuelPid} ->
            handle_duel_action(DuelPid, Username, Action, Sock);
        _ ->
            client_session:reply(Sock, {error, not_in_duel})
    end;

handle_message(Sock, _) ->
    client_session:reply(Sock, {error, invalid_command}).

% Funções auxiliares
clamp(Value, Min, Max) when Value < Min -> Min;
clamp(Value, Min, Max) when Value > Max -> Max;
clamp(Value, _, _) -> Value.

set_cooldown(Username) ->
    ets:insert(player_effects, {Username++"_cooldown", erlang:system_time(millisecond)}).

can_fire(Username) ->
    case ets:lookup(player_effects, Username++"_cooldown") of
        [{_, Timestamp}] ->
            (erlang:system_time(millisecond) - Timestamp) > 1000; % 1 segundo
        [] -> true
    end.

handle_duel_action(DuelPid, Username, {fire, Target}, Sock) ->
    handle_message(Sock, {fire, Username, Target});

handle_duel_action(DuelPid, Username, {move, Direction}, Sock) ->
    handle_message(Sock, {move, Username, Direction}).