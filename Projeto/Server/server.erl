-module(server).
-export([start/0, accept_loop/1]).

-define(PORT, 1234).

start() ->
    player_state:start(),
    modifiers:start(),
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor iniciado na porta ~p~n", [?PORT]),
    spawn(?MODULE, accept_loop, [LSock]).

accept_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, handle_client, [Sock]),
    gen_tcp:controlling_process(Sock, Pid),
    accept_loop(LSock).

handle_client(Sock) ->
    client_session:start(Sock, fun handle_message/2).

handle_message(Sock, {login, Username, Password}) ->
    case account_server:login({Username, Password}) of
        {ok, logged_in} ->
            client_session:reply(Sock, {login_success});
        {error, Reason} ->
            client_session:reply(Sock, {login_failed, Reason})
    end;

handle_message(Sock, {register, Username, Password}) ->
    case account_server:create_account(Username, Password) of
        {ok, created} ->
            client_session:reply(Sock, register_success);
        {error, Reason} ->
            client_session:reply(Sock, {register_failed, Reason})
    end;

handle_message(Sock, {join_queue, Username}) ->
    matchmaker:add_player(Username, Sock),
    client_session:reply(Sock, queue_joined);

handle_message(Sock, {duel_result, Username, Result}) ->
    duel:report_result(Username, Result),
    client_session:reply(Sock, result_recorded);

handle_message(Sock, {info, Username}) ->
    case account_server:get_stats(Username) of
        {ok, Stats} ->
            client_session:reply(Sock, {info, Stats});
        {error, not_found} ->
            client_session:reply(Sock, {error, "Utilizador não encontrado"})
    end;

handle_message(Sock, _) ->
    client_session:reply(Sock, {error, "Comando inválido"}).

% Registar o movimentp
handle_movement(Sock, {move, Username, Direction}) ->
    case player_data:get_positon(Username) of
        {ok, {X, Y}} -> 
            NewPos = case Direction of
                up -> {X, Y + 1},
                down -> {X, Y - 1},
                left -> {X - 1, Y},
                right -> {X + 1, Y}
            end,
            player_data:set_position(Username, NewPos),
            client_session:reply(Sock, {moved, NewPos});
        not_found ->
            client_session:reply(Sock, {error, user_not_found})
    end.

% Handle para apanhar o Modifier

handle_message(Sock, {pickup_mod, Username}) ->
    case player_data:get_positon(Username) of
        {ok, Pos} -> 
            case modifiers: get_at(Pos) of
                {ok, Type} ->
                    modifiers:remove(Pos),
                    apply_mod(Username, Type),
                    client_session:reply(Sock, {mod_active, Type});
                    not_found -> 
                        client_session:reply(Sock, {no_powerup})
            end;
        not_found ->
            client_session:reply(Sock, {error, user_not_found})
    end.

mod_active(Username, green) ->
    player_data:set_effect(Username, projectile_speed, 2.0);

mod_active(Username, orage) ->
    player_data:set_effect(Username, projectile_speed, 0.5);

mod_active(Username, blue) ->
    player_data:set_effect(Username, projectile_speed, 0.5);

mod_active(Username, red) ->
    player_data:set_effect(Username, projectile_speed, 2.0);
