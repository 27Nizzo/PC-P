-module(server).
-export([start/0, stop/0, accept_loop/1, handle_client/1]).

-define(PORT, 1234).

start() ->
    io:format("Iniciando servidor na porta ~p...~n", [?PORT]),
    
    % Inicializa serviços essenciais com tratamento robusto
    case initialize_services() of
        {error, Reason} ->
            io:format("Falha na inicialização: ~p~n", [Reason]),
            {error, initialization_failed};
        ok ->
            start_tcp_server()
    end.

stop() ->
    io:format("Parando servidor...~n"),
    % Limpeza de todos os serviços
    stop_services(),
    init:stop().

initialize_services() ->
    Services = [
        {fun player_state:start/0, "Player State"},
        {fun modifiers:start/0, "Modifiers"},
        {fun projectiles:start/0, "Projectiles"},
        {fun collisions:start/0, "Collisions"},
        {fun account_server:start/0, "Account Server"},
        {fun matchmaker:start/0, "Matchmaker"}
    ],
    
    start_services(Services).

start_services([]) -> ok;
start_services([{ServiceFun, Name}|Rest]) ->
    io:format("Iniciando ~s... ", [Name]),
    try
        case ServiceFun() of
            ok -> 
                io:format("[OK]~n"),
                start_services(Rest);
            {ok, _} -> 
                io:format("[OK]~n"),
                start_services(Rest);
            {error, already_started} -> 
                io:format("[ALREADY RUNNING]~n"),
                start_services(Rest);
            {error, table_already_exists} ->
                io:format("[TABLE EXISTS - RECOVERED]~n"),
                start_services(Rest);
            {error, Reason} -> 
                io:format("[ERROR: ~p]~n", [Reason]),
                {error, {service_start_failed, Name}};
            Other ->
                io:format("[UNEXPECTED: ~p]~n", [Other]),
                start_services(Rest)
        end
    catch
        Error:CatchReason ->
            io:format("[CRASH: ~p]~n", [{Error, CatchReason}]),
            {error, {service_crashed, Name}}
    end.

stop_services() ->
    lists:foreach(fun(Mod) ->
        try Mod:stop() catch _:_ -> ok end
    end, [matchmaker, collisions, projectiles, modifiers, player_state, account_server]).

start_tcp_server() ->
    case gen_tcp:listen(?PORT, [
        binary, 
        {packet, 0}, 
        {active, false}, 
        {reuseaddr, true},
        {backlog, 50}
    ]) of
        {ok, ListenSocket} ->
            io:format("Socket de escuta criado com sucesso~n"),
            spawn(?MODULE, accept_loop, [ListenSocket]),
            {ok, ListenSocket};
        {error, Reason} ->
            io:format("Erro ao criar socket: ~p~n", [Reason]),
            {error, socket_creation_failed}
    end.

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Nova conexão aceite~n"),
            Pid = spawn(?MODULE, handle_client, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            accept_loop(ListenSocket);
        {error, closed} ->
            io:format("Socket de escuta fechado~n");
        {error, Reason} ->
            io:format("Erro ao aceitar conexão: ~p~n", [Reason]),
            accept_loop(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            try binary_to_term(Data) of
                Message ->
                    handle_message(Socket, Message),
                    handle_client(Socket)
            catch
                error:badarg ->
                    io:format("Mensagem inválida recebida~n"),
                    gen_tcp:send(Socket, term_to_binary({error, invalid_message})),
                    handle_client(Socket)
            end;
        {error, closed} ->
            io:format("Cliente desconectado~n");
        {error, Reason} ->
            io:format("Erro na conexão: ~p~n", [Reason])
    end.

handle_message(Socket, {register, Username, Password}) ->
    case account_server:create_account(Username, Password) of
        {ok, created} ->
            gen_tcp:send(Socket, term_to_binary({register_success}));
        {error, Reason} ->
            gen_tcp:send(Socket, term_to_binary({register_failed, Reason}))
    end;

handle_message(Socket, {login, Username, Password}) ->
    case account_server:login({Username, Password}) of
        {ok, logged_in} ->
            ets:insert(player_sockets, {Username, Socket}),
            gen_tcp:send(Socket, term_to_binary({login_success}));
        {error, Reason} ->
            gen_tcp:send(Socket, term_to_binary({login_failed, Reason}))
    end;

handle_message(Socket, {join_queue, Username}) ->
    case account_server:is_logged_in(Username) of
        true ->
            case matchmaker:enter_queue(Username) of
                {ok, queued} ->
                    gen_tcp:send(Socket, term_to_binary({queue_joined}));
                Error ->
                    gen_tcp:send(Socket, term_to_binary(Error))
            end;
        false ->
            gen_tcp:send(Socket, term_to_binary({error, not_logged_in}))
    end;

handle_message(Socket, {move, Username, Direction}) ->
    case player_state:get_position(Username) of
        {ok, {X, Y}} ->
            {Vx, Vy} = calculate_velocity(Direction),
            player_state:set_position(Username, {X + Vx, Y + Vy}),
            player_state:set_velocity(Username, {Vx, Vy}),
            gen_tcp:send(Socket, term_to_binary({moved, {X + Vx, Y + Vy}}));
        {error, Reason} ->
            gen_tcp:send(Socket, term_to_binary({error, Reason}))
    end;

handle_message(Socket, {fire, Username, Target}) ->
    case player_state:can_fire(Username) of
        true ->
            case player_state:get_position(Username) of
                {ok, Position} ->
                    projectiles:fire(Username, Position, Target, 5.0),
                    player_state:set_cooldown(Username),
                    gen_tcp:send(Socket, term_to_binary({fired}));
                {error, Reason} ->
                    gen_tcp:send(Socket, term_to_binary({error, Reason}))
            end;
        false ->
            gen_tcp:send(Socket, term_to_binary({error, cooldown}))
    end;

handle_message(Socket, _Unknown) ->
    gen_tcp:send(Socket, term_to_binary({error, unknown_command})).

calculate_velocity(up) -> {0, 1};
calculate_velocity(down) -> {0, -1};
calculate_velocity(left) -> {-1, 0};
calculate_velocity(right) -> {1, 0};
calculate_velocity(_) -> {0, 0}.