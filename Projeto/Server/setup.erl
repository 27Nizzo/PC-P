-module(setup).
-export([start/0, stop/0, init_all/0]).

start() ->
    io:format("=== Iniciando Sistema de Jogo ===~n~n"),
    try
        init_all(),
        io:format("~n=== Sistema pronto! ===~n")
    catch
        Error:Reason:Stacktrace ->
            io:format("~n=== ERRO durante inicialização ===~n"),
            io:format("Tipo: ~p~nMotivo: ~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            stop()
    end.

init_all() ->
    % 1. Iniciar dependências
    io:format("1. Iniciando dependências...~n"),
    application:start(sasl),
    application:start(crypto),

    % 2. Compilar todos os módulos
    io:format("2. Compilando módulos...~n"),
    Modules = [
        account_server,
        client_session,
        collisions,
        convertTextToBin,
        duel,
        matchmaker,
        modifiers,
        player_state,
        projectiles,
        readFile,
        server
    ],
    lists:foreach(fun(M) ->
        case compile:file(M) of
            {ok, _} -> io:format("  ~-20s [OK]~n", [M]);
            Error -> io:format("  ~-20s [ERRO: ~p]~n", [M, Error])
        end
    end, Modules),

    % 3. Iniciar serviços base
    io:format("~n3. Iniciando serviços base...~n"),
    Services = [
        {account_server, "Servidor de Contas"},
        {player_state, "Estado de Jogadores"},
        {modifiers, "Sistema de Modificadores"},
        {projectiles, "Sistema de Projéteis"},
        {collisions, "Sistema de Colisões"},
        {matchmaker, "Sistema de Matchmaking"}
    ],
    lists:foreach(fun({Mod, Name}) -> start_service(Mod, Name) end, Services),

    % 4. Iniciar servidor TCP
    io:format("~n4. Iniciando servidor TCP...~n"),
    case server:start() of
        {ok, _} -> io:format("  Servidor TCP [OK]~n");
        Error -> io:format("  Servidor TCP [ERRO: ~p]~n", [Error])
    end.

start_service(Module, Name) ->
    try
        case Module:start() of
            {ok, _} -> io:format("  ~-20s [OK]~n", [Name]);
            {error, already_started} -> 
                io:format("  ~-20s [JÁ INICIADO]~n", [Name]);
            Other -> 
                io:format("  ~-20s [ERRO: ~p]~n", [Name, Other]),
                throw({service_start_failed, Module})
        end
    catch
        Error:Reason ->
            io:format("  ~-20s [CRASH: ~p]~n", [Name, {Error, Reason}]),
            throw({service_crashed, Module})
    end.

stop() ->
    io:format("~n=== Parando servidor... ===~n"),
    lists:foreach(fun
        ({RegName, _Pid}) -> 
            try 
                case RegName of
                    account_server -> account_server ! stop;
                    _ -> ok
                end
            catch _:_ -> ok end;
        (_) -> ok
    end, registered()),
    init:stop(),
    io:format("Servidor parado com sucesso.~n").

% erl -make && erl -noshell -s setup start -> WSL/Bash % 
% erl -make; erl -noshell -s setup start -> PowerShell% 