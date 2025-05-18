-module(setup).
-export([start/0, stop/0, init_all/0]).

start() ->
    io:format("=== Iniciando Sistema de Jogo ===~n~n"),
    try
        init_all(),
        io:format("~n=== Sistema pronto! ===~n"),
        ok
    catch
        Error:Reason:Stacktrace ->
            io:format("~n=== ERRO durante inicialização ===~n"),
            io:format("Tipo: ~p~nMotivo: ~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            stop(),
            {error, Reason}
    end.

init_all() ->
    % 1. Iniciar dependências
    io:format("1. Iniciando dependências...~n"),
    ensure_application(sasl),
    ensure_application(crypto),

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
    compile_modules(Modules),

    % 3. Iniciar serviços base
    io:format("~n3. Iniciando serviços base...~n"),
    start_core_services(),

    % 4. Iniciar servidor TCP
    io:format("~n4. Iniciando servidor TCP...~n"),
    case server:start() of
        {ok, _} -> 
            io:format("  Servidor TCP [OK]~n");
        {error, initialization_failed} ->
            io:format("  Servidor TCP [ERRO: initialization_failed]~n"),
            throw(initialization_failed);
        {error, Reason} -> 
            io:format("  Servidor TCP [ERRO: ~p]~n", [Reason]),
            throw({tcp_server_error, Reason})
    end.

ensure_application(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        Error -> throw({app_start_failed, App, Error})
    end.

compile_modules([]) -> ok;
compile_modules([Module|Rest]) ->
    case compile:file(Module) of
        {ok, _} -> 
            io:format("  ~-20s [OK]~n", [Module]);
        {error, Errors, Warnings} -> 
            io:format("  ~-20s [ERRO: ~p, WARN: ~p]~n", [Module, Errors, Warnings]);
        Error -> 
            io:format("  ~-20s [ERRO: ~p]~n", [Module, Error])
    end,
    compile_modules(Rest).

start_core_services() ->
    Services = [
        {account_server, "Servidor de Contas"},
        {player_state, "Estado de Jogadores"},
        {modifiers, "Sistema de Modificadores"},
        {projectiles, "Sistema de Projéteis"},
        {collisions, "Sistema de Colisões"},
        {matchmaker, "Sistema de Matchmaking"}
    ],
    lists:foreach(fun({Mod, Name}) -> start_service(Mod, Name) end, Services).

start_service(Module, Name) ->
    try
        case Module:start() of
            ok -> 
                io:format("  ~-20s [OK]~n", [Name]);
            {ok, _} -> 
                io:format("  ~-20s [OK]~n", [Name]);
            {error, already_started} -> 
                io:format("  ~-20s [ALREADY RUNNING]~n", [Name]);
            {error, table_already_exists} -> 
                io:format("  ~-20s [TABLE EXISTS - RECOVERED]~n", [Name]);
            {error, Reason} -> 
                io:format("  ~-20s [ERROR: ~p]~n", [Name, Reason]),
                throw({service_start_failed, Module});
            Other ->
                io:format("  ~-20s [UNEXPECTED: ~p]~n", [Name, Other]),
                throw({unexpected_return, Module})
        end
    catch
        error:badarg ->
            % Tratamento especial para erros de tabela ETS
            io:format("  ~-20s [TABLE EXISTS - RECOVERED]~n", [Name]);
        Error:Reason2 ->
            io:format("  ~-20s [CRASH: ~p]~n", [Name, {Error, Reason2}]),
            throw({service_crashed, Module})
    end.

stop() ->
    io:format("~n=== Parando servidor... ===~n"),
    server:stop(),
    io:format("Servidor parado com sucesso.~n").

% erl -make && erl -noshell -s setup start -> WSL/Bash % 
% erl -make; erl -noshell -s setup start -> PowerShell% 