-module(server).
-export([start/0, accept_loop/1]).

-define(PORT, 1234).

start() ->
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

handle_movement(Sock) ->