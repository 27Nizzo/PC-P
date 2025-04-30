-module(client_session).
-export([start/1]).

-record(state, {
    username = undefined,
    logged_in = false,
    in_duel = false,
    in_queue = false
}).

start(Socket) ->
    gen_tcp:send(Socket, <<"Bem-vindo! Escreva LOGIN, REGISTER ou UNREGISTER\n">>),
    loop(Socket, #state{}).

loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Input = clean_input(Line),
            case handle_input(Input, Socket, State) of
                {continue, NewState} -> 
                    loop(Socket, NewState);
                stop ->
                    ok
            end;
        {error, closed} ->
            handle_disconnect(State),
            ok;
        {error, Reason} ->
            io:format("Erro no socket: ~p~n", [Reason]),
            handle_disconnect(State),
            ok
    end.

handle_disconnect(#state{username = Username, logged_in = true}) ->
    login_manager:logout(Username),
    matchmaker:leave_queue(Username);
handle_disconnect(_) -> ok.

%%% Command handlers %%%
handle_input(<<"PLAY">>, Socket, State = #state{logged_in = true, username = Username}) ->
    case matchmaker:enter_queue(Username) of
        {ok, queued} ->
            gen_tcp:send(Socket, <<"Aguardando outro jogador...\n">>),
            {continue, State#state{in_queue = true, in_duel = false}};
        {ok, waiting} ->
            gen_tcp:send(Socket, <<"Duelo a iniciar...\n">>),
            {continue, State#state{in_duel = true, in_queue = false}};
        {error, already_in_queue} ->
            gen_tcp:send(Socket, <<"Já estás na fila de espera\n">>),
            {continue, State};
        {error, already_in_duel} ->
            gen_tcp:send(Socket, <<"Já estás em um duelo\n">>),
            {continue, State};
        _ ->
            gen_tcp:send(Socket, <<"Erro no matchmaking\n">>),
            {continue, State}
    end;

handle_input(<<"PLAY">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Precisa fazer LOGIN primeiro\n">>),
    {continue, State};

handle_input(<<"REGISTER">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Username:\n">>),
    {ok, ULine} = gen_tcp:recv(Socket, 0),
    Username = clean_input(ULine),
    gen_tcp:send(Socket, <<"Password:\n">>),
    {ok, PLine} = gen_tcp:recv(Socket, 0),
    Password = clean_input(PLine),
    case login_manager:create_account(Username, Password) of
        {ok, _} ->
            gen_tcp:send(Socket, <<"Registo concluído\n">>),
            {continue, State};
        {error, user_exists} ->
            gen_tcp:send(Socket, <<"Utilizador já existe\n">>),
            {continue, State}
    end;

handle_input(<<"LOGIN">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Username:\n">>),
    {ok, ULine} = gen_tcp:recv(Socket, 0),
    Username = clean_input(ULine),
    gen_tcp:send(Socket, <<"Password:\n">>),
    {ok, PLine} = gen_tcp:recv(Socket, 0),
    Password = clean_input(PLine),
    case login_manager:login(Username, Password) of
        {ok, logged_in} ->
            gen_tcp:send(Socket, <<"Login com sucesso!\n">>),
            {continue, State#state{username = Username, logged_in = true}};
        {error, Reason} ->
            Msg = case Reason of
                already_logged_in -> <<"Já autenticado noutro local\n">>;
                invalid_credentials -> <<"Credenciais inválidas\n">>;
                _ -> <<"Erro no login\n">>
            end,
            gen_tcp:send(Socket, Msg),
            {continue, State}
    end;

handle_input(<<"UNREGISTER">>, Socket, State = #state{logged_in = true, username = Username}) ->
    case login_manager:close_account(Username) of
        {ok, _} ->
            gen_tcp:send(Socket, <<"Conta removida\n">>),
            {continue, #state{}};
        {error, user_not_found} ->
            gen_tcp:send(Socket, <<"Utilizador não existe\n">>),
            {continue, State}
    end;

handle_input(<<"UNREGISTER">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Precisa estar autenticado\n">>),
    {continue, State};

handle_input(<<"ENDDUEL">>, Socket, State = #state{logged_in = true, username = Username}) ->
    case matchmaker:end_duel(Username) of
        {ok, ended} ->
            gen_tcp:send(Socket, <<"Duelo terminado\n">>),
            {continue, State#state{in_duel = false}};
        {error, not_in_duel} ->
            gen_tcp:send(Socket, <<"Não está em duelo\n">>),
            {continue, State}
    end;

handle_input(<<"ENDDUEL">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Precisa estar autenticado\n">>),
    {continue, State};

handle_input(<<"QUIT">>, Socket, State = #state{in_queue = true}) ->
    gen_tcp:send(Socket, <<"Use CANCEL para sair da fila\n">>),
    {continue, State};

handle_input(<<"QUIT">>, Socket, State = #state{in_duel = true}) ->
    gen_tcp:send(Socket, <<"Use ENDDUEL para terminar o duelo\n">>),
    {continue, State};

handle_input(<<"QUIT">>, Socket, _State) ->
    gen_tcp:send(Socket, <<"Adeus!\n">>),
    gen_tcp:close(Socket),
    stop;

handle_input(<<"SHUTDOWN">>, Socket, #state{username = "admin", logged_in = true}) ->
    gen_tcp:send(Socket, <<"Servidor a encerrar...\n">>),
    spawn(fun() -> timer:sleep(1000), init:stop() end),
    gen_tcp:close(Socket),
    stop;

handle_input(<<"SHUTDOWN">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Acesso negado\n">>),
    {continue, State};

handle_input(<<"CANCEL">>, Socket, State = #state{in_queue = true, username = Username}) ->
    case matchmaker:leave_queue(Username) of
        ok ->
            gen_tcp:send(Socket, <<"Saiu da fila com sucesso.\n">>),
            {continue, State#state{in_queue = false}};
        not_in_queue ->
            gen_tcp:send(Socket, <<"Não estás na fila de espera.\n">>),
            {continue, State#state{in_queue = false}};
        _ ->
            gen_tcp:send(Socket, <<"Erro ao sair da fila.\n">>),
            {continue, State}
    end;

handle_input(<<"CANCEL">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Não estás na fila de espera.\n">>),
    {continue, State};

handle_input(_, Socket, State) ->
    gen_tcp:send(Socket, <<"Comando inválido\n">>),
    {continue, State}.

clean_input(Line) ->
    list_to_binary(string:trim(binary_to_list(Line))).