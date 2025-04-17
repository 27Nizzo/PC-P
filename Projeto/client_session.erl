-module(client_session).
-export([start/1]).

-record(state, {
    username = undefined,
    logged_in = false
}).

start(Socket) ->
    gen_tcp:send(Socket, <<"Bem-vindo! Escreva LOGIN, REGISTER ou UNREGISTER\n">>),
    loop(Socket, #state{}).

loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Input = clean_input(Line),
            NewState = handle_input(Input, Socket, State),
            loop(Socket, NewState);
        {error, closed} ->
            io:format("Cliente desconectado~n"),
            ok
    end.

%%% COMANDOS AUTENTICADOS %%%

handle_input(<<"PLAY">>, Socket, State = #state{logged_in = true, username = Username}) ->
    case matchmaker:enter_queue(Username) of
        {ok, queued} ->
            gen_tcp:send(Socket, <<"Aguardando outro jogador...\n">>);
        {ok, waiting} ->
            gen_tcp:send(Socket, <<"Duelo a iniciar...\n">>);
        Other ->
            io:format("Resposta inesperada do matchmaker: ~p~n", [Other]),
            gen_tcp:send(Socket, <<"Erro no matchmaking.\n">>)
    end,
    State;

handle_input(<<"PLAY">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Precisa de fazer LOGIN antes de jogar.\n">>),
    State;

%%% REGISTO DE UTILIZADOR %%%

handle_input(<<"REGISTER">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Username:\n">>),
    {ok, ULine} = gen_tcp:recv(Socket, 0),
    Username = clean_input(ULine),
    gen_tcp:send(Socket, <<"Password:\n">>),
    {ok, PLine} = gen_tcp:recv(Socket, 0),
    Password = clean_input(PLine),
    case login_manager:create_account(Username, Password) of
        {ok, _} ->
            gen_tcp:send(Socket, <<"Registo concluído. Faça LOGIN para continuar.\n">>),
            State;
        {error, user_exists} ->
            gen_tcp:send(Socket, <<"Utilizador já existe.\n">>),
            State
    end;

%%% LOGIN DO UTILIZADOR %%%

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
            State#state{username = Username, logged_in = true};
        {error, Reason} ->
            ReasonMsg = case Reason of
                already_logged_in -> <<"Já está autenticado noutro local.\n">>;
                invalid_credentials -> <<"Credenciais inválidas.\n">>;
                _ -> <<"Erro ao fazer login.\n">>
            end,
            gen_tcp:send(Socket, ReasonMsg),
            State
    end;

%%% REMOVER CONTA (apenas se autenticado) %%%

handle_input(<<"UNREGISTER">>, Socket, State = #state{username = Username, logged_in = true}) ->
    case login_manager:close_account(Username) of
        {ok, _} ->
            gen_tcp:send(Socket, <<"Conta removida com sucesso. Adeus!\n">>),
            #state{};
        {error, user_not_found} ->
            gen_tcp:send(Socket, <<"Utilizador não encontrado.\n">>),
            State
    end;

handle_input(<<"UNREGISTER">>, Socket, State) ->
    gen_tcp:send(Socket, <<"Tem de estar autenticado para remover a conta.\n">>),
    State;

%%% COMANDO INVÁLIDO %%%

handle_input(_, Socket, State) ->
    gen_tcp:send(Socket, <<"Comando inválido. Use LOGIN, REGISTER, UNREGISTER ou PLAY\n">>),
    State.

%%% FUNÇÃO AUXILIAR %%%

clean_input(Line) ->
    list_to_binary(string:trim(binary_to_list(Line))).
