-module(client_session).
-export([start/2, reply/2, stop/1]).

-record(session_state, {
    socket,
    username = undefined,
    logged_in = false
}).

start(Sock, Handler) ->
    Pid = spawn_link(fun() -> 
        State = #session_state{socket=Sock},
        loop(State, Handler) 
    end),
    {ok, Pid}.

reply(Sock, Msg) ->
    try
        gen_tcp:send(Sock, term_to_binary(Msg))
    catch
        _:_ -> 
            io:format("Erro ao enviar mensagem para socket ~p~n", [Sock]),
            {error, send_failed}
    end.

stop(Pid) ->
    Pid ! stop,
    ok.

loop(State, Handler) ->
    receive
        stop ->
            gen_tcp:close(State#session_state.socket),
            io:format("Sessão encerrada para ~p~n", [State#session_state.username]);
        
        {tcp, Socket, Data} ->
            NewState = handle_data(Socket, binary_to_term(Data), State, Handler),
            loop(NewState, Handler);
            
        {tcp_closed, _Socket} ->
            io:format("Conexão fechada por ~p~n", [State#session_state.username]),
            gen_tcp:close(State#session_state.socket);
            
        Unknown ->
            io:format("Mensagem desconhecida: ~p~n", [Unknown]),
            loop(State, Handler)
    end.

handle_data(Socket, Data, State, Handler) ->
    try
        case Data of
            {register, Username, Password} ->
                case Handler(Socket, {register, Username, Password}) of
                    {ok, registered} ->
                        State#session_state{username=Username};
                    Error ->
                        reply(Socket, Error),
                        State
                end;
                
            {login, Username, Password} ->
                case Handler(Socket, {login, Username, Password}) of
                    {ok, logged_in} ->
                        reply(Socket, {login_success}),
                        State#session_state{username=Username, logged_in=true};
                    Error ->
                        reply(Socket, Error),
                        State
                end;
                
            {logout, Username} when State#session_state.username =:= Username ->
                reply(Socket, {logout_success}),
                State#session_state{logged_in=false};
                
            {join_queue, Username} when State#session_state.logged_in ->
                Handler(Socket, {join_queue, Username}),
                State;
                
            {move, Direction} when State#session_state.logged_in ->
                Handler(Socket, {move, State#session_state.username, Direction}),
                State;
                
            {fire, Target} when State#session_state.logged_in ->
                Handler(Socket, {fire, State#session_state.username, Target}),
                State;
                
            _ when not State#session_state.logged_in ->
                reply(Socket, {error, not_authenticated}),
                State;
                
            _ ->
                reply(Socket, {error, invalid_command}),
                State
        end
    catch
        error:Reason ->
            io:format("Erro ao processar dados: ~p~n", [Reason]),
            reply(Socket, {error, internal_error}),
            State
    end.