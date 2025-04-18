-module(login_manager).
-export([start/0, create_account/2, close_account/1, login/2, logout/1, online/0, is_logged_in/1]).


rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    end.

create_account(User, Pass) ->
    rpc({create_account, User, Pass}).

close_account(User) ->
    rpc({close_account, User}).

login(User, Pass) ->
    rpc({login, User, Pass}).

logout(User) ->
    rpc({logout, User}).

online() ->
    rpc(online).


start() ->
    Map = #{}, 
    Pid = spawn(fun() -> loop(Map) end),
    register(?MODULE, Pid).

loop(Map) ->
    receive
        {Pid, {create_account, U, P}} ->
            case maps:is_key(U, Map) of
                true ->
                    Pid ! {error, user_exists},
                    loop(Map);
                false ->
                    NewMap = maps:put(U, {P, false}, Map),
                    Pid ! {ok, created},
                    loop(NewMap)
            end;

        {Pid, {close_account, U}} ->
            case maps:is_key(U, Map) of
                true ->
                    NewMap = maps:remove(U, Map),
                    Pid ! {ok, removed},
                    loop(NewMap);
                false ->
                    Pid ! {error, user_not_found},
                    loop(Map)
            end;

        {Pid, {login, U, P}} ->
            case maps:find(U, Map) of
                {ok, {StoredPass, false}} when StoredPass =:= P ->
                    NewMap = maps:put(U, {P, true}, Map),
                    Pid ! {ok, logged_in},
                    loop(NewMap);
                {ok, {_, true}} ->
                    Pid ! {error, already_logged_in},
                    loop(Map);
                _ ->
                    Pid ! {error, invalid_credentials},
                    loop(Map)
            end;

        {Pid, {is_logged_in, User}} ->
                case maps:get(User, Map, undefined) of
                   {_, true} -> Pid ! true;
                    _ -> Pid ! false
                end,
                    loop(Map);


        {Pid, {logout, U}} ->
            case maps:find(U, Map) of
                {ok, {P, true}} ->
                    NewMap = maps:put(U, {P, false}, Map),
                    Pid ! {ok, logged_out},
                    loop(NewMap);
                _ ->
                    Pid ! {error, not_logged_in},
                    loop(Map)
            end;

        {Pid, online} ->
            OnlineUsers = [U || {U, {_, true}} <- maps:to_list(Map)],
            Pid ! OnlineUsers,
            loop(Map)
    end.

is_logged_in(User) ->
    rpc({is_logged_in, User}).


%
% 1º) Metes este comando para entrar no shell erlang: erl

% 2º) c(login_manager). -> Compila o ficheiro

% 3º) Começa o servidor:

% login_manager:start().

% 4º) Criar User:

% login_manager:create_account("afonso", "pw123").

% 5º) Login do user:
% login_manager:login("afonso", "pw123").

% 6º) Comando para ver quem está online no servidor:
% login_manager:online().

% 7º) Para dar logout ao user:
% login_manager:logout("afonso").

% 8º) Para encerrar/apagar o user:
% login_manager:close_account("afonso").

% Nota: Podes apagar o user sem dar logout ao mesmo