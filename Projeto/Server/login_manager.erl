-module(login_manager).
-export([start/0, create_account/2, close_account/1, login/2, logout/1, online/0, is_logged_in/1]).

%% Interface de chamadas remotas (RPC)
rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    after 5000 ->
        {error, timeout}
    end.

%% Inicia o serviço e cria o usuário admin automaticamente
start() ->
    InitialMap = #{}, 
    MapWithAdmin = maps:put("admin", {"adminpw", false}, InitialMap), % Cria admin com senha "adminpw"
    Pid = spawn(fun() -> loop(MapWithAdmin) end),
    register(?MODULE, Pid),
    {ok, started}.

%% Loop principal do servidor
loop(Map) ->
    receive
        %% Criação de conta
        {Pid, {create_account, User, Pass}} ->
            case maps:is_key(User, Map) of
                true ->
                    Pid ! {error, user_exists},
                    loop(Map);
                false ->
                    NewMap = maps:put(User, {Pass, false}, Map),
                    Pid ! {ok, created},
                    loop(NewMap)
            end;

        %% Remoção de conta
        {Pid, {close_account, User}} ->
            case maps:is_key(User, Map) of
                true ->
                    NewMap = maps:remove(User, Map),
                    Pid ! {ok, removed},
                    loop(NewMap);
                false ->
                    Pid ! {error, user_not_found},
                    loop(Map)
            end;

        %% Login
        {Pid, {login, User, Pass}} ->
            case maps:find(User, Map) of
                {ok, {StoredPass, false}} when StoredPass =:= Pass ->
                    NewMap = maps:put(User, {Pass, true}, Map),
                    Pid ! {ok, logged_in},
                    loop(NewMap);
                {ok, {_, true}} ->
                    Pid ! {error, already_logged_in},
                    loop(Map);
                _ ->
                    Pid ! {error, invalid_credentials},
                    loop(Map)
            end;

        %% Logout
        {Pid, {logout, User}} ->
            case maps:find(User, Map) of
                {ok, {Pass, true}} ->
                    NewMap = maps:put(User, {Pass, false}, Map),
                    Pid ! {ok, logged_out},
                    loop(NewMap);
                _ ->
                    Pid ! {error, not_logged_in},
                    loop(Map)
            end;

        %% Lista de usuários online
        {Pid, online} ->
            OnlineUsers = [U || {U, {_, true}} <- maps:to_list(Map)],
            Pid ! OnlineUsers,
            loop(Map);

        %% Verifica se usuário está logado
        {Pid, {is_logged_in, User}} ->
            case maps:get(User, Map, undefined) of
                {_, true} -> Pid ! true;
                _ -> Pid ! false
            end,
            loop(Map);

        %% Mensagem desconhecida
        Unknown ->
            io:format("Mensagem desconhecida: ~p~n", [Unknown]),
            loop(Map)
    end.

%% Funções públicas (API)

create_account(User, Pass) -> rpc({create_account, User, Pass}).

close_account(User) -> rpc({close_account, User}).

login(User, Pass) -> rpc({login, User, Pass}).

logout(User) -> rpc({logout, User}).

online() -> rpc(online).

is_logged_in(User) -> rpc({is_logged_in, User}).