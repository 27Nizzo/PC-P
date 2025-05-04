-module(account_server).
-export([
    start/0, create_account/2, is_logged_in/1, 
    online/0, remove_account/1,
    rpc/1, login/1, logout/1, auth/1,
    loop/1
]).

% Interface RPC
rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    after 5000 ->
        {error, timeout}
    end.

% Iniciar servidor com dados carregados (se existirem)
start() ->
    Users = readFile:readAccounts(),
    Pid = spawn(fun() -> loop(Users) end),
    register(?MODULE, Pid),
    {ok, started}.

% Loop principal do servidor
loop(Users) ->
    receive
        {Pid, {create_account, User, Pass}} ->
            case maps:is_key(User, Users) of
                true ->
                    Pid ! {error, user_exists},
                    loop(Users);
                false when Pass == "" ->
                    Pid ! {error, invalid_password},
                    loop(Users);
                false ->
                    NewUsers = maps:put(User, {Pass, 0, false}, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, created},
                    loop(NewUsers)
            end;

        {Pid, {remove_account, {User, Pass}}} ->
            case maps:find(User, Users) of
                {ok, {PassStored, _Score, _LoggedIn}} when PassStored == Pass ->
                    NewUsers = maps:remove(User, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, removed},
                    loop(NewUsers);
                {ok, _} ->
                    Pid ! {error, invalid_password},
                    loop(Users);
                _ ->
                    Pid ! {error, invalid_account},
                    loop(Users)
            end;

        {Pid, {login, {User, Pass}}} ->
            case maps:find(User, Users) of
                {ok, {PassStored, Score, false}} when PassStored == Pass ->
                    NewUsers = maps:put(User, {PassStored, Score, true}, Users),
                    Pid ! {ok, logged_in},
                    loop(NewUsers);
                {ok, {_, _, true}} ->
                    Pid ! {error, already_logged_in},
                    loop(Users);
                {ok, _} ->
                    Pid ! {error, invalid_password},
                    loop(Users);
                _ ->
                    Pid ! {error, invalid_account},
                    loop(Users)
            end;

        {Pid, {logout, {User, Pass}}} ->
            case maps:find(User, Users) of
                {ok, {PassStored, Score, true}} when PassStored == Pass ->
                    NewUsers = maps:put(User, {PassStored, Score, false}, Users),
                    Pid ! {ok, logged_out},
                    loop(NewUsers);
                {ok, _} ->
                    Pid ! {error, not_logged_in_or_wrong_password},
                    loop(Users);
                _ ->
                    Pid ! {error, invalid_account},
                    loop(Users)
            end;

        {Pid, {auth, {User, Pass}}} ->
            case maps:find(User, Users) of
                {ok, {StoredPass, _Score, true}} when StoredPass == Pass ->
                    Pid ! true;
                _ ->
                    Pid ! false
            end,
            loop(Users);

        {Pid, {is_logged_in, User}} ->
            case maps:get(User, Users, undefined) of
                {_, _, true} -> Pid ! true;
                _ -> Pid ! false
            end,
            loop(Users);

        {Pid, online} ->
            Online = [U || {U, {_, _, true}} <- maps:to_list(Users)],
            Pid ! Online,
            loop(Users);

        Unknown ->
            io:format("Mensagem desconhecida: ~p~n", [Unknown]),
            loop(Users)
    end.

create_account(User, Pass) -> rpc({create_account, User, Pass}).
remove_account({User, Pass}) -> rpc({remove_account, {User, Pass}}).
login({User, Pass}) -> rpc({login, {User, Pass}}).
logout({User, Pass}) -> rpc({logout, {User, Pass}}).
auth({User, Pass}) -> rpc({auth, {User, Pass}}).
is_logged_in(User) -> rpc({is_logged_in, User}).
online() -> rpc(online).
