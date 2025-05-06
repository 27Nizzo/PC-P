-module(account_server).

-export([
    start/0, create_account/2, is_logged_in/1, 
    online/0, remove_account/1,
    rpc/1, login/1, logout/1, auth/1,
    loop/1, get_stats/1, update_stats/2
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
    case whereis(?MODULE) of
        undefined ->
            Users = readFile:readAccounts(),
            Pid = spawn(fun() -> loop(Users) end),
            register(?MODULE, Pid),
            {ok, started};
        _Pid ->
            {error, already_started}
    end.

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
                    Info = #{password => Pass, nvl => 1, wins => 0, losses => 0, logged_in => false},
                    NewUsers = maps:put(User, Info, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, created},
                    loop(NewUsers)
            end;

        {Pid, {remove_account, {User, Pass}}} ->
            case maps:get(User, Users, undefined) of
                undefined ->
                    Pid ! {error, invalid_account},
                    loop(Users);
                #{password := PassStored} when PassStored == Pass ->
                    NewUsers = maps:remove(User, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, removed},
                    loop(NewUsers);
                _ ->
                    Pid ! {error, invalid_password},
                    loop(Users)
            end;

        {Pid, {login, {User, Pass}}} ->
            case maps:get(User, Users, undefined) of
                undefined ->
                    Pid ! {error, invalid_account},
                    loop(Users);
                #{password := PassStored, logged_in := false} = Info when PassStored == Pass ->
                    NewInfo = Info#{logged_in => true},
                    NewUsers = maps:put(User, NewInfo, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, logged_in},
                    loop(NewUsers);
                #{logged_in := true} ->
                    Pid ! {error, logged_in},
                    loop(Users);
                _ ->
                    Pid ! {error, invalid_password},
                    loop(Users)
            end;

        {Pid, {logout, {User, Pass}}} ->
            case maps:get(User, Users, undefined) of
                undefined ->
                    Pid ! {error, invalid_account},
                    loop(Users);
                #{password := PassStored, logged_in := true} = Info when PassStored == Pass ->
                    NewInfo = Info#{logged_in => false},
                    NewUsers = maps:put(User, NewInfo, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, logged_out},
                    loop(NewUsers);
                _ ->
                    Pid ! {error, not_logged_in_or_wrong_password},
                    loop(Users)
            end;

        {Pid, {auth, {User, Pass}}} ->
            case maps:get(User, Users, undefined) of
                #{password := PassStored, logged_in := true} when PassStored == Pass ->
                    Pid ! true;
                _ ->
                    Pid ! false
            end,
            loop(Users);

        {Pid, {is_logged_in, User}} ->
            case maps:get(User, Users, undefined) of
                #{logged_in := true} ->
                    Pid ! true;
                _ ->
                    Pid ! false
            end,
            loop(Users);

        {Pid, online} ->
            Online = [U || {U, #{logged_in := true}} <- maps:to_list(Users)],
            Pid ! Online,
            loop(Users);

        {Pid, {get_stats, User}} ->
            case maps:get(User, Users, undefined) of
                undefined ->
                    Pid ! {error, user_not_found},
                    loop(Users);
                #{nvl := Nvl, wins := Wins, losses := Losses} ->
                    Pid ! {ok, #{nvl => Nvl, wins => Wins, losses => Losses}},
                    loop(Users)
            end;

        {Pid, {update_stats, User, Result}} ->
            case maps:get(User, Users, undefined) of
                undefined ->
                    Pid ! {error, not_found},
                    loop(Users);
                Info = #{password := Pass, nvl := Nvl, wins := Wins, losses := Losses, logged_in := LoggedIn} -> % LoggedIn and Pass unused
                    NewInfo = case Result of
                        win -> Info#{nvl => Nvl + 1, wins => Wins + 1};
                        loss -> Info#{losses => Losses + 1};
                        _ -> Info
                    end,
                    NewUsers = maps:put(User, NewInfo, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, updated},
                    loop(NewUsers)
            end;

        Unknown ->
            io:format("Mensagem desconhecida: ~p~n", [Unknown]),
            loop(Users)
    end.

% API pública
create_account(User, Pass) -> rpc({create_account, User, Pass}).
remove_account({User, Pass}) -> rpc({remove_account, {User, Pass}}).
login({User, Pass}) -> rpc({login, {User, Pass}}).
logout({User, Pass}) -> rpc({logout, {User, Pass}}).
auth({User, Pass}) -> rpc({auth, {User, Pass}}).
is_logged_in(User) -> rpc({is_logged_in, User}).
online() -> rpc(online).
get_stats(User) -> rpc({get_stats, User}).
update_stats(User, Result) -> rpc({update_stats, User, Result}).
