-module(account_server).
-export([
    start/0, create_account/2, is_logged_in/1, 
    online/0, remove_account/1,
    rpc/1, login/1, logout/1, auth/1,
    loop/1, get_stats/1, update_stats/2, get_leaderboard/0
]).

-record(player, {
    username = undefined,
    nvl = 1,
    wins = 0,
    losses = 0,
    logged_in = false,
    current_streak = 0,
    is_in_win_streak = true,
    loss_streak = 0
}).

% Interface RPC
rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    after 5000 ->
        {error, timeout}
    end.

% Iniciar servidor com dados carregados
start() ->
    case whereis(?MODULE) of
        undefined ->
            Users = case readFile:readAccounts() of
                       {ok, U} -> U;
                       _ -> #{}
                    end,
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
                    Info = #{
                        password => Pass,
                        nvl => 1,
                        wins => 0,
                        losses => 0,
                        logged_in => false,
                        current_streak => 0,
                        is_in_win_streak => true,
                        loss_streak => 0
                    },
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
                Info = #{nvl := Nvl, wins := Wins, losses := Losses} ->
                    NewInfo = case Result of
                        win -> 
                            CurrentStreak = maps:get(current_streak, Info, 0) + 1,
                            NewNvl = case CurrentStreak >= Nvl of
                                true -> Nvl + 1;
                                false -> Nvl
                            end,
                            Info#{
                                nvl => NewNvl,
                                wins => Wins + 1,
                                current_streak => CurrentStreak,
                                is_in_win_streak => true,
                                loss_streak => 0
                            };
                        loss -> 
                            LossStreak = maps:get(loss_streak, Info, 0) + 1,
                            RequiredLosses = ceil(Nvl/2),
                            NewNvl = case LossStreak >= RequiredLosses of
                                true -> max(1, Nvl - 1);
                                false -> Nvl
                            end,
                            Info#{
                                nvl => NewNvl,
                                losses => Losses + 1,
                                current_streak => 0,
                                is_in_win_streak => false,
                                loss_streak => LossStreak
                            };
                        timeout_win ->
                            update_stats(User, win);
                        timeout_loss ->
                            update_stats(User, loss);
                        _ -> Info
                    end,
                    NewUsers = maps:put(User, NewInfo, Users),
                    readFile:writeAccounts(NewUsers),
                    Pid ! {ok, updated},
                    loop(NewUsers)
            end;

        {Pid, get_leaderboard} ->
            List = maps:to_list(Users),
            Sorted = lists:sort(fun compare_users/2, List),
            Formatted = [format_user(U, Info) || {U, Info} <- Sorted],
            Pid ! {ok, Formatted},
            loop(Users);

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
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
get_leaderboard() ->
    case rpc(get_leaderboard) of
        {ok, Leaderboard} -> lists:sublist(Leaderboard, 10);
        Error -> Error
    end.

compare_users({_, Info1}, {_, Info2}) ->
    Nvl1 = maps:get(nvl, Info1, 0),
    Nvl2 = maps:get(nvl, Info2, 0),
    case Nvl1 > Nvl2 of
        true -> true;
        false when Nvl1 < Nvl2 -> false;
        false ->
            Streak1 = maps:get(current_streak, Info1, 0),
            Streak2 = maps:get(current_streak, Info2, 0),
            Win1 = maps:get(is_in_win_streak, Info1, true),
            Win2 = maps:get(is_in_win_streak, Info2, true),
            compare_streak(Streak1, Win1, Streak2, Win2)
    end.

compare_streak(S1, true, S2, false) -> true;
compare_streak(S1, false, S2, true) -> false;
compare_streak(S1, _, S2, _) -> S1 >= S2.

format_user(User, Info) ->
    #{username => User,
      nvl => maps:get(nvl, Info, 1),
      wins => maps:get(wins, Info, 0),
      losses => maps:get(losses, Info, 0)}.