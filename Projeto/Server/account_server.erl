-module(account_server).
-export([
    start/0, stop/0, create_account/2, is_logged_in/1, 
    online/0, remove_account/1, login/1, logout/1, 
    auth/1, get_stats/1, update_stats/2, get_leaderboard/0
]).

-define(TIMEOUT, 5000).
-define(FILENAME, "accounts.txt").

%%% Interface Pública %%%


start() ->
    case whereis(?MODULE) of
        undefined ->
            {ok, Users} = load_accounts(),
            Pid = spawn(fun() -> server_loop(Users) end),
            register(?MODULE, Pid),
            {ok, started};
        _Pid ->
            {error, already_started}
    end.

stop() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(?MODULE),
            ok
    end.

create_account(User, Pass) -> 
    rpc({create_account, User, Pass}).

remove_account({User, Pass}) -> 
    rpc({remove_account, {User, Pass}}).

login({User, Pass}) -> 
    rpc({login, {User, Pass}}).

logout({User, Pass}) -> 
    rpc({logout, {User, Pass}}).

auth({User, Pass}) -> 
    rpc({auth, {User, Pass}}).

is_logged_in(User) -> 
    rpc({is_logged_in, User}).

online() -> 
    rpc(online).

get_stats(User) -> 
    rpc({get_stats, User}).

update_stats(User, Result) -> 
    rpc({update_stats, User, Result}).

get_leaderboard() -> 
    case rpc(get_leaderboard) of
        {ok, Leaderboard} -> lists:sublist(Leaderboard, 10);
        Error -> Error
    end.

%%% Funções de Persistência %%%

load_accounts() ->
    case file:read_file(?FILENAME) of
        {ok, Data} ->
            try
                {ok, parse_accounts(binary_to_list(Data))}
            catch
                _:_ -> 
                    io:format("Erro ao ler arquivo, criando novo~n"),
                    {ok, #{}}
            end;
        {error, enoent} ->
            io:format("Arquivo não encontrado, criando novo~n"),
            {ok, #{}};
        {error, Reason} ->
            io:format("Erro ao ler arquivo: ~p~n", [Reason]),
            {ok, #{}}
    end.

parse_accounts(Data) ->
    Lines = string:split(Data, "\n", all),
    lists:foldl(fun parse_line/2, #{}, Lines).

parse_line(Line, Acc) ->
    case string:split(Line, ".", leading) of
        [Username, MapStr] ->
            case safe_parse_map(MapStr) of
                {ok, Map} -> 
                    io:format("Carregada conta: ~s~n", [Username]),
                    maps:put(Username, Map, Acc);
                error -> 
                    io:format("Linha ignorada: ~s~n", [Line]),
                    Acc
            end;
        _ -> 
            io:format("Linha mal formatada ignorada~n"),
            Acc
    end.

safe_parse_map(MapStr) ->
    try
        {ok, Tokens, _} = erl_scan:string(MapStr ++ "."),
        {ok, Term} = erl_parse:parse_term(Tokens),
        case is_map(Term) of
            true -> {ok, Term};
            false -> error
        end
    catch
        _:_ -> error
    end.

save_accounts(Accounts) ->
    TempFile = ?FILENAME ++ ".tmp",
    FinalFile = ?FILENAME,
    Content = lists:map(
        fun({User, Map}) -> 
            io_lib:format("~s.~p", [User, Map])
        end,
        maps:to_list(Accounts)),
    Formatted = string:join(Content, "\n"),
    % Escreve primeiro num arquivo temporário
    case file:write_file(TempFile, Formatted) of
        ok ->
            % Renomeia atomicamente
            file:rename(TempFile, FinalFile),
            ok;
        Error ->
            Error
    end.

%%% Loop Principal do Servidor %%%

server_loop(Users) ->
    receive
        {From, {create_account, User, Pass}} ->
            handle_create(From, User, Pass, Users);

        {From, {remove_account, {User, Pass}}} ->
            handle_remove(From, User, Pass, Users);

        {From, {login, {User, Pass}}} ->
            handle_login(From, User, Pass, Users);

        {From, {logout, {User, Pass}}} ->
            handle_logout(From, User, Pass, Users);

        {From, {auth, {User, Pass}}} ->
            handle_auth(From, User, Pass, Users);

        {From, {is_logged_in, User}} ->
            handle_is_logged_in(From, User, Users);

        {From, online} ->
            handle_online(From, Users);

        {From, {get_stats, User}} ->
            handle_get_stats(From, User, Users);

        {From, {update_stats, User, Result}} ->
            handle_update_stats(From, User, Result, Users);

        {From, get_leaderboard} ->
            handle_leaderboard(From, Users);

        stop ->
            save_accounts(Users),
            ok;

        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            server_loop(Users)
    end.

%%% Handlers Específicos %%%

handle_create(From, User, Pass, Users) ->
    case {maps:is_key(User, Users), Pass =:= ""} of
        {true, _} -> 
            From ! {error, user_exists},
            server_loop(Users);
        {_, true} -> 
            From ! {error, invalid_password},
            server_loop(Users);
        _ ->
            NewUser = #{
                username => User,
                password => Pass,
                nvl => 1,
                wins => 0,
                losses => 0,
                logged_in => false,
                current_streak => 0,
                is_in_win_streak => true,
                loss_streak => 0
            },
            NewUsers = maps:put(User, NewUser, Users),
            save_accounts(NewUsers),
            From ! {ok, created},
            server_loop(NewUsers)
    end.

handle_remove(From, User, Pass, Users) ->
    case maps:get(User, Users, undefined) of
        #{password := PassStored} when PassStored =:= Pass ->
            NewUsers = maps:remove(User, Users),
            save_accounts(NewUsers),
            From ! {ok, removed},
            server_loop(NewUsers);
        _ ->
            From ! {error, invalid_credentials},
            server_loop(Users)
    end.

handle_login(From, User, Pass, Users) ->
    case maps:get(User, Users, undefined) of
        #{password := PassStored, logged_in := false} when PassStored =:= Pass ->
            NewUser = maps:put(logged_in, true, maps:get(User, Users)),
            NewUsers = maps:put(User, NewUser, Users),
            save_accounts(NewUsers),
            From ! {ok, logged_in},
            server_loop(NewUsers);
        #{logged_in := true} ->
            From ! {error, already_logged_in},
            server_loop(Users);
        _ ->
            From ! {error, invalid_credentials},
            server_loop(Users)
    end.

handle_logout(From, User, Pass, Users) ->
    case maps:get(User, Users, undefined) of
        #{password := PassStored, logged_in := true} when PassStored =:= Pass ->
            NewUsers = maps:update(User, fun(M) -> M#{logged_in := false} end, Users),
            save_accounts(NewUsers),
            From ! {ok, logged_out},
            server_loop(NewUsers);
        _ ->
            From ! {error, invalid_credentials},
            server_loop(Users)
    end.

handle_auth(From, User, Pass, Users) ->
    case maps:get(User, Users, undefined) of
        #{password := PassStored, logged_in := true} when PassStored =:= Pass ->
            From ! true;
        _ ->
            From ! false
    end,
    server_loop(Users).

handle_is_logged_in(From, User, Users) ->
    case maps:get(User, Users, undefined) of
        #{logged_in := true} -> From ! true;
        _ -> From ! false
    end,
    server_loop(Users).

handle_online(From, Users) ->
    Online = [U || {U, #{logged_in := true}} <- maps:to_list(Users)],
    From ! Online,
    server_loop(Users).

handle_get_stats(From, User, Users) ->
    case maps:get(User, Users, undefined) of
        #{nvl := Nvl, wins := Wins, losses := Losses} ->
            From ! {ok, #{nvl => Nvl, wins => Wins, losses => Losses}};
        undefined ->
            From ! {error, user_not_found}
    end,
    server_loop(Users).

handle_update_stats(From, User, Result, Users) ->
    case maps:get(User, Users, undefined) of
        undefined ->
            From ! {error, user_not_found},
            server_loop(Users);
        #{nvl := Nvl, wins := Wins, losses := Losses} = UserData ->
            UpdatedData = case Result of
                win -> update_win_stats(UserData);
                loss -> update_loss_stats(UserData);
                _ -> UserData
            end,
            NewUsers = maps:put(User, UpdatedData, Users),
            save_accounts(NewUsers),
            From ! {ok, updated},
            server_loop(NewUsers)
    end.

handle_leaderboard(From, Users) ->
    Leaderboard = generate_leaderboard(Users),
    From ! {ok, Leaderboard},
    server_loop(Users).

%%% Funções Auxiliares %%%

update_win_stats(#{nvl := Nvl, wins := Wins, current_streak := Streak} = User) ->
    NewStreak = Streak + 1,
    NewNvl = case NewStreak >= Nvl of
        true -> Nvl + 1;
        false -> Nvl
    end,
    User#{
        nvl => NewNvl,
        wins => Wins + 1,
        current_streak => NewStreak,
        is_in_win_streak => true,
        loss_streak => 0
    }.

update_loss_stats(#{nvl := Nvl, losses := Losses, loss_streak := LossStreak} = User) ->
    NewLossStreak = LossStreak + 1,
    RequiredLosses = ceil(Nvl / 2),
    NewNvl = case NewLossStreak >= RequiredLosses of
        true -> max(1, Nvl - 1);
        false -> Nvl
    end,
    User#{
        nvl => NewNvl,
        losses => Losses + 1,
        current_streak => 0,
        is_in_win_streak => false,
        loss_streak => NewLossStreak
    }.

generate_leaderboard(Users) ->
    Sorted = lists:sort(
        fun({_, A}, {_, B}) -> 
            compare_users(A, B) 
        end, 
        maps:to_list(Users)),
    [format_leaderboard_entry(U, D) || {U, D} <- Sorted].

compare_users(#{nvl := NvlA, current_streak := StreakA, is_in_win_streak := WinA},
              #{nvl := NvlB, current_streak := StreakB, is_in_win_streak := WinB}) ->
    case NvlA > NvlB of
        true -> true;
        false when NvlA < NvlB -> false;
        false ->
            case {WinA, WinB} of
                {true, false} -> true;
                {false, true} -> false;
                _ -> StreakA >= StreakB
            end
    end.

format_leaderboard_entry(User, #{nvl := Nvl, wins := Wins, losses := Losses}) ->
    #{username => User, nvl => Nvl, wins => Wins, losses => Losses}.

rpc(Request) ->
    case whereis(?MODULE) of
        undefined -> {error, server_not_running};
        _ ->
            ?MODULE ! {self(), Request},
            receive
                Response -> Response
            after ?TIMEOUT ->
                {error, timeout}
            end
    end.