-module(readFile).
-export([readAccounts/0, parseList/2, writeAccounts/1, parse/1, parseUser/1]).

readAccounts() ->
    case file:read_file("accounts.txt") of
        {error, _Reason} ->
            {ok, NewFile} = file:open("accounts.txt", [write]),
            file:close(NewFile),
            #{};
        {ok, FileBin} ->
            FileStr = binary_to_list(FileBin),
            Lines = string:split(FileStr, "\n", all),
            case Lines of
                [""] -> #{};
                _ -> parseList(Lines, #{})
            end
    end.

parseList([], Acc) -> Acc;
parseList([H | T], Acc) ->
    case string:split(H, ".", all) of
        [Username, Password, EloStr] ->
            UsernameStr = Username,
            PasswordStr = Password,
            {EloInt, _} = string:to_integer(EloStr),
            NewAcc = maps:put(UsernameStr, {PasswordStr, EloInt, false}, Acc),
            parseList(T, NewAcc);
        _ ->
            parseList(T, Acc) % Ignora linhas malformadas
    end.

writeAccounts(Acc) ->
    Data = parse(maps:to_list(Acc)),
    file:write_file("accounts.txt", Data).

parseUser({Username, {Password, Elo, _LoggedIn}}) ->
    string:join([Username, Password, integer_to_list(Elo)], ".").

parse([]) -> "";
parse([H]) -> parseUser(H);
parse([H | T]) -> string:join([parseUser(H), parse(T)], "\n").
