-module(readFile).
-export([readAccounts/0, writeAccounts/1]).

readAccounts() ->
    case file:read_file("accounts.txt") of
        {error, _Reason} ->
            io:format("Arquivo não encontrado. Criando arquivo 'accounts.txt'...~n"),
            file:write_file("accounts.txt", ""),
            #{};
        {ok, FileBin} ->
            FileStr = binary_to_list(FileBin),
            Lines = string:split(FileStr, "\n", all),
            parseList(Lines, #{})
    end.

parseList([], Acc) -> Acc;
parseList(["" | T], Acc) -> parseList(T, Acc); % Ignora linhas vazias
parseList([H | T], Acc) ->
    case string:split(H, ".", all) of
        [UsernameStr, MapStr] ->
            try
                Map = list_to_term(MapStr),
                NewAcc = maps:put(UsernameStr, Map, Acc),
                parseList(T, NewAcc)
            catch
                error:Error -> % Captura de erro mais detalhada
                    io:format("Erro a interpretar utilizador: ~p~nLinha com erro: ~p~n", [Error, H]),
                    parseList(T, Acc)
            end;
        _ ->
            io:format("Linha ignorada (formato inválido): ~p~n", [H]),
            parseList(T, Acc)
    end.

writeAccounts(AccMap) ->
    Lines = maps:to_list(AccMap),
    Data = lists:map(fun({Username, Info}) ->
        Username ++ "." ++ term_to_list(Info)
    end, Lines),
    Content = string:join(Data, "\n"),
    case file:write_file("accounts.txt", Content) of
        ok -> io:format("Conta guardada com sucesso.~n");
        {error, Reason} -> io:format("Erro ao salvar a conta: ~p~n", [Reason])
    end.

term_to_list(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

list_to_term(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
