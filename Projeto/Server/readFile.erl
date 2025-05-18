-module(readFile).
-export([readAccounts/0, writeAccounts/1]).

readAccounts() ->
    case file:read_file("accounts.txt") of
        {ok, Data} ->
            try
                % Remove quebras de linha e transforma em termo Erlang
                Trimmed = string:trim(binary_to_list(Data)),
                {ok, Tokens, _} = erl_scan:string(Trimmed ++ "."),
                {ok, Term} = erl_parse:parse_term(Tokens),
                {ok, Term}
            catch
                _:_ -> 
                    io:format("Erro ao ler accounts.txt, criando novo~n"),
                    {ok, #{}}
            end;
        {error, enoent} ->
            {ok, #{}};
        Error ->
            Error
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

writeAccounts(Accounts) ->
    % Garante que o arquivo é sobrescrito com os dados corretos
    file:write_file("accounts.txt", io_lib:format("~p.~n", [Accounts]), [write, binary]).

term_to_list(Term) ->
    lists:flatten(io_lib:format("~p", [Term])).

list_to_term(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
