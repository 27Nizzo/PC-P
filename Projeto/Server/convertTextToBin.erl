-module(convertTextToBin).
-export([convert/0]).

-define(OLD_FILE, "accounts.txt").
-define(NEW_FILE, "accounts.txt").

convert() ->
    case file:read_file(?OLD_FILE) of
        {ok, Bin} ->
            Lines = string:split(binary_to_list(Bin), "\n", all),
            UsersMap = parse_lines(Lines, #{}),
            ok = write_binary(UsersMap),
            io:format("Ficheiro convertido com sucesso.~n"),
            ok;
        {error, Reason} ->
            io:format("Erro a ler o ficheiro antigo: ~p~n", [Reason]),
            {error, Reason}
    end.

parse_lines([], Map) -> Map;
parse_lines(["" | Rest], Map) -> parse_lines(Rest, Map);  % Ignora linhas vazias
parse_lines([Line | Rest], Map) ->
    Parts = string:split(Line, ".", all),
    case Parts of
        [Username, Password, WinsStr, LevelStr] ->
            Wins = list_to_integer(WinsStr),
            Level = list_to_integer(LevelStr),
            UserData = #{password => Password, wins => Wins, nvl => Level},
            parse_lines(Rest, Map#{Username => UserData});
        _ ->
            io:format("Linha inválida ignorada: ~s~n", [Line]),
            parse_lines(Rest, Map)
    end.

write_binary(Map) ->
    Bin = term_to_binary(Map),
    file:write_file(?NEW_FILE, Bin).
