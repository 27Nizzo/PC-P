-module(modifiers).
-export([start/0, spawn_mod/0, get_at/1, remove/1, list_all/0]).

start() ->
    try
        case ets:info(mods) of
            undefined -> 
                ets:new(mods, [named_table, public, {keypos, 1}]);
            _ ->
                ets:delete_all_objects(mods)  % Limpa a tabela se já existir
        end,
        ok
    catch
        _:_ -> 
            ets:new(mods, [named_table, public, {keypos, 1}]),
            ok
    end.

spawn_mod() ->
    Types = [green, orange, blue, red],
    RandomType = lists:nth(rand:uniform(length(Types)), Types),
    Pos = {rand:uniform(20), rand:uniform(20)},
    ets:insert(mods, {Pos, RandomType}),
    {ok, {Pos, RandomType}}.

get_at(Pos) ->
    case ets:lookup(mods, Pos) of
        [{Pos, Type}] -> {ok, Type};
        [] -> not_found
    end.

remove(Pos) ->
    ets:delete(mods, Pos).

list_all() ->
    ets:tab2list(mods).
