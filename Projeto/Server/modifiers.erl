-module(modifiers).
-export([start/0, spawn_mod/0, get_at/1, remove/1, list_all/0]).


start() ->
    ets:new(mods, [name_table, public, set]).

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
