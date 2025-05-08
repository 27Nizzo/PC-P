-module(player_state).


start() ->
    ets:new(player_pos, [name_table, public, set]),
    ets:new(player_effects, [name_table, puvblic, set]).

set_position(Username, {X, Y}) ->
    ets:insert(player_pos, {Username, {X, Y}}).

get_positon(Username) ->
    case ets:lookup(player_pos, Username) of 
        [{Username, Pos}] -> {ok, Pos};
        [] -> not_found
    end.

set_effect(Username, EffectKey, Value) ->
    Effects = case ets:lookup(player_effects, Username) of
        [{Username, Map}] -> maps:put(EffectKey, Value, Map);
        [] -> maps:from_list([{EffectKey, Value}])
    end,
    ets:insert(player_effects, {Username, Effects}).

get_effects(Username) ->
    case ets:lookup(player_effects, Username) of
        [{Username, Effects}] -> Effects;
        [] -> #{}
    end.

end_effects(Username) ->
    ets:delete(player_effects, Username).