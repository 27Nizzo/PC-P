-module(player_state).
-export([start/0, set_position/2, get_position/1, set_effect/3, get_effects/1, end_effects/1, get_velocity/2, set_velocity/2, can_fire/1, set_cooldown/1]).

start() ->
    try
        case ets:info(player_pos) of
            undefined -> 
                ets:new(player_pos, [named_table, set, public]);
            _ -> ok
        end,
        case ets:info(player_effects) of
            undefined ->
                ets:new(player_effects, [named_table, set, public]);
            _ -> ok
        end,
        {ok, tables_ready}
    catch
        error:Reason ->
            {error, {table_creation_failed, Reason}}
    end.

set_position(Username, {X, Y}) ->
    ets:insert(player_pos, {Username, {X, Y}}).

get_position(Username) ->  
    case ets:lookup(player_pos, Username) of 
        [{Username, Pos}] -> {ok, Pos};
        [] -> {error, not_found}
    end.

set_effect(Username, EffectKey, Value) ->
    Effects = case ets:lookup(player_effects, Username) of
        [{Username, Map}] -> maps:put(EffectKey, Value, Map);
        [] -> #{EffectKey => Value}
    end,
    ets:insert(player_effects, {Username, Effects}).

get_effects(Username) ->
    case ets:lookup(player_effects, Username) of
        [{Username, Effects}] -> {ok, Effects};
        [] -> {ok, #{}}
    end.

end_effects(Username) ->
    ets:delete(player_effects, Username).

get_velocity(Vx, Vy) -> {ok, {Vx, Vy}}.

set_velocity(Username, {Vx, Vy}) ->
    ets:insert(player_pos, {Username, {Vx, Vy}}). 

set_cooldown(Username) ->
    ets:insert(player_effects, {Username++"_cooldown", erlang:system_time(millisecond)}).  % Corrigido: millisecond

can_fire(Username) ->
    case ets:lookup(player_effects, Username++"_cooldown") of
        [{_, Timestamp}] ->
            (erlang:system_time(millisecond) - Timestamp) > 1000;  % Corrigido: millisecond
        [] -> true
    end.