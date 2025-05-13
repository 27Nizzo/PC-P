-module(player_state).
-export([start/0, set_position/2, get_position/1, set_effect/3, get_effects/1, end_effects/1, get_velocity/2, set_velocity/2]).

start() ->
    ets:new(player_pos, [named_table, public, set]),  
    ets:new(player_effects, [named_table, public, set]).  

set_position(Username, {X, Y}) ->
    ets:insert(player_pos, {Username, {X, Y}}).

get_position(Username) ->  
    case ets:lookup(player_pos, Username) of 
        [{Username, Pos}] -> {ok, Pos};
        [] -> {error, not_found}  % More consistent with Erlang conventions
    end.

set_effect(Username, EffectKey, Value) ->
    Effects = case ets:lookup(player_effects, Username) of
        [{Username, Map}] -> maps:put(EffectKey, Value, Map);
        [] -> #{EffectKey => Value}  % More efficient than maps:from_list for single key
    end,
    ets:insert(player_effects, {Username, Effects}).

get_effects(Username) ->
    case ets:lookup(player_effects, Username) of
        [{Username, Effects}] -> {ok, Effects};
        [] -> {ok, #{}}  % Wrapped in {ok, _} for consistency
    end.

end_effects(Username) ->
    ets:delete(player_effects, Username).

get_velocity(Vx,Vy) -> {ok, {Vx, Vy}}.
set_velocity(Username, {Vx, Vy}) ->
    ets:insert(player_pos, {Username, {Vx, Vy}}). 