-module(projectiles).
-export([start/0, fire/4, update_all/0, get_all/0]).

start() ->
    ets:new(projectiles_table, [named_table, public, set]).

fire(Owner, {X, Y}, {DX, DY}, Speed) ->
    Id = erlang:unique_integer([positive]),
    Projectile = #{
        id => Id,
        owner => Owner,
        position => {X, Y},
        direction => {DX, DY},
        speed => Speed,
        timestamp => erlang:system_time(milisecond)
    },
    ets:insert(projectiles_table, Projectile),
    {ok, Projectile}.

update_all() ->
    Now = erlang:system_time(milisecond),
    Projectile = ets:tab2list(projectiles_table),
    lists:foreach(fun(P) -> update_projectile(P, Now) end, Projectile).

update_projectile(#{id := Id, position := {X, Y}, direction := {DX, DY}, speed := S, timestamp := T} = P, Now) ->
    DeltaT = (Now - T) / 1000,
    NewX = X + DX * S * DeltaT,
    NewY = Y + DY * S * DeltaT,
    case {NewX < 0 orelse NewX > 100, NewY < 0 orelse NewY > 100} of
        {true, _} -> ets:delete(projectiles_table, Id);
        {_, true} -> ets:delete(projectiles_table, Id);
        _ -> 
            ets:insert(projectiles_table, P#{
                position => {NewX, NewY},
                timestamp => Now
            })
    end.

get_all() ->
    ets:tab2list(projectiles_table).
