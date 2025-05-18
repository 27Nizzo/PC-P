-module(projectiles).
-export([start/0, fire/4, update_all/0, get_all/0, remove/1]).

start() ->
    try
        ets:new(projectiles_table, [named_table, public, set]),
        {ok, projectiles_started}
    catch
        error:badarg -> 
            {error, table_already_exists}
    end.

fire(Owner, {X, Y}, {DX, DY}, Speed) ->
    Id = erlang:unique_integer([positive]),
    Projectile = #{
        id => Id,
        owner => Owner,
        position => {X, Y},
        direction => {DX, DY},
        speed => Speed,
        timestamp => erlang:system_time(millisecond)  % Corrigido: 'millisecond' em vez de 'milisecond'
    },
    ets:insert(projectiles_table, Projectile),
    {ok, Projectile}.

update_all() ->
    Now = erlang:system_time(millisecond),  % Corrigido aqui também
    Projectiles = ets:tab2list(projectiles_table),  % Corrigido nome da variável
    lists:foreach(fun(P) -> update_projectile(P, Now) end, Projectiles).

update_projectile(#{id := Id, position := {X, Y}, direction := {DX, DY}, speed := S, timestamp := T} = P, Now) ->
    DeltaT = (Now - T) / 1000,
    NewX = X + DX * S * DeltaT,
    NewY = Y + DY * S * DeltaT,
    
    % Verifica se o projétil saiu dos limites do mapa (ajuste os valores conforme necessário)
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

remove(Id) ->
    ets:delete(projectiles_table, Id).