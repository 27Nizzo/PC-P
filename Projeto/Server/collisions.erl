-module(collisions).
-export([start/0, check_all/0, get_walls/0, distance/2]).

start() ->
    % Paredes do jogo (coordenadas x, y, largura, altura) METER AS MEDIDAS CERTAS 
    Walls = [
        {0, 0, 100, 5},    % Parede superior
        {0, 95, 100, 5},    % Parede inferior
        {0, 0, 5, 100},     % Parede esquerda
        {95, 0, 5, 100}     % Parede direita
    ],
    ets:new(walls_table, [named_table, set, public]),
    ets:insert(walls_table, {walls, Walls}),
    ok.

check_all() ->
    Players = ets:tab2list(player_pos),
    Projectiles = projectiles:get_all(),
    Walls = get_walls(),
    
    check_projectiles(Projectiles, Players, Walls),
    check_players(Players, Walls).

check_projectiles([], _, _) -> ok;
check_projectiles([P | Rest], Players, Walls) ->
    check_projectile(P, Players, Walls),
    check_projectiles(Rest, Players, Walls).

check_projectile(#{id := Id, owner := Owner, position := {PX, PY}} = Proj, Players, Walls) ->
    % Verificar colisão com jogadores
    lists:foreach(fun({Username, {X, Y}}) ->
        Dist = distance({PX, PY}, {X, Y}),
        if 
            Username /= Owner andalso Dist < 2.0 ->
                handle_hit(Username, Proj),
                projectiles:remove(Id);
            true -> ok
        end
    end, Players),    
    % Verificar colisão com paredes
    lists:foreach(fun({WX, WY, W, H}) ->
        if
            PX >= WX andalso PX =< WX+W andalso
            PY >= WY andalso PY =< WY+H ->
                projectiles:remove(Id);
            true -> ok
        end
    end, Walls).

check_players(Players, Walls) ->
    lists:foreach(fun({Username, {X, Y}}) ->
        lists:foreach(fun({WX, WY, W, H}) ->
            if
                X >= WX andalso X =< WX+W andalso
                Y >= WY andalso Y =< WY+H ->
                    handle_wall_collision(Username);
                true -> ok
            end
        end, Walls)
    end, Players).

handle_hit(Username, #{owner := Owner}) ->
    case duel:get_duel(Owner) of
        {ok, DuelPid} ->
            duel:add_point(DuelPid, opponent_of(Owner, Username), 1);
        _ -> ok
    end.

handle_wall_collision(Username) ->
    case duel:get_duel(Username) of
        {ok, DuelPid} ->
            Opponent = opponent_of(Username),
            duel:add_point(DuelPid, Opponent, 2),
            reset_position(Username);
        _ -> ok
    end.

reset_position(Username) ->
    % Posição inicial aleatória (exemplo)
    X = rand:uniform(80) + 10,
    Y = rand:uniform(80) + 10,
    player_state:set_position(Username, {X, Y}).

get_walls() ->
    case ets:lookup(walls_table, walls) of
        [{walls, Walls}] -> Walls;
        [] -> []
    end.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

opponent_of(Player) ->
    % Retorna o oponente de Player em um duelo 1v1
    case duel:get_players(Player) of
        [Player, Opponent] -> Opponent;
        [Opponent, Player] -> Opponent;
        _ -> undefined
    end.

opponent_of(Player1, _) ->
    % Em um duelo 1v1, o oponente é o outro jogador
    case duel:get_players(Player1) of
        [Player1, Opponent] -> Opponent;
        [Opponent, Player1] -> Opponent;
        _ -> undefined
    end.