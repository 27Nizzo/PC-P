-module(matchmaker).
-export([start/0, enter_queue/1, leave_queue/1, end_duel/1]).

start() ->
    ets:new(?MODULE, [set, named_table, public]),
    Pid = spawn(fun() -> loop([], #{}) end),
    register(?MODULE, Pid).

enter_queue(Username) ->
    call({enter, Username}).

leave_queue(Username) ->
    call({leave, Username}).

end_duel(Username) ->
    call({end_duel, Username}).

call(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    after 5000 -> {error, timeout}
    end.

loop(Queue, Duels) ->
    receive
        {Pid, {enter, Username}} ->
            case {login_manager:is_logged_in(Username), lists:member(Username, Queue)} of
                {true, false} ->
                    NewQueue = Queue ++ [Username],
                    ets:insert(?MODULE, {Username}),
                    case match_players(NewQueue) of
                        {matched, P1, P2, Rest} ->
                            start_duel(Pid, P1, P2, Rest, Duels);
                        no_match ->
                            reply(Pid, {ok, queued}),
                            loop(NewQueue, Duels)
                    end;
                {false, _} ->
                    reply(Pid, {error, not_logged_in}),
                    loop(Queue, Duels);
                {_, true} ->
                    reply(Pid, {error, already_in_queue}),
                    loop(Queue, Duels)
            end;

        {Pid, {leave, Username}} ->
            case lists:member(Username, Queue) of
                true ->
                    NewQueue = lists:delete(Username, Queue),
                    ets:delete(?MODULE, Username),
                    reply(Pid, ok),
                    loop(NewQueue, Duels);
                false ->
                    reply(Pid, {error, not_in_queue}),
                    loop(Queue, Duels)
            end;

        {Pid, {end_duel, Username}} ->
            case maps:find(Username, Duels) of
                {ok, DuelPid} ->
                    DuelPid ! {end_duel, Username},
                    NewDuels = maps:filter(fun(_, P) -> P =/= DuelPid end, Duels),
                    reply(Pid, {ok, ended}),
                    loop(Queue, NewDuels);
                error ->
                    reply(Pid, {error, not_in_duel}),
                    loop(Queue, Duels)
            end
    end.

match_players([P1, P2 | Rest]) when P1 =/= P2 -> {matched, P1, P2, Rest};
match_players(_) -> no_match.

start_duel(Pid, P1, P2, RestQueue, Duels) ->
    DuelPid = duel:start(P1, P2),
    NewDuels = Duels#{P1 => DuelPid, P2 => DuelPid},
    ets:delete(?MODULE, P1),
    ets:delete(?MODULE, P2),
    reply(Pid, {ok, waiting}),
    loop(RestQueue, NewDuels).

reply(Pid, Msg) ->
    Pid ! Msg.