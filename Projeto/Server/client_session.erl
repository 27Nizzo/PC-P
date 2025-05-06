-module(client_session).
-export([start/1]).

start(Socket) ->
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
            Line = binary_to_list(Bin),
            Response = handle_command(Line),
            gen_tcp:send(Socket, Response ++ "\n"),
            loop(Socket);
        {error, closed} ->
            ok
    end.

handle_command(Line) ->
    case string:tokens(Line, " ") of
        ["LOGIN", User, Pass] ->
            case account_server:auth({User, Pass}) of
                {ok, created} -> "OK";
                _ -> "ERROR"
            end;
        _ -> "ERROR"
    end.