-module(setup).
-export([start/0]).

start() ->
    c:l(readFile),
    c:l(account_server),
    account_server:start(), % Já chama create_admin() internamente
    c:l(matchmaker),
    matchmaker:start(),
    c:l(duel),
    c:l(client_session).
    %c:l(server),
    %server:start(),
    %io:format("Servidor iniciado. Use 'admin' e 'admin123' para controlar o servidor.~n").