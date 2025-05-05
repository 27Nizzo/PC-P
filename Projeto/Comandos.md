# Comando para realizar o teste
1) Primeiro Terminal:
Escrevem erl na diretoria correta e aquilo vai abrir o erl shell e de seguida metem os comandos abaixo pela ordem que estão um de cada vez:

- c(login_manager), login_manager:start().
- c(matchmaker), matchmaker:start().
- c(duel).
- c(client_session).
- c(server), server:start().

- c(setup), setup:start().

2) Segundo terminal metem:
- telnet 192.168.77.216(este valor varia, metes o comando ifconfig e ves valor no inet) 1234
- telnet 192.168.1.13 1234

(caso nao tenham façam sudo apt ou qualquer coisa :D)

DESATUALIZADO!!!