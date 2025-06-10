-module(cliente).
-define(PUERTO, 12345).
%
% Este módulo implementa un cliente que se conecta a un servidor TCP y permite enviar mensajes.
% El cliente solicita al usuario que ingrese un mensaje, lo envía al servidor y espera una respuesta.
% El cliente puede enviar múltiples mensajes hasta que se cierre la conexión.
%
% -include_lib("kernel/include/io.hrl").
% -include_lib("stdlib/include/erl_parse.hrl").
% -include_lib("kernel/include/gen_tcp.hrl").
% -include_lib("kernel/include/inet.hrl"). 
-export([cliente/0, iniciar/0, enviar_mensaje/1]).
cliente() ->
    iniciar().
iniciar() ->
    case gen_tcp:connect("localhost", ?PUERTO, [binary, {active, false}]) of
        {ok, Socket} -> 
            io:format("Conexión establecida con el servidor.~n"),
            enviar_mensaje(Socket);
        {error, Razon} -> 
            io:format("Error al conectar con el servidor. Tipo de error: ~p ~n", Razon)
    end.
enviar_mensaje(Socket) ->
    io:format("Ingrese un mensaje para enviar al servidor: "),
    Mensaje = io:get_line(""),
    case gen_tcp:send(Socket, Mensaje) of
        ok -> 
            io:format("Mensaje enviado: ~s~n", Mensaje),
            recibir_respuesta(Socket);
        {error, Razon} -> 
            io:format("Error al enviar el mensaje. Tipo de error: ~p ~n", Razon)
    end.
recibir_respuesta(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Respuesta} -> 
            io:format("Respuesta del servidor: ~s~n", Respuesta),
            enviar_mensaje(Socket); % Permite enviar más mensajes
        {error, closed} -> 
            io:format("El servidor ha cerrado la conexión.~n"),
            ok;
        {error, Razon} -> 
            io:format("Error al recibir la respuesta. Tipo de error: ~p ~n", Razon)
    end.
% Para ejecutar el cliente, se puede iniciar en la consola de Erlang con:
% 1> c(cliente).
% 2> cliente:cliente().
% Esto iniciará el cliente, que se conectará al servidor y permitirá enviar mensajes.     