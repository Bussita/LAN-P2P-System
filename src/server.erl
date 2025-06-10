-module(server).

-define(PUERTO, 12345).

-export([server/0,init/0]).

start() ->
    case gen_tcp:listen(?PUERTO,[binary,{active,false}]) of
        {ok, Socket} -> spawn(?MODULE, receptor, [Socket]);
        {error, Razon} -> io:format("Error al crear el Socket de escucha. Tipo de error: ~p ~n",Razon)
    end.

receptor(Socket) ->
    case gen_tcp:accept(Socket) of
        {ok, CSocket} -> spawn(?MODULE, echo, [CSocket]);
        {error, Razon} -> io:format("Error al crear el Socket de conexión. Tipo de error: ~p ~n",Razon)
    end,
    receptor(Socket).

scanSharedDirectory() ->
    case file:list_dir("/Shared") of
        {ok, Files, CId} ->
            io:format("Archivos en el directorio ~s:~n", [Path]),
            lists:foreach(fun(File) -> io:format("~s~n", [File]) end, Files),
            CId ! {scan_complete, Files},

        {error, Reason} ->
            io:format("Error al listar el directorio ~s. Tipo de error: ~p ~n", [Path, Reason]),
            {error, Reason}
    end.