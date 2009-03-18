-module(island_snapshot_liaison).

-export([start/3, run_snapshot_liaison/4]).

-import(http_driver, [send_response/3, begin_response/3]).
-import(island_utils, [socket_pipe/3]).

-include("island_manager.hrl").

start(ClientId, IslandId, Socket) ->
    Pid = spawn(?MODULE, run_snapshot_liaison, [ClientId, IslandId, self(), Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    Pid.

run_snapshot_liaison(ClientId, IslandId, ServerPid, Socket) ->
    receive
	snapshot_not_available -> 
	    send_response(Socket, not_found, <<>>),
	    ok;
	{partner, DataSoFar, TotalContentLen, PartnerSocket } -> 
	    gen_tcp:controlling_process(PartnerSocket, self()),
	    begin_response(Socket, text, TotalContentLen),
	    ok = gen_tcp:send(Socket, DataSoFar),
	    io:format("Piped ~w bytes ~s.~n", [size(DataSoFar), DataSoFar]),
	    ok = socket_pipe(PartnerSocket, Socket, TotalContentLen - size(DataSoFar)),
	    ok = gen_tcp:close(Socket),
	    io:format("Finished piping ~w bytes between peers.~n", [TotalContentLen]),
	    ok
    end,
    run_snapshot_liaison(ClientId, IslandId, ServerPid, Socket).

