%%% Copyright 2007 Aemon Cannon

-module(croq_snapshot_server).

-created_by('Aemon Cannon').

-export([start/1, do_accept_client_connections/2, handle_client_login/3, 
	 coordinate_snapshot_requests/1, snapshot_liason/2]).

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(LOGIN_ACK, ["LOGIN_ACK"]).
-define(LOGIN_NO_SNAPSHOTS_ACK, ["LOGIN_NO_SNAPSHOTS_ACK"]).
-define(LOGIN_YOU_ARE_FIRST_ACK, ["LOGIN_YOU_ARE_FIRST_ACK"]).
-define(SNAPSHOT_REQUEST, ["SNAPSHOT_REQUEST"]).

-record(client, {userId=none, socket}).

%% Listen on port 'Port'.
start(Port) ->
    {ok, ClientLSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    SnapshotServerPid = spawn(?MODULE, coordinate_snapshot_requests, [[]]),
    spawn(?MODULE, do_accept_client_connections, [ClientLSocket, SnapshotServerPid]),
    SnapshotServerPid.

%% Spawn a new client handler for each new connection
do_accept_client_connections(LSocket, SnapshotServerPid) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
	    SnapshotServerPid ! {connection, Socket},
	    ok;
        {error, Reason} ->
            io:format("Socket accept error in snapshot server: ~w.~n", [Reason])
    end,
    do_accept_client_connections(LSocket, SnapshotServerPid).


%% This client handler waits for a single data sentence, the login string.
%% If and when it is recieved, notify the snapshot server of the attempt.

handle_client_login(Buf, Socket, SnapshotServerPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    case croq_utils:parse_one_sentence(Buf ++ Data) of
		{ok, UserId, _} -> 
		    SnapshotServerPid ! {login_attempt, UserId, Socket},
		    ok;
		{none, RemainingBuffer} -> 
		    handle_client_login(RemainingBuffer, Socket, SnapshotServerPid)
	    end;
        {error, closed} -> ok
    end.

coordinate_snapshot_requests(Clients) ->
    receive
	{connection, Socket} ->
	    spawn(?MODULE, handle_client_login, [[], Socket, self()]),
	    coordinate_snapshot_requests(Clients);
        {login_attempt, UserId, Socket} ->
	    io:format("Remote host client attempted login to snapshot server ~s~n", [UserId]),
            Client = #client{socket=Socket, userId=UserId},
            io:format("Client connected to snapshot server: ~s~n", [Client#client.userId]),
	    case length(Clients) of
		N when N > 0 ->
		    gen_tcp:send(Socket, ?LOGIN_ACK ++ croq_utils:sentence_term()),
		    FromSocket = (lists:nth(1, Clients))#client.socket,
		    spawn(?MODULE, snapshot_liason, [FromSocket, Socket]),
		    coordinate_snapshot_requests([Client | Clients]);
		_ ->
		    gen_tcp:send(Socket, ?LOGIN_YOU_ARE_FIRST_ACK ++ croq_utils:sentence_term()),
		    coordinate_snapshot_requests([Client | Clients])
	    end;

	{disconnect, UserId} ->
	    case client_for_id(UserId, Clients) of
		{ok, Client} -> 
		    io:format("Client disconnected from snapshot server: ~s~n", [Client#client.userId]),
		    ok = gen_tcp:close(Client#client.socket),
		    coordinate_snapshot_requests(lists:delete(Client, Clients));
		false -> 
		    coordinate_snapshot_requests(Clients)
	    end
    end.

client_for_id(UserId, Clients) ->
    case lists:keysearch(UserId, #client.userId, Clients) of
	{value, Client} -> {ok, Client};
	false -> false
    end.

snapshot_liason(FromSocket, ToSocket) ->
    gen_tcp:send(FromSocket, ?SNAPSHOT_REQUEST ++ croq_utils:sentence_term()),
    ok = stream_copy_sentence(FromSocket, ToSocket),
    ok.

stream_copy_sentence(FromSocket, ToSocket) ->
    case gen_tcp:recv(FromSocket, 0) of
        {ok, Data} ->
	    case string:str(Data, croq_utils:sentence_term()) of
		N when N /= 0 ->
		    gen_tcp:send(ToSocket, string:substr(Data, 1, N - 1) ++ croq_utils:sentence_term()),
		    ok;
		_ ->
		    gen_tcp:send(ToSocket, Data),
		    stream_copy_sentence(FromSocket, ToSocket)
	    end;
	{error, Reason} -> 
	    io:format("Stream failed: ~w ~n", [Reason]),
	    Reason
    end.    



