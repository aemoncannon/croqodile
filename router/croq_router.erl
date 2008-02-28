%%% Copyright 2007 Aemon Cannon

-module(croq_router).

-created_by('Aemon Cannon').

-export([start/3, handle_client_login/3, handle_client_messages/3, do_accept_client_connections/2, run_router/3]).

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]).
-define(LOGIN_ACK, ["LOGIN_ACK"]).
-define(MESSAGE_SEP, "@").
-define(HEART_RATE, 20).

-record(client, {userId=none, socket}).

%% Start the router and its supporting cast.

start(Port, PolicyPort, SnapshotPort) ->

    croq_policy_server:start(PolicyPort),
    io:format("Flash security policy server started on port ~w.~n", [PolicyPort]),

    SnapshotServerPid = croq_snapshot_server:start(SnapshotPort),
    io:format("Snapshot server started on port ~w.~n", [SnapshotPort]),
    
    {ok, ClientLSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    RouterPid = spawn(?MODULE, run_router, [[], next_time(0), SnapshotServerPid]),
    io:format("Message router started on port ~w.~n", [Port]),

    start_heartbeat(RouterPid),
    io:format("Heartbeat started.~n", []),

    do_accept_client_connections(ClientLSocket, RouterPid).



%% Spawn a new client handler for each new connection

do_accept_client_connections(LSocket, RouterPid) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
	    RouterPid ! {connection, Socket},
	    ok;
        {error, Reason} ->
            io:format("Socket accept error: ~s~n", [Reason])
    end,
    do_accept_client_connections(LSocket, RouterPid).


%% This client handler waits for a single data sentence, the login string.
%% If and when it is recieved, we send an the login attempt to the router and die.

handle_client_login(Buf, Socket, RouterPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    case croq_utils:parse_one_sentence(Buf ++ Data) of
		{ok, UserId, _} -> 
		    RouterPid ! {login_attempt, UserId, Socket},
		    ok;
		{none, RemainingBuffer} -> 
		    handle_client_login(RemainingBuffer, Socket, RouterPid)
	    end;
	{error, closed} -> ok
    end.



%% This is the main loop of the router process.  It maintains
%% the list of "clients" and performs the primary actions.

run_router(Clients, LastTime, SnapshotServerPid) ->
    Time = next_time(LastTime),
    receive
	{connection, Socket} ->
	    io:format("Client connected..~n", []),
	    spawn_link(?MODULE, handle_client_login, [[], Socket, self()]),
	    run_router(Clients, Time, SnapshotServerPid);
        {login_attempt, UserId, Socket} ->
	    io:format("Client attempted login as ~s~n", [UserId]),
            spawn_link(?MODULE, handle_client_messages, [[], Socket, self()]),
	    gen_tcp:send(Socket, ?LOGIN_ACK ++ croq_utils:sentence_term()),
	    Client = #client{socket=Socket, userId=UserId},
	    io:format("New client accepted: ~s~n", [Client#client.userId]),
	    run_router([Client | Clients], Time, SnapshotServerPid);
        {disconnect, Socket} ->
	    case client_for_socket(Socket, Clients) of
		{ok, Client} -> 
		    io:format("Client disconnected: ~s~n", [Client#client.userId]),
		    SnapshotServerPid ! {disconnect, Client#client.userId},
		    ok = gen_tcp:close(Socket),
		    run_router(lists:delete(Client, Clients), Time, SnapshotServerPid);
		false -> 
		    run_router(Clients, Time, SnapshotServerPid)
	    end;
        {message, _ , Data} ->
	    Message = float_to_list(Time) ++ ?MESSAGE_SEP ++ Data ++ croq_utils:sentence_term(),
	    send_to_active(Clients, Message),
	    run_router(Clients, Time, SnapshotServerPid);
        heartbeat ->
	    Message = float_to_list(Time) ++ ?MESSAGE_SEP ++ "[\"heartbeat\"]" ++ croq_utils:sentence_term(),
	    send_to_active(Clients, Message),
	    run_router(Clients, Time, SnapshotServerPid);
	Else ->
	    io:format("Unknown message: ~w.~n", [Else])
    end.


%% This client handler waits for data, assembles it into meaningful 
%% sentences, then forwards it to the router process.
%% If the client disconnects, we let the router know and then die.

handle_client_messages(Buf, Socket, RouterPid) ->
    %%    timer:sleep(random:uniform(100)),  %%simulate some lag
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    {Messages, RemainingBuffer} = croq_utils:parse_all_sentences(Buf ++ Data, []),
	    lists:foreach(fun(M) -> RouterPid ! {message, Socket, M } end, Messages),
	    handle_client_messages(RemainingBuffer, Socket, RouterPid);
        {error, closed} ->
            RouterPid ! {disconnect, Socket}
    end.



client_for_socket(Socket, Clients) ->
    case lists:keysearch(Socket, #client.socket, Clients) of
	{value, Client} -> {ok, Client};
	false -> false
    end.

%% Sends the given data to all clients that are 'active'
send_to_active(Clients, Data) ->
    lists:foreach(fun(C) -> gen_tcp:send(C#client.socket, Data) end, Clients),
    ok.

%% Heartbeat generator
start_heartbeat(RouterPid) ->
    timer:send_interval(?HEART_RATE, RouterPid, heartbeat),
    ok.


%% Return next timestamp in milliseconds.
%% Router must guarantee that timestamps are always increasing, never repeating.
next_time(LastTime) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    Time = (MegaSecs * 1000000000) + (Secs * 1000) + (MicroSecs/1000), 
    case Time of
	N when N =< LastTime -> LastTime + 1;
	_  -> Time
    end.

