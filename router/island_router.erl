-module(island_router).

-created_by('Aemon Cannon').

-export([start/2, run_router/4]).

-include("island_manager.hrl").

-define(HEART_RATE, 20).


%% Start the router and its supporting cast.
start(IslandMgrPid, Island) ->
    RouterPid = spawn_link(?MODULE, run_router, [[], next_time(0), IslandMgrPid, Island]),
    start_heartbeat(RouterPid),
    RouterPid.


%% This is the main loop of the router process.  It maintains
%% the list of clients and performs the primary actions.
run_router(Clients, LastTime, MgrPid, Island) ->
    Time = next_time(LastTime),
    receive
	{join, C } ->
	    io:format("Router: New client joined: ~s~n", [C#client.id]),
	    run_router([C | Clients], Time, MgrPid, Island);
        {remove_client, ClientId} ->
	    case client_by_id(ClientId, Clients) of
		{value, C} -> 
		    io:format("Router: Client removed: ~s~n", [C#client.id]),
		    run_router(lists:delete(C, Clients), Time, MgrPid, Island);
		false -> 
		    run_router(Clients, Time, MgrPid, Island)
	    end;
        {message, _FromPid, Message} ->
	    send_to_active(Clients, Message),
	    run_router(Clients, Time, MgrPid, Island);
        {client_closed, ClientPid} ->
	    case client_by_pid(ClientPid, Clients) of
		{value, C} -> 
		    self() ! {remove_client,  C#client.id},
		    run_router(Clients, Time, MgrPid, Island);
		false -> 
		    run_router(Clients, Time, MgrPid, Island)
	    end;
	heartbeat ->
	    Message = float_to_list(Time),
	    send_to_active(Clients, Message),
	    run_router(Clients, Time, MgrPid, Island);
	Else ->
	    io:format("Unknown message: ~w.~n", [Else])
    end.


%% Sends the given data to all clients that are 'active'
send_to_active(Clients, Message) ->
    lists:foreach(fun(C) -> (C#client.pid ! {message, Message}) end, Clients),
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

client_by_id(Id, Clients) -> lists:keysearch(Id, #client.id, Clients).

client_by_pid(Pid, Clients) -> lists:keysearch(Pid, #client.pid, Clients).




