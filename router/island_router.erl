-module(island_router).

-created_by('Aemon Cannon').

-export([start/2, run_router/5]).

-import(island_utils, [stamp_message/3, make_heartbeat_message/2, make_snapshot_req_message/2]).

-include("island_manager.hrl").

-define(HEART_RATE, 20).


%% Start the router and its supporting cast.
start(IslandMgrPid, Island) ->
    RouterPid = spawn_link(?MODULE, run_router, [[], 0, next_time(0), IslandMgrPid, Island]),
    start_heartbeat(RouterPid),
    RouterPid.


%% This is the main loop of the router process.  It maintains
%% the list of clients and performs the primary actions.
run_router(Clients, MsgNum, Time, MgrPid, Island) ->
    receive
	{join, C } ->
	    io:format("Router: New client joined: ~s~n", [C#client.id]),
	    run_router([C | Clients], MsgNum, Time, MgrPid, Island);
        {remove_client, ClientId} ->
	    case client_by_id(ClientId, Clients) of
		{value, C} -> 
		    io:format("Router: Client removed: ~s~n", [C#client.id]),
		    run_router(lists:delete(C, Clients), MsgNum, Time, MgrPid, Island);
		false -> 
		    run_router(Clients, MsgNum, Time, MgrPid, Island)
	    end;
        {message, _FromPid, Message} ->
	    {NextMsgNum, NextTime} = {MsgNum + 1, next_time(Time)},
	    io:format("Message from client: ~p~n", [Message]),
	    StampedMsg = stamp_message(Message, NextMsgNum, NextTime),
	    send_to_active(Clients, StampedMsg),
	    run_router(Clients, NextMsgNum, NextTime, MgrPid, Island);
        {snapshot_request, ClientId, LiasonPid} ->
	    {NextMsgNum, NextTime} = {MsgNum + 1, next_time(Time)},
	    io:format("Snapshot request from client.~n", []),
	    case find_snapshot_partner(ClientId, Clients) of
		{value, Partner} ->
		    io:format("Router sending Snapshot request to partner...~n", []),
		    Partner#client.pid ! {router_message, make_snapshot_req_message(NextMsgNum, NextTime)};
		_Else -> LiasonPid ! snapshot_not_available
	    end,
	    run_router(Clients, NextMsgNum, NextTime, MgrPid, Island);
        {client_closed, ClientPid} ->
	    case client_by_pid(ClientPid, Clients) of
		{value, C} -> 
		    self() ! {remove_client,  C#client.id},
		    run_router(Clients, MsgNum, Time, MgrPid, Island);
		false ->
		    run_router(Clients, MsgNum, Time, MgrPid, Island)
	    end;
	heartbeat ->
	    {NextMsgNum, NextTime} = {MsgNum + 1, next_time(Time)},
	    Message = make_heartbeat_message(NextMsgNum, NextTime),
	    send_to_active(Clients, Message),
	    run_router(Clients, NextMsgNum, NextTime, MgrPid, Island)
    end.


%% Sends the given data to all clients that are 'active'
send_to_active(Clients, Message) ->
    lists:foreach(fun(C) -> (C#client.pid ! {router_message, Message}) end, Clients),
    ok.

%% Heartbeat generator
start_heartbeat(RouterPid) ->
    timer:send_interval(?HEART_RATE, RouterPid, heartbeat),
    ok.


%% Return next timestamp in milliseconds.
%% Router must guarantee that timestamps are always increasing, never repeating.
next_time(LastTime) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    Time = trunc((MegaSecs * 1000000000) + (Secs * 1000) + (MicroSecs/1000)), 
    case Time of
	N when N =< LastTime -> LastTime + 1;
	_  -> Time
    end.

client_by_id(Id, Clients) -> lists:keysearch(Id, #client.id, Clients).

client_by_pid(Pid, Clients) -> lists:keysearch(Pid, #client.pid, Clients).

%% Return the first Client record whose id does NOT match ClientId.
find_snapshot_partner(_, []) -> false;
find_snapshot_partner(ClientId, [C | Rest]) -> case C of
						   #client{id=ClientId} -> find_snapshot_partner(ClientId, Rest);
						   _Else -> {value, C}
					       end.







