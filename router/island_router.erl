-module(island_router).

-created_by('Aemon Cannon').

-export([start/2]).

-include("island_manager.hrl").

-define(HEART_RATE, 20).

%%
%% TODO: Use these. especially the no-delay!
%%
%%-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]).


%% Start the router and its supporting cast.
start(IslandMgrPid, Island) ->
    RouterPid = spawn_link(?MODULE, run_router, [[], next_time(0)]),
    start_heartbeat(RouterPid),
    RouterPid.


%% This is the main loop of the router process.  It maintains
%% the list of "clients" and performs the primary actions.

run_router(Clients, LastTime) ->
    Time = next_time(LastTime),
    receive
	{join, Client } ->
	    io:format("Router: New client joined: ~s~n", [Client#client.id]),
	    run_router([Client | Clients], Time);
        {remove_client, ClientId} ->
	    case client_by_id(ClientId, Clients) of
		{value, Client} -> 
		    io:format("Router: Client removed: ~s~n", [Client#client.id]),
		    run_router(lists:delete(Client, Clients), Time);
		false -> 
		    run_router(Clients, Time)
	    end;
        {message, _FromPid, Data} ->
	    Message = "hullo",%%float_to_list(Time) ++ ?MESSAGE_SEP ++ Data ++ croq_utils:sentence_term(),
	    send_to_active(Clients, Message),
	    run_router(Clients, Time);
        heartbeat ->
	    Message = float_to_list(Time),
	    send_to_active(Clients, Message),
	    run_router(Clients, Time);
	Else ->
	    io:format("Unknown message: ~w.~n", [Else])
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

client_by_id(Id, Clients) -> lists:keysearch(Id, #client.id, Clients).

client_by_pid(Pid, Clients) -> lists:keysearch(Pid, #client.pid, Clients).


% Should look roughly like this. BTW, we need to steal the socket, and have 
% messages sent here isntead. OR switch to passive mode.
%
% relay(Socket, Server, State) ->
%    receive
%	{tcp, Socket, Bin} ->
%	    Data = binary_to_list(Bin),
%	    %% io:format("<-- ~s~n", [Data]),
%	    parse_request(State, Socket, Server, Data);
%	{tcp_closed, Socket} ->
%	    Server ! {self(), closed};
%	{Server, close} ->
%	    gen_tcp:close(Socket);
%	{Server, {Headers, BinaryData}} ->
%	    Len = size(BinaryData),
%	    Headers1 = Headers ++ "Content-Length: " ++ 
%		integer_to_list(Len) ++ "\r\n\r\n",
%	    %% io:format("--> ~p ~p~n", [Headers1, BinaryData]),
%    	    gen_tcp:send(Socket, [Headers1, BinaryData]),
%	    relay(Socket, Server, State);
%	{'EXIT', Server, _} ->
%	    gen_tcp:close(Socket)
%    end.
protocol_driver(Client, Island, Socket) ->
    receive

    after 5000 ->
	    true
    end.



