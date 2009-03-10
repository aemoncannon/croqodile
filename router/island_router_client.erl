-module(island_router_client).

-created_by('Aemon Cannon').

-export([start/4, init_client/4, input_driver/3]).

-import(island_utils, [parse_all_messages/1, encode_message/1]).

-include("island_manager.hrl").



start(IslandMgrPid, Island, Client, Socket) ->
    ClientPid = spawn_link(?MODULE, init_client, [Client, IslandMgrPid, Island, Socket]),
    ClientPid.


%% First, fork off a low-level protocol driver. Then start listening for messages from
%% that driver, and from the router.
init_client(Client, IslandMgrPid, Island, Socket) ->
    inet:setopts(Socket, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
    DriverPid = spawn_link(?MODULE, input_driver, [Socket, self(), []]),
    run_client(Client, IslandMgrPid, Island, DriverPid, Socket).


run_client(Client, IslandMgrPid, Island=#island{router_pid=RouterPid}, DriverPid, Socket) ->
    receive
	{router_message, Msg} ->
	    io:format("Sending msg ~p~n", [Msg]),
	    gen_tcp:send(Socket, encode_message(Msg)),
	    run_client(Client, IslandMgrPid, Island, DriverPid, Socket);
	{driver_message, Msg} ->
	    RouterPid ! {message, self(), Msg},
	    run_client(Client, IslandMgrPid, Island, DriverPid, Socket);
	{driver_closed} ->
	    RouterPid ! {client_closed, self()}
    end.


input_driver(Socket, ClientPid, Buf) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    Combined = list_to_binary([Buf, Data]),
	    {Messages, RemainingBuffer} = parse_all_messages(Combined),
	    lists:foreach(fun(M) -> ClientPid ! {driver_message, M } end, Messages),
	    input_driver(Socket, ClientPid, RemainingBuffer);
        {error, closed} ->
            ClientPid ! {driver_closed}
    end.


