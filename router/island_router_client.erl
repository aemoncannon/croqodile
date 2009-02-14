-module(island_router_client).

-created_by('Aemon Cannon').

-export([start/4, init_client/4, protocol_driver/3]).

-include("island_manager.hrl").


start(IslandMgrPid, Island, Client, Socket) ->
    ClientPid = spawn_link(?MODULE, init_client, [Client, IslandMgrPid, Island, Socket]),
    ClientPid.


%% First, fork off a low-level protocol driver. Then start listening for messages from
%% that driver, and from the router.
init_client(Client, IslandMgrPid, Island, Socket) ->
    inet:setopts(Socket, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
    DriverPid = spawn_link(?MODULE, protocol_driver, [self(), Socket, []]),
    run_client(Client, IslandMgrPid, Island, DriverPid).


run_client(Client, IslandMgrPid, Island=#island{router_pid=RouterPid}, DriverPid) ->
    receive
	{router_message, Data} ->
	    DriverPid ! {message, Data},
	    run_client(Client, IslandMgrPid, Island, DriverPid);
	{driver_message, Data} ->
	    RouterPid ! {message, Data},
	    run_client(Client, IslandMgrPid, Island, DriverPid);
	{driver_closed} ->
	    RouterPid ! {client_closed, self()};
	Else ->
	    io:format("Unknown message: ~w.~n", [Else])
    end.


protocol_driver(Socket, ClientPid, Buf) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    {Messages, RemainingBuffer} = parse_all_sentences(Buf ++ Data, []),
	    lists:foreach(fun(M) -> ClientPid ! {driver_message, M } end, Messages),
	    protocol_driver(Socket, ClientPid, RemainingBuffer);
        {error, closed} ->
            ClientPid ! {driver_closed}
    end.


-define(TERMINATOR, [12,12]).

sentence_term() -> ?TERMINATOR.

%% Parse all sentences in the string Buf, return the sentences and the remainder of the string.
parse_all_sentences(Buf, Sentences) ->
    case string:str(Buf, sentence_term()) of
	N when N /= 0 -> parse_all_sentences(string:substr(Buf, N + length(sentence_term())), 
					     Sentences ++ [string:substr(Buf, 1, N - 1)]);
	
	_ -> 		{Sentences, Buf}
    end.
