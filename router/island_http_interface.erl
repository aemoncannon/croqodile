-module(island_http_interface).

-export([start/2, stop/1, run_snapshot_liason/4]).

-import(http_driver, [header/1]).
-import(lists, [map/2]).

-include("island_manager.hrl").

-record(state, {master_pid=undefined, island_mgr_pid=undefined}).

start(Port, IslandMgrPid) -> spawn_link(fun() -> server(Port, IslandMgrPid) end).

stop(Port) -> tcp_server:stop(Port).


server(Port, IslandMgrPid) ->
    S = self(),
    process_flag(trap_exit, true),
    http_driver:start(
      Port, 
      fun(Client) -> 
	      client_handler(Client, #state{
			       island_mgr_pid=IslandMgrPid,
			       master_pid=S
			      })
      end, 15),
    loop().

loop() ->
    receive
	Any ->
	    io:format("server:~p~n",[Any]),
	    loop()
    end.


%% Handle a single HTTP request
client_handler(DriverPid, State=#state{island_mgr_pid=IslandMgrPid}) ->
    receive
	{DriverPid, closed} ->
	    true;
	{DriverPid, {_, _Vsn, F, Args, _Env, Socket}} ->
	    io:format("Received request for '~s'~n", [F]),
	    case F of
		"/directory" -> 
		    {response, IslandList} = gen_server:call(IslandMgrPid, {directory}, 5000),
		    JsonList = map(fun island_data:island_to_json_obj/1, IslandList),
		    Encoded = list_to_binary(lists:flatten([mochijson2:encode(JsonList)])),
		    DriverPid ! { self(), { header(text), Encoded } },
		    DriverPid ! { self(), close };
		"/hup" -> 
		    {response, ok} = gen_server:call(IslandMgrPid, {hup}, 5000),
		    DriverPid ! { self(), { header(text), <<>> } },
		    DriverPid ! { self(), close };
		"/join_island" -> 
		    case lookup_args(["id", "clientId"], Args) of
			[{"id", IslandId}, {"clientId", ClientId}] ->
			    case gen_server:call(IslandMgrPid, { join_island, ClientId, IslandId, Socket }, 1000) of
				{response, #island{}} ->
				    %% Send response, but don't close the socket..
				    DriverPid ! { self(), { header(text), <<>> }};
				{response, no_such_island} ->
				    DriverPid ! { self(), { header(not_found), <<>> } },
				    DriverPid ! { self(), close }
			    end;
			_Else -> 
			    DriverPid ! { self(), { header(error), <<>>} },
			    DriverPid ! { self(), close }
		    end;
		"/get_snapshot" -> 
		    case lookup_args(["id", "clientId"], Args) of
			[{"id", IslandId}, {"clientId", ClientId}] ->
			    %% Liason will be notified if snapshot is not available or will hang around and 
			    %% facilitate data exchange once a partner starts uploading a snapshot..
			    LiasonPid = create_snapshot_liason(ClientId, IslandId, DriverPid, Socket),
			    gen_server:cast(IslandMgrPid, { add_snapshot_liason, ClientId, IslandId, LiasonPid });
			_Else -> 
			    DriverPid ! { self(), { header(error), <<>>} },
			    DriverPid ! { self(), close }
		    end;
		"/send_snapshot" -> 
		    case lookup_args(["id", "clientId"], Args) of
			[{"id", IslandId}, {"clientId", ClientId}] ->
			    DriverPid ! { self(), { header(text), <<>> }},
			    case gen_server:call(IslandMgrPid, { get_snapshot_liason, ClientId, IslandId }) of
				{response, {liason, LiasonPid, IslandId}} -> 
				    LiasonPid ! {partner, Socket};
				_Else -> 
				    DriverPid ! { self(), close }
			    end;
			_Else -> 
			    DriverPid ! { self(), { header(error), <<>>} },
			    DriverPid ! { self(), close }
		    end;
		_ -> 
		    DriverPid ! show({do_not_understand, F, args, Args, cwd, file:get_cwd()})
	    end,
	    client_handler(DriverPid, State)
    after 5000 ->
	    true
    end.


create_snapshot_liason(ClientId, IslandId, DriverPid, Socket) ->
    inet:setopts(Socket, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
    Pid = spawn_link(?MODULE, run_snapshot_liason, [ClientId, IslandId, DriverPid, Socket]),
    Pid.

run_snapshot_liason(ClientId, IslandId, DriverPid, Socket) ->
    receive
	snapshot_not_available -> 
	    DriverPid ! { self(), { header(not_found), <<>>}},
	    DriverPid ! { self(), close },
	    ok;
	{partner, PartnerSocket } -> 
	    DriverPid ! { self(), { header(text), <<>> }},
	    croq_utils:socket_pipe(PartnerSocket, Socket),
	    ok
    end,
    run_snapshot_liason(ClientId, IslandId, DriverPid, Socket).



show(X) ->
    {header(text),[lists:flatten(io_lib:format("~p~n",[X]))]}.

lookup_args(Keys, Args) -> lists:map(fun(Key) ->
					     case lists:keysearch(Key, 1, Args) of
						 {value, {Key, Val}} -> {Key, Val};
						 _Else  -> false
					     end
				     end, 
				     Keys).





