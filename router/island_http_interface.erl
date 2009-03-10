-module(island_http_interface).

-export([start/3, stop/1, run_snapshot_liason/5]).

-import(http_driver, [header/1, classify/1]).
-import(lists, [map/2]).
-import(island_utils, [socket_pipe/3]).

-include("island_manager.hrl").

start(Port, Docroot, IslandMgrPid) -> 
    io:format("Starting HTTP Interface on: ~w.~n", [Port]),
    spawn_link(fun() -> server(Port, Docroot, IslandMgrPid) end).

stop(Port) -> tcp_server:stop(Port).


server(Port, Docroot, IslandMgrPid) ->
    process_flag(trap_exit, true),
    http_driver:start(
      Port, 
      fun(Client) -> 
	      client_handler(Client, Docroot, IslandMgrPid)
      end, 15),
    loop().

loop() ->
    receive
	Any ->
	    io:format("server:~p~n",[Any]),
	    loop()
    end.


%% Handle a single HTTP request
client_handler(DriverPid, Docroot, IslandMgrPid) ->
    receive
	{DriverPid, closed} ->
	    true;
	{DriverPid, {get, _Vsn, F, Args, _Env, Socket}} ->
	    io:format("Received GET for ~s, ~p.~n", [F, Args]),
	    case F of
		"/directory" -> 
		    {response, IslandList} = gen_server:call(IslandMgrPid, {directory}, 5000),
		    JsonList = map(fun island_data:island_to_json_obj/1, IslandList),
		    Encoded = list_to_binary(lists:flatten([mochijson2:encode(JsonList)])),
		    DriverPid ! { self(), { header(text), Encoded }},
		    DriverPid ! { self(), close };
		"/hup" -> 
		    {response, ok} = gen_server:call(IslandMgrPid, {hup}, 5000),
		    DriverPid ! { self(), { header(text), <<>> } },
		    DriverPid ! { self(), close };
		"/new_island" -> 
		    case lookup_args(["type", "desc"], Args) of
			[{"type", IslandType}, {"desc", IslandDesc}] ->
			    {response, Isl} = gen_server:call(IslandMgrPid, {create_new_island, IslandType, IslandDesc}, 5000),
			    Data = list_to_binary(Isl#island.id),
			    DriverPid ! { self(), { header(text), Data } },
			    DriverPid ! { self(), close };
			_Else -> 
			    DriverPid ! { self(), { header(error), <<>>} },
			    DriverPid ! { self(), close }
		    end;
		"/join_island" -> 
		    case lookup_args(["id", "clientId"], Args) of
			[{"id", IslandId}, {"clientId", ClientId}] ->
			    case gen_server:call(IslandMgrPid, { join_island, ClientId, IslandId, Socket }, 1000) of
				{response, #island{}} ->
				    %% Send header, but don't close the socket.
				    %% Msg routing is now happening on this socket.
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
		_Filename -> 
		    DriverPid ! { self(), file_response(F) },
		    DriverPid ! { self(), close }
	    end,
	    client_handler(DriverPid, Docroot, IslandMgrPid);

	{DriverPid, {post, _Vsn, F, Args, _Env, Socket, DataSoFar, ContentLen}} ->
	    io:format("Received POST for '~s'~n", [F]),
	    case F of
		"/send_snapshot" -> 
		    case lookup_args(["id", "clientId"], Args) of
			[{"id", IslandId}, {"clientId", ClientId}] ->
			    DriverPid ! { self(), { header(text), <<>> }},
			    case gen_server:call(IslandMgrPid, { get_snapshot_liason, ClientId, IslandId }) of
				{response, {liason, LiasonPid, IslandId}} -> 
				    LiasonPid ! {partner, list_to_binary(DataSoFar), ContentLen, Socket};
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
	    client_handler(DriverPid, Docroot, IslandMgrPid)
    end.


file_response(F) ->
    F1 = "." ++ F,
    case file:read_file(F1) of
        {ok, Bin} ->
            case classify(F) of
                html ->
                    {header(html), Bin};
                jpg ->
                    {header(jpg), Bin};
                gif ->
                    {header(jpg), Bin};
                _ ->
                    {header(text), Bin }
            end;
        _ ->
	    {header(not_found), <<>>}
    end.

create_snapshot_liason(ClientId, IslandId, DriverPid, Socket) ->
    Pid = spawn_link(?MODULE, run_snapshot_liason, [ClientId, IslandId, self(), DriverPid, Socket]),
    Pid.

run_snapshot_liason(ClientId, IslandId, ServerPid, DriverPid, Socket) ->
    receive
	snapshot_not_available -> 
	    DriverPid ! { ServerPid, { header(not_found), <<>>}},
	    DriverPid ! { ServerPid, close },
	    ok;
	{partner, DataSoFar, TotalContentLen, PartnerSocket } -> 
	    inet:setopts(Socket, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
	    inet:setopts(PartnerSocket, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]),
    	    gen_tcp:send(Socket, [header(text), "Content-Length: " ++ integer_to_list(TotalContentLen) ++ "\r\n\r\n"]),
	    gen_tcp:send(Socket, DataSoFar),
	    ok = socket_pipe(PartnerSocket, Socket, TotalContentLen - size(DataSoFar)),
	    gen_tcp:close(Socket),
	    ok
    end,
    run_snapshot_liason(ClientId, IslandId, ServerPid, DriverPid, Socket).



show(X) ->
    {header(text),[lists:flatten(io_lib:format("~p~n",[X]))]}.

lookup_args(Keys, Args) -> lists:map(fun(Key) ->
					     case lists:keysearch(Key, 1, Args) of
						 {value, {Key, Val}} -> {Key, Val};
						 _Else  -> false
					     end
				     end, 
				     Keys).





