-module(island_http_interface).

-export([start/2, stop/1]).

-import(http_driver, [classify/1, header/1]).
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
		    case lookup_arg("id", Args) of
			{value, IslandId} -> 
			    {response, _Isl} = gen_server:call(IslandMgrPid, { join_island, IslandId, Socket }, 1000);
			_Else -> 
			    DriverPid ! { self(), { header(text), <<>> } },
			    DriverPid ! { self(), close }
		    end;
		_ -> 
		    DriverPid ! show({do_not_understand, F, args, Args, cwd, file:get_cwd()})
	    end,
	    client_handler(DriverPid, State)
    after 5000 ->
	    true
    end.




show(X) ->
    {header(text),[lists:flatten(io_lib:format("~p~n",[X]))]}.

lookup_arg(Key, Args) -> lists:keysearch(Key, 0, Args).



