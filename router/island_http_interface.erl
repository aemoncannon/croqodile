-module(island_http_interface).

-export([start/2, stop/1]).

-import(http_driver, [classify/1, header/1]).
-import(lists, [map/2]).
-import(mochijson2, [encode/1]).


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

client_handler(Client, State=#state{island_mgr_pid=IslandMgrPid}) ->
    receive
	{Client, closed} ->
	    true;
	{Client, {_, _Vsn, F, Args, _Env}} ->
	    case F of
		"/directory" -> 
		    {response, IslandList} = gen_server:call(IslandMgrPid, {directory}, 5000),
		    Client ! {self(), {header(text), encode(IslandList)}},
		    Client ! {self(), close};
		_ -> 
		    Client ! show({do_not_understand, F, args, Args, cwd, file:get_cwd()})
	    end,
	    client_handler(Client, State)
    after 5000 ->
	    true
    end.


show(X) ->
    {header(text),[lists:flatten(io_lib:format("~p~n",[X]))]}.

