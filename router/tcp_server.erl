-module(tcp_server).

-export([start_raw_server/2, run_accept/2]).


-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true} ]).


start_raw_server(Port, Fun) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    run_accept(LSocket, Fun).


run_accept(LSocket, Fun) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    spawn_link(fun() -> Fun(Socket) end),
	    run_accept(LSocket, Fun);
	_Other ->
	    io:format("Oops ~w~n", [_Other]),
	    exit(oops)
    end.








