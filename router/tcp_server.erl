-module(tcp_server).

-export([start_raw_server/2, run_accept/2]).

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}, {recbuf, 5000} ]).

start_raw_server(Port, Fun) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
	{ok, LSocket} -> run_accept(LSocket, Fun);
	{error, eaddrinuse} -> 
	    io:format("Oops! Port ~w is already bound.~n", [Port]),
	    exit(error);
	{error, Other} -> 
	    io:format("Oops! Error starting flash policy server: ~w.~n", [Other]),
	    exit(error)
    end.



run_accept(LSocket, Fun) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    spawn_link(fun() -> Fun(Socket) end),
	    run_accept(LSocket, Fun);
	_Other ->
	    io:format("Oops ~w~n", [_Other]),
	    exit(error)
    end.








