%%% Copyright 2007 Aemon Cannon

-module(flash_policy_server).

-created_by('Aemon Cannon').

-export([start/2, policy_request_handler/2]).

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).


start(Port, AllowPorts) ->
    io:format("Starting policy server on ~w.~n", [Port]),
    %% Handle requests from Flash's security-manager
    {ok, PolicyLSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    Pid = spawn(?MODULE, policy_request_handler, [PolicyLSocket, AllowPorts]),
    Pid.

%% When flash requests a security policy, shoot it back Carte blanche
policy_request_handler(LSocket, AllowPorts) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
	    io:format("Receiving policy request...~n", []),
	    ok = read_req(Socket, []),
	    Policy = policy_string(AllowPorts),
	    io:format("Got request. Sending ~p.~n", [Policy]),
	    ok = gen_tcp:send(Socket, Policy ++ "\0"),
	    ok = gen_tcp:shutdown(Socket, write);
        {error, Reason} ->
            io:format("PolicySocket accept error: ~p~n", [Reason])
    end,
    policy_request_handler(LSocket, AllowPorts).


policy_string(AllowPorts) ->
    Strs = lists:map(fun integer_to_list/1, AllowPorts),
    PortStr = island_utils:join(Strs, ","),
    lists:flatten(["<?xml version=\"1.0\"?>",
		   "<!DOCTYPE cross-domain-policy SYSTEM \"/xml/dtds/cross-domain-policy.dtd\">",
		   "<cross-domain-policy>",
		   %%"<site-control permitted-cross-domain-policies=\"master-only\"/>",
		   "<allow-access-from domain=\"*\" to-ports=\"", PortStr, "\" />",
		   %%"<allow-access-from domain=\"*\" to-ports=\"*\" />",
		   "</cross-domain-policy>"]).



read_req(_Socket, "<policy-file-request/>\0") -> ok;
read_req(Socket, SoFar) -> 
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    read_req(Socket, SoFar ++ Data);
	_Else -> error
    end.
