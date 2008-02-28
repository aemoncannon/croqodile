%%% Copyright 2007 Aemon Cannon

-module(croq_policy_server).

-created_by('Aemon Cannon').

-export([start/1, policy_request_handler/1]).

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).


start(Port) ->
    %% Handle requests from Flash's security-manager
    {ok, PolicyLSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(?MODULE, policy_request_handler, [PolicyLSocket]),
    ok.

%% When flash requests a security policy, shoot it back Carte blanche
policy_request_handler(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
	    io:format("Policy requested, sending..~n"),
	    ok = gen_tcp:send(Socket, "<?xml version=\"1.0\"?><cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"*\" /></cross-domain-policy>\0"),
	    ok = gen_tcp:shutdown(Socket, write);
        {error, Reason} ->
            io:format("PolicySocket accept error: ~s~n", [Reason])
    end,
    policy_request_handler(LSocket).
