-module(island_manager_handler).

-include("/usr/lib/erlang/lib/yaws-1.73/include/yaws.hrl").
-include("/usr/lib/erlang/lib/yaws-1.73/include/yaws_api.hrl").
-include("island_manager.hrl").

-export([out/1, handle_request/3]).

-define(TCP_OPTIONS,[binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}]).

out(Arg) ->
    _Req = Arg#arg.req,
    _ReqPath = get_path(Arg),
    Socket = Arg#arg.clisock,
    inet:setopts(Socket, ?TCP_OPTIONS),
    io:format("handling messages..~n"),
    gen_tcp:send(Socket, "hello!!"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    io:format("data: ~w", Data);
        {error, closed} -> 
	    io:format("oops"),
	    ok
    end,
    exit(normal).

get_path(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Path.

handle_request('GET', [47,97,99,99,111,117,110,116 | _], _Arg) -> % "/account" ...
    make_response(200, "<p>Please login or logout.</p>");

handle_request('GET', [47,112,114,111,102,105,108,101 | _], _Arg) -> % "/profile" ...
    make_response(200, "<p>This is a slick profile.</p>");

handle_request(_, _, _Arg) -> % catchall
    make_response(200, "<p>What exactly are you looking for?</p>").

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) ->
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

handle_client_messages(Socket) ->
    case gen_tcp:recv(Socket, 5) of
        {ok, Data} ->
	    io:format("data: ~w", Data),
	    handle_client_messages(Socket);
        {error, closed} -> ok
    end.
