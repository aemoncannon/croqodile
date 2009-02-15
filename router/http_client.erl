-module(http_client).

-export([http_request/4, http_request_keep_open/4]).

-import(lists, [nth/2]).
-import(lists, [append/1]).
-import(lists, [takewhile/2]).
-import(lists, [dropwhile/2]).
-import(lists, [member/2]).


-define(HEAD_SEP, "\r\n").
-define(HEAD_TERM, "\r\n\r\n").


http_request(get, Host, Port, Path) ->
    Request = "GET " ++ Path ++ " " ++ "HTTP/1.0" ++ ?HEAD_TERM,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}, {keepalive, true}]),
    ok = gen_tcp:send(Socket, Request),
    Raw = finish_request(Socket, ""),
    {ok, Fields} = regexp:split(Raw, ?HEAD_SEP),
    Body = append(dropwhile(fun not_empty/1, Fields)),
    Head = hd(Fields),
    {ok, Pieces} = regexp:split(Head, " "),
    {ok, 
     nth(1, Pieces), 
     list_to_integer(nth(2, Pieces)), 
     nth(3, Pieces),
     Body
    }.

finish_request(Socket, Buf) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    finish_request(Socket, Buf ++ Data);
        {error, closed} ->
            Buf
    end.



http_request_keep_open(get, Host, Port, Path) ->
    Request = "GET " ++ Path ++ " " ++ "HTTP/1.0" ++ ?HEAD_TERM,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}, {keepalive, true}]),
    ok = gen_tcp:send(Socket, Request),
    {ok, Socket, Fields, Extra} = finish_request_keep_open(Socket, [], []),
    Head = hd(Fields),
    {ok, Pieces} = regexp:split(Head, " "),
    {ok,
     nth(1, Pieces), 
     list_to_integer(nth(2, Pieces)), 
     nth(3, Pieces),
     Extra,
     Socket
    }.


finish_request_keep_open(Socket, Fields, Buf) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    Combined = Buf ++ Data,
	    {ok, Tokens} = regexp:split(Combined, ?HEAD_SEP),
	    HeadComplete = member("", Tokens),
	    if
		HeadComplete -> 
		    HeaderFields = takewhile(fun not_empty/1, Tokens),
		    Remaining = append(dropwhile(fun not_empty/1, Tokens)),
		    {ok, Socket, HeaderFields, Remaining};
		true -> 
		    finish_request_keep_open(Socket, Fields, Combined)
	    end;
        {error, closed} ->
	    {ok, Socket, Fields, Buf}
    end.


not_empty(L) -> length(L) > 0.
