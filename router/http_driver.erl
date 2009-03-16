-module(http_driver).

-import(lists, [map/2, reverse/1]).

-export([start/2]).
-export([classify/1, header/1]).
-export([send_response/3]).
-export([begin_response/2]).
-export([begin_response/3]).
-export([end_response/2]).


start(Port, Fun) ->
    spawn(fun() -> server(Port, Fun) end).


server(Port, Fun) ->
    tcp_server:start_raw_server(Port,
				fun(Socket) -> input_handler(Socket, Fun) end
			       ).

input_handler(Socket, Fun) ->
    request_handler(Socket, Fun, {header, []}).


request_handler(Socket, Fun, {header, Buf}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
	    Combined = Buf ++ binary_to_list(Bin),
	    case check_for_request(Combined) of
		{yes, Header, Remainder} -> Fun(Header, Socket, Remainder);
		{no, _Remainder} -> request_handler(Socket, Fun, {header, Combined})
	    end;
        {error, closed} -> 	    
	    io:format("Error! socket closed.", []), 
	    error
    end.


check_for_request(Buf) ->
    case scan_header(Buf) of
	{yes, Header, Remainder} ->  {yes, parse_header(Header), Remainder};
	{no, Remainder} -> {no, Remainder }
    end.


scan_header(Q) -> scan_header(Q,[]).

scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, reverse(L), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.


%%----------------------------------------------------------------------

send_response(Socket, Type, Bin) ->
    begin_response(Socket, Type),
    end_response(Socket, Bin).

begin_response(Socket, Type) ->
    ok = gen_tcp:send(Socket, lists:flatten(header(Type))).

begin_response(Socket, Type, CLen) ->
    ok = gen_tcp:send(Socket, lists:flatten([header(Type), 
					     "Content-Length: ", 
					     integer_to_list(CLen), 
					     "\r\n\r\n"])).
end_response(Socket, Bin) ->
    ok = gen_tcp:send(Socket, lists:flatten(["Content-Length: ", 
					     integer_to_list(size(Bin)), 
					     "\r\n\r\n"])),
    ok = gen_tcp:send(Socket, Bin),
    ok = gen_tcp:close(Socket).


%%---------------------------------------------------------------------

classify(FileName) ->
    case filename:extension(FileName) of
	".GIF" -> gif;
	".gif" -> gif;
	".JPG" -> jpg;
	".jpg" -> jpg;
	".jpeg" -> jpg;
	".JPEG" -> jpg;
	".HTML" -> html;
	".html" -> html;
	".HTM" -> html;
	".htm" -> html;
	_Other -> text
    end.

header(text) -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type("text/plain")];
header(html) -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type("text/html")];
header(jpg)  -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type("image/jpeg")];
header(gif)  -> 
    ["HTTP/1.0 200 Ok\r\n", powered_by(), content_type("image/gif")];
header({redirect,To}) ->
    ["HTTP/1.0 302 Come and get it!\r\n",
     powered_by(), "Location: " ++ To ++ "\r\n"];
header(not_found)  -> 
    ["HTTP/1.0 404 Not Found\r\n", powered_by()];
header(error)  -> 
    ["HTTP/1.0 500 Error\r\n", powered_by()].

powered_by() ->
    "X-Powered-By: Erlang \r\n".

content_type(X) ->
    ["Content-Type: ", X, "\r\n"].

%%----------------------------------------------------------------------

parse_header(Str) ->
    {ok, Fields} = regexp:split(Str, "\r\n"),
    PRequest = parse_request(hd(Fields)),
    %% Args = "KeyWord: Str" ..
    PArgs = map(fun isolate_arg/1, tl(Fields)),
    make_return_value({PRequest, PArgs}).

make_return_value({{Op,Vsn,{URI,Args}}, Env}) ->
    {Op, content_length(Env), Vsn, URI, Args, Env}.


content_length([{"content-length",Str} | _T]) -> list_to_integer(Str);
content_length([_|T]) ->  content_length(T);
content_length([]) ->  0.

urlencoded2str([$%,Hi,Lo|T]) -> [decode_hex(Hi, Lo)|urlencoded2str(T)];
urlencoded2str([$+|T])       -> [$ |urlencoded2str(T)];
urlencoded2str([H|T])        -> [H|urlencoded2str(T)];
urlencoded2str([])           -> [].


isolate_arg(Str) -> isolate_arg(Str, []).

isolate_arg([$:,$ |T], L) -> {string:to_lower(reverse(L)), T};
isolate_arg([H|T], L)     -> isolate_arg(T, [H|L]).

%% decode_hex %%

decode_hex(Hex1, Hex2) ->
    hex2dec(Hex1)*16 + hex2dec(Hex2).

hex2dec(X) when X >=$0, X =<$9 -> X-$0;
hex2dec($A) -> 10;
hex2dec($B) -> 11;
hex2dec($C) -> 12;
hex2dec($D) -> 13;
hex2dec($E) -> 14;
hex2dec($F) -> 15;
hex2dec($a) -> 10;
hex2dec($b) -> 11;
hex2dec($c) -> 12;
hex2dec($d) -> 13;
hex2dec($e) -> 14;
hex2dec($f) -> 15.

parse_request(Str) ->
    {ok, Args} = regexp:split(Str, " "),
    case Args of
	["POST", URI, Vsn] ->
	    {post, parse_vsn(Vsn) ,parse_uri(URI)};
	["GET", URI, Vsn] ->
	    {get, parse_vsn(Vsn), parse_uri(URI)};
	_  -> 
	    oops
    end.

parse_vsn("HTTP/1.0") -> {1,0};
parse_vsn("HTTP/1.1") -> {1,1};
parse_vsn(X) -> X.

%% A typical URI looks
%% like
%% URI = "/a/b/c?password=aaa&invisible=A+hidden+value"

parse_uri(URI) ->
    case string:tokens(URI, "?") of
	[Root] ->
	    {Root, []};
	[Root, Args] ->
	    {Root, parse_uri_args(Args)}
    end.

parse_uri_args(Args) ->
    Args1 = string:tokens(Args, "&;"),
    map(fun(KeyVal) ->
		case string:tokens(KeyVal, "=") of
		    [Key, Val] ->
			{urlencoded2str(Key), urlencoded2str(Val)};
		    [Key] ->
			{urlencoded2str(Key), ""};
		    _ ->
			io:format("Invalid str:~p~n",[KeyVal]),
			{"error", "error"}
		end
	end, Args1).



