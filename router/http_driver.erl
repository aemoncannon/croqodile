-module(http_driver).

-import(lists, [map/2, reverse/1]).

-export([start/3]).
-export([classify/1, header/1]).

start(Port, Fun, Max) ->
    spawn(fun() -> server(Port, Fun, Max) end).

server(Port, Fun, Max) ->
    tcp_server:start_raw_server(Port,
				fun(Socket) -> input_handler(Socket, Fun) end, 
				Max,
			        0).

						%input_handler(Socket, Fun) ->
						%    RequestHandler = spawn_link(fun() -> Fun(S) end),
						%    S = self(),
						%    Server = spawn_link(fun() -> Fun(S) end),
						%    process_flag(trap_exit, true),
						%    relay(Socket, Server, {header, []}).

input_handler(Socket, Fun) ->
    spawn_link(fun() -> request_handler(Socket, Fun, {header, []}) end).

request_handler(Socket, Fun, {header, Buf}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bin} ->
	    Combined = list_to_binary([Buf, Bin]),
	    case parse_request(Combined) of
		{no, Remainder} -> 
		    {yes, Remainder} -> 
					end
		lists:foreach(fun(M) -> ClientPid ! {driver_message, M } end, Messages),
	    input_driver(Socket, ClientPid, RemainingBuffer);
        {error, closed} ->
            ClientPid ! {driver_closed}
    end.

receive
    {tcp, Socket, Bin} ->
	Data = binary_to_list(Bin),
	parse_request(State, Data);
    {tcp_closed, Socket} ->
	Server ! {self(), closed};
    {Server, close} ->
	gen_tcp:close(Socket);
    {Server, {Headers, BinaryData}} ->
	Len = size(BinaryData),
	Headers1 = Headers ++ "Content-Length: " ++ integer_to_list(Len) ++ "\r\n\r\n",
	gen_tcp:send(Socket, [Headers1, BinaryData]),
	relay(Socket, Server, State);
    {'EXIT', Server, _} ->
	io:format("HTTP Driver 'EXIT'~n", []),
	gen_tcp:close(Socket)
end.

parse_request({header, Buff}, Data) ->
    case scan_header(Data, Buff) of
	{no, Buff1} -> {no, Buff1 };
	{yes, Header, After} ->  {yes, parse_header(Header, After)}
    end.

%%parse_request({post, Buff, Len, X}, Socket, Server, Data) ->
%%    case collect_chunk(Len, Data, Buff) of
%%        {yes,PostData,After} ->
%%            Args2 = parse_uri_args(PostData),
%%	    {Op,Vsn,URI,Args1,Env} = X,
%%	    Request = {Op,Vsn,URI,Args1++Args2,Env,Socket},
%%            Server ! {self(), Request},
%%	    io:format("PostData ~w~n", [PostData]),
%%	    io:format("After ~w~n", [After]),
%%	    parse_request({header,[]}, Socket, Server, After);
%%        {no,Buff1, Len1} ->
%%            State = {post, Buff1, Len1, X},
%%	    relay(Socket, Server, State)
%%    end.

parse_header(Header, After) ->
    %% We've got the header - parse it
    case parse_header(Header) of
	Result = {_Op, _Vsn, _URI, _Args, _Env, _ContentLen} -> Result;
	Other ->
	    io:format("Oops ~p ~n", [Other]),
	    exit(debug)
    end.

						%collect_chunk(0,New,Buf)      -> {yes, reverse(Buf), New};
						%collect_chunk(N, [H|T], Buff) -> collect_chunk(N-1,T,[H|Buff]);
						%collect_chunk(N, [], Buff)    -> {no, Buff, N}.

scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, reverse(L), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.

%%----------------------------------------------------------------------



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



