-module(web_server).

-export([cover_start/0, cover_stop/0, start/1, stop/1]).

-import(http_driver, [classify/1, header/1]).
-import(lists, [map/2]).

%% To start this server
%%   give the command
%%   webserver.sh start Port -- start a permanent server
%%   webserver.sh debug Port -- start a server in debugging mode
%%   webserver.sh stop       -- stop a permanent server

start(Port) -> start_on_port(Port).

stop(Port) -> tcp_server:stop(Port).

%% To run a coverage analysis
%%  1) start erlang
%%  2) run web_server:cover_start()
%%  3) run the web server for a while
%%  4) run web_server:cover_stop()

cover_start() ->
    cover:start(),
    map(fun(I) -> cover:compile(I) end, mods()),
    %% The next line *must* use the : form !!!!!
    web_server:start(['4501']).

cover_stop() ->
    map(fun(I) -> cover:analyse_to_file(I) end, mods()),
    cover:stop(),
    erlang:halt().
    
mods() ->
    [web_server, http_driver, tcp_server].

start_on_port(Port) ->
    spawn_link(fun() -> server(Port) end).

server(Port) ->
    S = self(),
    process_flag(trap_exit, true),
    http_driver:start(Port, fun(Client) -> server(Client, S) end, 15),
    loop().

loop() ->
    receive
	Any ->
	    io:format("server:~p~n",[Any]),
	    loop()
    end.

server(Client, Master) ->
    receive
	{Client, closed} ->
	    true;
	{Client, Request} ->
	    Response = generate_response(Request),
	    Client ! {self(), Response},
	    server(Client, Master)
    after 5000 ->
	    true
    end.

generate_response({_, _Vsn, F, Args, _Env}) ->
    F1 = "." ++ F,
    case file:read_file(F1) of
        {ok, Bin} ->
            case classify(F) of
                html ->
                    {header(html),[Bin]};
                jpg ->
                    {header(jpg),[Bin]};
                gif ->
                    {header(jpg),[Bin]};
                _ ->
                    {header(text),[body("white"),"<pre>",Bin,"</pre>"]}
            end;
        _ ->
            show({no_such_file,F,args,Args,cwd,file:get_cwd()})
    end.

body(X) -> ["<body bgcolor=\"", X, "\">"].


show(X) ->
    {header(text),[body("white"),"<pre>",
		   quote_lt(lists:flatten(io_lib:format("~p~n",[X]))),
		   "</pre>"]}.
	   
quote_lt([$<|T]) -> "&lt;" ++ quote_lt(T);
quote_lt([H|T])  -> [H|quote_lt(T)];
quote_lt([])     -> [].

