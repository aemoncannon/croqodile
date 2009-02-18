-module(island_test).

-export([run/0, mock_client_init/2, mock_client_message_handler/3]).

-import(http_client, [http_request/4]).
-import(http_client, [http_request_keep_open/4]).


-include("island_manager.hrl").

-define(TEST_HOST, "localhost").
-define(TEST_PORT, 6666).


run() ->
    %% Need to start for http..
    application:start(inets),

    %% Create a fresh schema
    island_data:destroy_schema(),
    island_data:start_and_create_schema(),

    TestState = island_manager_server:start_link([?TEST_PORT, ""]),

    run_test(fun test_protocol_parsing/1, TestState),
    run_test(fun test_simple_empty/1, TestState),
    run_test(fun test_one_island/1, TestState),
    run_test(fun test_join_no_island/1, TestState),
    run_test(fun test_join_no_island_id/1, TestState),
    run_test(fun test_join_island_and_send_messages/1, TestState),

    io:format("Done.~n"),
    ok.

%% Each test run gets a fresh state
run_test(Fun, TestState) -> 
    island_data:clear(),
    Fun(TestState),
    ok.


test_protocol_parsing(_TestState) ->
    Time = 12345,

    {[],<<>>} = croq_utils:parse_all_messages(<<>>),

    %% Parse one message.
    {[{msg, 1, Time, <<"hello">>}],<<>>} = croq_utils:parse_all_messages(<<1:8,Time:64,5:32,"hello">>),

    %% Parse to messages.
    {[{msg, 1, Time, <<"hello">>},{msg, 120, Time, <<"121 &*">>}],<<>>} = 
	croq_utils:parse_all_messages(<<1:8,Time:64,5:32,"hello",120:8,Time:64,6:32,"121 &*">>),
    
    %% Shouldn't parse anything if payload is too short..
    {[],<<1:8,Time:64,10:32,"hello">>} = croq_utils:parse_all_messages(<<1:8,Time:64,10:32,"hello">>),

    %% Should stop at terminator header (Type=0,Length=0)..
    {[{msg, 1, Time, <<"hello">>}],<<"don't read me'">>} = 
	croq_utils:parse_all_messages(<<1:8,Time:64,5:32,"hello",0:8,0:64,0:32,"don't read me'">>),
    ok.


test_simple_empty(_TestState) ->
    {ok, _Vsn, 200, _Reason, Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/directory"),
    [] = mochijson2:decode(Body),
    ok.

test_one_island(TestState) ->
    NewIsland = #island{id="1", description="An island.", type="lsdkfj"},
    mnesia:transaction(fun()-> ok = mnesia:write(NewIsland) end),
    {ok, AppPid} = TestState,
    {response, ok} = gen_server:call(AppPid, {hup}, 5000),
    {ok, _Vsn, 200, _Reason, Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/directory"),
    JsonObj = island_data:island_to_json_obj(NewIsland),
    [JsonObj] = mochijson2:decode(Body),
    ok.

test_join_no_island(_TestState) ->
    {ok, _Vsn, 404, _Reason, _Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/join_island?id=23"),
    ok.

test_join_no_island_id(_TestState) ->
    {ok, _Vsn, 500, _Reason, _Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/join_island"),
    ok.

test_join_island_and_send_messages({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    Id = Isl#island.id,
    Pid = create_mock_client(island_data:guid(), self()),
    Pid ! {connect_to_router, Id, ?TEST_HOST, ?TEST_PORT},
    receive
	{connected_to_router} -> ok
    end,
    Pid ! {message, "Hello"},
    receive
	{router_message, "Hello"} -> ok
    end,
    Pid ! {message, "Dude's are you out there?"},
    receive
	{router_message, "Dude's are you out there?"} -> ok
    end,
    Pid ! {message, "'dsd %#@****"},
    receive
	{router_message, "'dsd %#@****"} -> ok
    end,
    ok.



%% Utilities

create_mock_client(Id, StatusPid) ->
    Pid = spawn_link(?MODULE, mock_client_init, [Id, StatusPid]),
    Pid.

mock_client_init(Id, StatusPid) ->
    receive
	{connect_to_router, IslandId, Host, Port} ->
	    {ok, _Vsn, 200, _Reason, Extra, Socket} = http_request_keep_open(get, Host, Port, "/join_island?id=" ++ IslandId),
	    mock_client_connected_to_router_init(Id, Extra, Socket, StatusPid);
	{close} -> ok
    end.    

mock_client_connected_to_router_init(Id, BufferedData, Socket, StatusPid) ->
    StatusPid ! {connected_to_router},
    spawn_link(?MODULE, mock_client_message_handler, [BufferedData, Socket, self()]),
    mock_client_connected_to_router(Id, StatusPid, Socket).


mock_client_connected_to_router(Id, StatusPid, Socket) ->
    receive
	{router_message, Msg} -> 
	    io:format("Received msg from router: ~s~n", [Msg]),
	    StatusPid ! {router_message, Msg},
	    mock_client_connected_to_router(Id, StatusPid, Socket);
	{message, Msg} -> 
	    io:format("Sending message to router: ~s~n", [Msg]),
	    gen_tcp:send(Socket, Msg),
	    mock_client_connected_to_router(Id, StatusPid, Socket);
	{disconnect} -> ok;
	{close} -> ok
    end.


mock_client_message_handler(Buf, Socket, CallbackPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    {Messages, RemainingBuffer} = croq_utils:parse_all_messages(list_to_binary([Buf,Data])),
	    lists:foreach(fun(M) -> CallbackPid ! {router_message, M } end, Messages),
	    mock_client_message_handler(RemainingBuffer, Socket, CallbackPid);
        {error, closed} ->
            CallbackPid ! {disconnect}
    end.


