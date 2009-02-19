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
    run_test(fun test_multiple_clients_joining_island/1, TestState),

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
    IslandId = Isl#island.id,
    CliendId = island_data:guid(),
    Pid = create_mock_client(CliendId, self()),
    Pid ! {connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},
    receive
	{router_message, CliendId, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok
    end,
    Pid ! {message, {msg, ?MSG_TYPE_NORMAL, 0, <<"Hello">>}},
    receive
	{router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Time1, <<"Hello">>}} -> ok
    end,
    Pid ! {message, {msg, ?MSG_TYPE_NORMAL, 0, <<"Dudes, are you out there?">>}},
    receive
	{router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Time2, <<"Dudes, are you out there?">>}} -> ok
    end,
    Pid ! {message, {msg, ?MSG_TYPE_NORMAL, 0, <<"'dsd %#@****">>}},
    receive
	{router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Type, <<"'dsd %#@****">>}} -> ok
    end,
    ok.

test_multiple_clients_joining_island({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    IslandId = Isl#island.id,
    CliendId1 = island_data:guid(),
    CliendId2 = island_data:guid(),
    CliendId3 = island_data:guid(),

    Pid1 = create_mock_client(CliendId1, self()),
    Pid1 ! {connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid2 = create_mock_client(CliendId2, self()),
    Pid2 ! {connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid3 = create_mock_client(CliendId3, self()),
    Pid3 ! {connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    receive
	{router_message, CliendId1, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok
    end,
    receive
	{router_message, CliendId2, {msg, ?MSG_TYPE_HEARTBEAT, _T1, _X1}} -> ok
    end,
    receive
	{router_message, CliendId3, {msg, ?MSG_TYPE_HEARTBEAT, _T2, _X2}} -> ok
    end,
    ok.





%% Mock Client:
%% A simulated client, communicating with the server via HTTP. 
%%

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
    spawn_link(?MODULE, mock_client_message_handler, [BufferedData, Socket, self()]),
    mock_client_connected_to_router(Id, StatusPid, Socket).


mock_client_connected_to_router(Id, StatusPid, Socket) ->
    receive
	{router_message, Msg} -> 
	    StatusPid ! { router_message, Id, Msg },
	    mock_client_connected_to_router(Id, StatusPid, Socket);
	{message, Msg} -> 
	    io:format("Sending message to router: ~w~n", [Msg]),
	    gen_tcp:send(Socket, croq_utils:encode_message(Msg)),
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


