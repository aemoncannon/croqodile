-module(island_test).

-export([run/0, mock_client_init/2, mock_client_message_handler/3]).

-import(http_client, [http_request/4, http_request/5, http_request_keep_open/4]).


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
    run_test(fun test_state_accumulation/1, TestState),
    run_test(fun test_snapshot_request/1, TestState),

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
    {ok, _Vsn, 404, _Reason, _Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/join_island?id=23&clientId=1"),
    ok.


test_join_no_island_id(_TestState) ->
    {ok, _Vsn, 500, _Reason, _Body} = http_request(get, ?TEST_HOST, ?TEST_PORT, "/join_island"),
    ok.


test_join_island_and_send_messages({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    IslandId = Isl#island.id,
    CliendId = island_data:guid(),
    Pid = create_mock_client(CliendId, self()),
    Pid ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    %% Wait for a heartbeat to make sure we're connected to router.    
    receive {status_router_message, CliendId, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok end,

    Pid ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"Hello">>}},
    receive {status_router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Time1, <<"Hello">>}} -> ok end,

    Pid ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"Dudes, are you out there?">>}},
    receive {status_router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Time2, <<"Dudes, are you out there?">>}} -> ok end,

    Pid ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"'dsd %#@****">>}},
    receive {status_router_message, CliendId, {msg, ?MSG_TYPE_NORMAL, _Type, <<"'dsd %#@****">>}} -> ok end,
    ok.


test_multiple_clients_joining_island({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    IslandId = Isl#island.id,
    CliendId1 = island_data:guid(),
    CliendId2 = island_data:guid(),
    CliendId3 = island_data:guid(),

    Pid1 = create_mock_client(CliendId1, self()),
    Pid1 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid2 = create_mock_client(CliendId2, self()),
    Pid2 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid3 = create_mock_client(CliendId3, self()),
    Pid3 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    %% Wait for some heartbeats to make sure we're connected to router.
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_HEARTBEAT, _T1, _X1}} -> ok end,
    receive {status_router_message, CliendId3, {msg, ?MSG_TYPE_HEARTBEAT, _T2, _X2}} -> ok end,
    ok.



test_state_accumulation({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    IslandId = Isl#island.id,
    CliendId1 = island_data:guid(),
    CliendId2 = island_data:guid(),

    Pid1 = create_mock_client(CliendId1, self()),
    Pid1 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid2 = create_mock_client(CliendId2, self()),
    Pid2 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    %% Wait for some heartbeats to make sure we're connected to router.
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_HEARTBEAT, _T1, _X1}} -> ok end,


    %% Now send user message, verifying their correct routing..
    Pid1 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"A">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"A">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"A">>}} -> ok end,

    Pid2 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"B">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"B">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"B">>}} -> ok end,

    Pid1 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"C">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"C">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"C">>}} -> ok end,


    %% Now verify the accumulated snapshot in each mock client process.
    Pid1 ! {please_echo_state},
    Pid2 ! {please_echo_state},
    receive {status_state, CliendId1, <<"ABC">>} -> ok end,

    receive {status_state, CliendId2, <<"ABC">>} -> ok end,

    ok.


test_snapshot_request({ok, AppPid}) ->
    {response, Isl} = gen_server:call(AppPid, {create_new_island, "House", "A nice one."}, 5000),
    IslandId = Isl#island.id,
    CliendId1 = island_data:guid(),
    CliendId2 = island_data:guid(),

    Pid1 = create_mock_client(CliendId1, self()),
    Pid1 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    Pid2 = create_mock_client(CliendId2, self()),
    Pid2 ! {please_connect_to_router, IslandId, ?TEST_HOST, ?TEST_PORT},

    %% Wait for some heartbeats to make sure we're connected to router.
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_HEARTBEAT, _T, _X}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_HEARTBEAT, _T1, _X1}} -> ok end,

    %% Now send user message, verifying their correct routing..
    Pid1 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"A">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"A">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"A">>}} -> ok end,

    Pid2 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"B">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"B">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"B">>}} -> ok end,


    Pid1 ! {please_send_message, {msg, ?MSG_TYPE_NORMAL, 0, <<"C">>}},
    receive {status_router_message, CliendId1, {msg, ?MSG_TYPE_NORMAL, _, <<"C">>}} -> ok end,
    receive {status_router_message, CliendId2, {msg, ?MSG_TYPE_NORMAL, _, <<"C">>}} -> ok end,


    %% Trigger a snapshot request..
    Pid1 ! {please_request_snapshot},

    %% Partner should receive request..
    receive 
	{status_snapshot_requested, CliendId2 } -> ok;
	{status_snapshot_requested, CliendId1 } -> io:format("Fuck, I don't have a snapshot. I asked YOU for one.", [])
    end,

    receive 
	{status_got_snapshot, CliendId1, <<"ABC">> } -> ok
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
	{please_connect_to_router, IslandId, Host, Port} ->
	    {ok, _Vsn, 200, _Reason, Extra, Socket} = http_request_keep_open(
							get, Host, Port, 
							"/join_island?id=" ++ IslandId ++ "&clientId=" ++ Id
						       ),
	    mock_client_connected_to_router_init(Id, IslandId, {Host, Port}, Extra, Socket, StatusPid);
	{close} -> ok
    end.    

mock_client_connected_to_router_init(Id, IslandId, Server, BufferedData, Socket, StatusPid) ->
    spawn_link(?MODULE, mock_client_message_handler, [BufferedData, Socket, self()]),
    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, <<>>).


mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot) ->
    receive
	{router_message, {msg, ?MSG_TYPE_TERM, _, _}} -> 
	    io:format("OOPS, mock_client got MSG_TYPE_TERM.~n",[]),
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{router_message, Msg={msg, ?MSG_TYPE_SNAPSHOT_REQ, _, _}} -> 
	    io:format("Got snapshot request from router.~n", []),
	    StatusPid ! { status_router_message, Id, Msg },
	    StatusPid ! { status_snapshot_requested, Id },
	    %%  Here's where we need to connect and upload the snapshot.
	    %%
	    {Host, Port} = Server,
	    {ok, _Vsn, 200, _Reason, _Body} = http_request(
						post, Host, Port, 
						"/send_snapshot?id=" ++ IslandId ++ "&clientId=" ++ Id,
						CurSnapshot
					       ),
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{router_message, Msg={msg, ?MSG_TYPE_HEARTBEAT, _, _}} -> 
	    StatusPid ! { status_router_message, Id, Msg },
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{router_message, Msg={msg, ?MSG_TYPE_NORMAL, _, Payload}} -> 
	    StatusPid ! { status_router_message, Id, Msg },
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, list_to_binary([CurSnapshot, Payload]));


	{please_send_message, Msg} -> 
	    io:format("Sending message to router: ~w~n", [Msg]),
	    gen_tcp:send(Socket, croq_utils:encode_message(Msg)),
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{please_echo_state} -> 
	    io:format("Replying with current state. ~n", []),
	    StatusPid ! {status_state, Id, CurSnapshot},
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{please_request_snapshot} -> 
	    io:format("Sending snapshot request.~n", []),
	    {Host, Port} = Server,
	    {ok, _Vsn, 200, _Reason, SnapshotData} = http_request(
						       get, Host, Port, 
						       "/get_snapshot?id=" ++ IslandId ++ "&clientId=" ++ Id
						      ),
	    StatusPid ! {status_got_snapshot, Id, SnapshotData},
	    mock_client_connected_to_router(Id, IslandId, Server, StatusPid, Socket, CurSnapshot);

	{disconnect} -> io:format("mock client got {disconnect}.~n",[]);

	{close} -> io:format("mock client got {close}.~n",[]);

	Else -> io:format("mock client received unknown message.~w~n", [Else])
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


