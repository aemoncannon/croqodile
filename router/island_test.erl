-module(island_test).

-import(lists, [foreach/2]).
-export([
	 run/0
	]).

-include("island_manager.hrl").


run() ->
    %% Need to start for http..
    application:start(inets),

    %% Create a fresh schema
    island_data:destroy_schema(),
    island_data:start_and_create_schema(),

    TestState = island_manager_server:start_link([6666, ""]),

    run_test(fun test_simple_empty/1, TestState),
    run_test(fun test_one_island/1, TestState),

    io:format("Done.~n"),
    ok.

%% Each test run gets a fresh state
run_test(Fun, TestState) -> 
    island_data:clear(),
    Fun(TestState),
    ok.


test_simple_empty(_TestState) ->
    {ok, {{_Vsn, 200, _Reason}, _Headers, Body}} = http:request(get, {"http://localhost:6666/directory", []}, [], []),
    [] = mochijson2:decode(Body),
    ok.

test_one_island(TestState) ->
    NewIsland = #island{id=1, description="An island."},
    mnesia:transaction(fun()-> ok = mnesia:write(NewIsland) end),
    {ok, AppPid} = TestState,
    {response, ok} = gen_server:call(AppPid, {hup}, 5000),
    {ok, {_Status, _Headers, Body}} = http:request(get, {"http://localhost:6666/directory", []}, [], []),
    JsonObj = island_data:island_to_json_obj(NewIsland),
    [JsonObj] = mochijson2:decode(Body),
    ok.


