-module(island_test).

-import(lists, [foreach/2]).
-export([
	 run/0
	]).

-include("island_manager.hrl").


run() ->
    island_data:destroy_schema(),
    island_data:start_and_create_schema(),
    application:set_env(island_manager, port, 6666, 1000),
    application:set_env(island_manager, working_dir, "", 1000),
    {ok, AppPid} = island_manager:start(local, []),
    test_simple_empty({AppPid}),
    test_one_island({AppPid}),
    io:format("All tests completed.~n"),
    application:stop(island_manager),
    ok.

test_simple_empty(_State) ->
    {ok, {{_Vsn, 200, _Reason}, _Headers, Body}} = http:request(get, {"http://localhost:6666/directory", []}, [], []),
    [] = mochijson2:decode(Body),
    ok.


test_one_island(State) ->
    mnesia:transaction(
      fun()->
	      ok = mnesia:write({island, 1, "An island."})
      end),
    {AppPid} = State,
    {response, ok} = gen_server:call(AppPid, {hup}, 5000),
    {ok, {_Status, _Headers, Body}} = http:request(get, {"http://localhost:6666/directory", []}, [], []),
    [#island{id=1,description="An island."}] = mochijson2:decode(Body),
    ok.


