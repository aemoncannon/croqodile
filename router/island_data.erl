-module(island_data).

-import(lists, [foreach/2]).
-export([
	 destroy_schema/0,
	 start_and_create_schema/0,
	 select_all_islands/0,
	 start/0,
	 stop/0,
	 island_to_json_obj/1,
	 clear/0,
	 guid/0,
	 new_island/2
	]).

-include("island_manager.hrl").
-include_lib("stdlib/include/qlc.hrl").

destroy_schema() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

start_and_create_schema() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:create_table(island, [{disc_copies, [node()]}, {attributes, record_info(fields, island)}]) of
	{atomic, ok} -> io:format("Tables created.~n");
	{aborted, FailCreateReason} -> io:format("Failed to create table: ~w.~n", [FailCreateReason])
    end,
    ok.

clear() ->
    {atomic, ok} = mnesia:clear_table(island),
    ok.

%% Note: start/0 must be called under same node as start_and_create_schema/0, otherwise they won't share the same
%% data.
start() ->
    ok = mnesia:start(),
    case mnesia:wait_for_tables([island], 5000) of
	{error, Reason} -> io:format("Error waiting for tables: ~w.~n", [Reason]);
	{timeout, BadTables} -> io:format("Timed out waiting for tables: ~w.~n", [BadTables]);
	ok -> ok
    end.


stop() -> mnesia:stop(), ok.


new_island(Type, Description) -> #island{id=guid(), type=Type, description=Description}.


select_all_islands() -> do(qlc:q([X || X <- mnesia:table(island)])).


do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


island_to_json_obj(#island{id=Id, type=Type, description=Description}) ->
    {struct, [
	      {<<"id">>, Id},
	      {<<"description">>, list_to_binary(Description)},
	      {<<"type">>, list_to_binary(Type)}
	     ]}.


guid() -> <<I:128/integer>> = erlang:md5(term_to_binary({node(), make_ref(), now()})), 
	  erlang:integer_to_list(I, 16). 



