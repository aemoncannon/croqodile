-module(island_data).

-import(lists, [foreach/2]).
-export([
	 init_schema/0,
	 start/0
	]).


-record(island, {id, description}).

init_schema() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    case mnesia:create_table(island, [{disc_copies, [node()]}, {attributes, record_info(fields, island)}]) of
	{atomic, ok} -> io:format("Tables created.~n");
	{aborted, FailCreateReason} -> io:format("Failed to create table: ~w.~n", [FailCreateReason])
    end,
    mnesia:stop().


start() ->
    ok = mnesia:start(),
    case mnesia:wait_for_tables([island], 5000) of
	{error, Reason} -> io:format("Error waiting for tables: ~w.~n", [Reason]);
	{timeout, BadTables} -> io:format("Timed out waiting for tables: ~w.~n", [BadTables]);
	ok -> io:format("mnesia ready.~n")
    end.
