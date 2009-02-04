-module(island_manager).
-behaviour(application).

-export([start/2, stop/1]).

-include("island_manager.hrl").

start(_Type, _Args) ->
    application:start(inets),
    Args = lists:map(
        fun (Var) -> {ok, Value} = application:get_env(?MODULE, Var), Value end,
        [port, working_dir]
    ),
    island_manager_sup:start_link(Args).

stop(_State) -> ok.
