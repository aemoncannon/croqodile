-module(island_manager_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-include("island_manager.hrl").

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    get_init_result(Args).

get_init_result(Args) ->
    Flags = {one_for_one, 2, 10},
    Children = [worker_spec(island_manager_yaws1, island_manager_server,  [Args])],
    {ok, {Flags, Children}}.

worker_spec(Id, Module, Args) ->
    StartFunc = {Module, start_link, Args},
    {Id, StartFunc, permanent, ?SHUTDOWN_WAITING_TIME, worker, [Module]}.
