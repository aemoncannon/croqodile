-module(island_manager_server).
-behaviour(gen_server).

-include("/usr/lib/erlang/lib/yaws-1.73/include/yaws.hrl").
-include("island_manager.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Port, WorkingDir]) ->
    process_flag(trap_exit, true),
    ServerPid = web_server:start(Port),
    {ok, ServerPid}.


handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
