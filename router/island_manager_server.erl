-module(island_manager_server).
-behaviour(gen_server).

-include("island_manager.hrl").

-export([
	 start_link/1, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3
	]).

-record(manager_state, {islands=[], snapshots=[]}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Port, _WorkingDir]) ->
    process_flag(trap_exit, true),
    island_data:start(),
    _ServerPid = island_http_interface:start(Port, self()),
    {ok, #manager_state{}}.

handle_call({directory}, _From, State=#manager_state{islands=Islands}) ->
    {reply, {response, Islands}, State};

handle_call({hup}, _From, _State) ->
    NewIslands = island_data:select_all_islands(),
    {reply, {response, ok}, #manager_state{islands=NewIslands, snapshots=[]}};

handle_call({load_data}, _From, State) ->
    mnesia:transaction(
      fun()->
	      ok = mnesia:write({island, 1, "An island."})
      end),
    {reply, {response, ok}, State};

handle_call(Request, _From, State) -> 
    {reply, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> island_data:stop(), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
