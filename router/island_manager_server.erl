-module(island_manager_server).
-behaviour(gen_server).

-include("island_manager.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

%%-record(island, {id=undefined, clients=[]}).
%%-record(snapshot, {id=undefined, data=undefined}).
-record(state, {islands=[1, 2, 3], snapshots=[]}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([Port, _WorkingDir]) ->
    process_flag(trap_exit, true),
    island_data:start(),
    _ServerPid = island_http_interface:start(Port, self()),
    {ok, #state{}}.

handle_call({directory}, _From, State=#state{islands=Islands}) ->
    {reply, {response, Islands}, State};

handle_call(Request, _From, State) -> 
    {reply, {unknown_call, Request}, State}.


handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
