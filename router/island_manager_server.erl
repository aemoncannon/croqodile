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
    _HttpInterfacePid = island_http_interface:start(Port, self()),
    {ok, #manager_state{}}.

handle_call({directory}, _From, State=#manager_state{islands=Islands}) ->
    {reply, {response, Islands}, State};

handle_call({hup}, _From, _State) ->
    NewIslands = island_data:select_all_islands(),
    {reply, {response, ok}, #manager_state{islands=NewIslands}};

handle_call({create_new_island, Type, Description}, _From, #manager_state{islands=Islands}) ->
    Isl = island_data:new_island(Type, Description),
    {reply, { response, Isl }, #manager_state{islands=[Isl | Islands]}};


handle_call({join_island, ClientId, IslandId, Socket}, _From, State=#manager_state{islands=Islands}) ->
    case island_by_id(IslandId, Islands) of
	{value, Isl} -> 
	    if 
		is_pid(Isl#island.router_pid) ->
		    create_router_client(Isl, ClientId, Socket),
		    {reply, { response, Isl}, State};
		true ->
		    %% Start a router where none exists
		    RouterPid = island_router:start(self(), Isl),
		    UpdatedIsl = Isl#island{router_pid=RouterPid},
		    create_router_client(UpdatedIsl, ClientId, Socket),
		    UpdatedIslList = update_island(IslandId, Islands, UpdatedIsl),
		    {reply, { response, Isl}, State#manager_state{islands=UpdatedIslList}}
	    end;
	_else -> {reply, { response, no_such_island }, State}
    end.


handle_cast({get_snapshot, ClientId, IslandId, LiasonPid}, State=#manager_state{islands=Islands}) ->
    case island_by_id(IslandId, Islands) of
	{value, Isl} -> 
	    if 
		is_pid(Isl#island.router_pid) ->
		    Isl#island.router_pid ! {get_snapshot, ClientId, LiasonPid},
		    {noreply, State};
		true ->
		    LiasonPid ! snapshot_not_available,
		    {noreply, State}
	    end;
	_else -> 
	    LiasonPid ! snapshot_not_available,
	    {noreply, State}
    end.


handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> island_data:stop(), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% Utilities

create_router_client(Isl, ClientId, Socket) ->
    RouterPid = Isl#island.router_pid,
    C = #client{id=ClientId},
    ClientPid = island_router_client:start(self(), Isl, C, Socket),
    CWithPid = C#client{pid=ClientPid},
    RouterPid ! {join, CWithPid},
    ClientPid.


island_by_id(Id, Islands) -> lists:keysearch(Id, #island.id, Islands).

update_island(Id, Islands, Island) -> lists:keyreplace(Id, #island.id, Islands, Island).


