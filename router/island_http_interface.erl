-module(island_http_interface).

-export([start/3, stop/1]).

-import(http_driver, [classify/1, send_response/3, begin_response/3, lookup_args/2]).
-import(lists, [map/2]).
-import(island_utils, []).

-include("island_manager.hrl").


start(Port, Docroot, IslandMgrPid) -> 
    io:format("Starting HTTP Interface on: ~w.~n", [Port]),
    process_flag(trap_exit, true),
    http_driver:start(
      Port,
      fun(Header, Socket, Remainder) -> 
	      handle_request(Header, Socket, Remainder, Docroot, IslandMgrPid)
      end).

stop(Port) -> tcp_server:stop(Port).

handle_request({get, _CLen, _Vsn, "/directory", _Args, _Env}, Socket, _Remainder, _Docroot, IslandMgrPid) ->
    {response, IslandList} = gen_server:call(IslandMgrPid, {directory}, 5000),
    JsonList = map(fun island_data:island_to_json_obj/1, IslandList),
    Encoded = list_to_binary(lists:flatten([mochijson2:encode(JsonList)])),
    send_response(Socket, text, Encoded);


handle_request({get, _Vsn, "/hup", _Args, _Env}, Socket, _Remainder, _Docroot, IslandMgrPid) ->
    {response, ok} = gen_server:call(IslandMgrPid, {hup}, 5000),
    send_response(Socket, text, <<>>);


handle_request({get, _CLen, _Vsn, "/new_island", Args, _Env}, Socket, _Remainder, _Docroot, IslandMgrPid) ->
    case lookup_args(["type", "desc"], Args) of
	[{"type", IslandType}, {"desc", IslandDesc}] ->
	    {response, Isl} = gen_server:call(IslandMgrPid, {create_new_island, IslandType, IslandDesc}, 5000),
	    Data = list_to_binary(Isl#island.id),
	    send_response(Socket, text, Data);
	_Else -> 
	    send_response(Socket, error, <<>>)
    end;    


handle_request({get, _CLen, _Vsn, "/join_island", Args, _Env}, Socket, _Remainder, _Docroot, IslandMgrPid) ->
    case lookup_args(["id", "clientId"], Args) of
	[{"id", IslandId}, {"clientId", ClientId}] ->
	    case gen_server:call(IslandMgrPid, { join_island, ClientId, IslandId, Socket }, 1000) of
		{response, #island{}} ->
		    %% Send header, but don't close the socket.
		    %% Msg routing is now happening on this socket.
		    begin_response(Socket, html, 0);
		{response, no_such_island} ->
		    send_response(Socket, not_found, <<>>)
	    end;
	_Else -> 
	    send_response(Socket, error, <<>>)
    end;


handle_request({get, _CLen, _Vsn, "/get_snapshot", Args, _Env}, Socket, _Remainder, _Docroot, IslandMgrPid) ->
    case lookup_args(["id", "clientId"], Args) of
	[{"id", IslandId}, {"clientId", ClientId}] ->
	    %% Liaison will be notified if snapshot is not available or will hang around and 
	    %% facilitate data exchange once a partner starts uploading a snapshot..
	    LiaisonPid = island_snapshot_liaison:start(ClientId, IslandId, Socket),
	    gen_server:cast(IslandMgrPid, { add_snapshot_liaison, ClientId, IslandId, LiaisonPid });
	_Else -> 
	    send_response(Socket, error, <<>>)
    end;



handle_request({get, _CLen, _Vsn, F, _Args, _Env}, Socket, _Remainder, _Docroot, _IslandMgrPid) ->
    F1 = "." ++ F,
    case file:read_file(F1) of
        {ok, Bin} ->
            case classify(F) of
                html ->
		    send_response(Socket, html, Bin);
                jpg ->
		    send_response(Socket, jpg, Bin);
                gif ->
		    send_response(Socket, gif, Bin);
                _ ->
		    send_response(Socket, text, Bin)
            end;
        _ ->
	    send_response(Socket, not_found, <<>>)
    end;


handle_request({post, CLen, _Vsn, "/send_snapshot", Args, _Env}, Socket, DataSoFar, _Docroot, IslandMgrPid) ->
    case lookup_args(["id", "clientId"], Args) of
	[{"id", IslandId}, {"clientId", ClientId}] ->
	    send_response(Socket, text, <<>>),
	    case gen_server:call(IslandMgrPid, { get_snapshot_liaison, ClientId, IslandId }) of
		{response, {liaison, LiaisonPid, IslandId}} -> 
		    LiaisonPid ! {partner, list_to_binary(DataSoFar), CLen, Socket};
		_Else -> 
		    gen_tcp:close(Socket)
	    end;
	_Else -> 
	    send_response(Socket, error, <<>>)
    end.







