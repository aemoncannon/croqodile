%%% Copyright 2007 Aemon Cannon

-module(croq_utils).

-created_by('Aemon Cannon').

-include("island_manager.hrl").


-export([ parse_all_messages/1, 
	  stamp_message/2, 
	  make_client_message/2, 
	  make_heartbeat_message/1,
	  make_snapshot_req_message/0,
	  make_term_message/0,
	  encode_message/1,
	  socket_pipe/2
	 ]).


%% Parse all messages in the binary Buf, return the messages and the unparsed remainder Buf.
%%
parse_all_messages(Buf) -> parse_all_messages(Buf, []).
parse_all_messages(Buf, Messages) ->
    case Buf of

	<<?MSG_TYPE_TERM:8,0:64,0:32,Rest/binary>> ->
	    {lists:reverse(Messages), Rest};

	<<Type:8,Time:64,Len:32,Rest/binary>> when (size(Rest) >= Len) ->
	    {Payload, Remainder} = split_binary(Rest, Len),
	    parse_all_messages(Remainder, [{msg, Type, Time, Payload} | Messages]);

	<<_Type:8,_Time:64,Len:32,Rest/binary>> when (size(Rest) < Len) ->
	    {lists:reverse(Messages), Buf};

	_Else -> 
	    {lists:reverse(Messages), Buf}
    end.

stamp_message({msg, Type, _Time, Payload}, Time) -> {msg, Type, Time, Payload}.

make_client_message(Time, Payload) ->
    {msg, ?MSG_TYPE_NORMAL, Time, Payload}.

make_heartbeat_message(Time) ->
    {msg, ?MSG_TYPE_HEARTBEAT, Time, <<>>}.

make_snapshot_req_message() ->
    {msg, ?MSG_TYPE_SNAPSHOT_REQ, 0, <<>>}.

make_term_message() ->
    {msg, ?MSG_TYPE_TERM, 0, <<>>}.


encode_message({msg, Type, Time, Payload}) ->
    Len = size(Payload),
    <<Type:8,Time:64,Len:32,Payload/binary>>.
     


socket_pipe(FromSocket, ToSocket) ->
    case gen_tcp:recv(FromSocket, 0) of
        {ok, Data} ->
	    gen_tcp:send(ToSocket, Data),
	    socket_pipe(FromSocket, ToSocket);
	{error, _Reason} -> ok
    end.
