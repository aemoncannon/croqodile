%%% Copyright 2007 Aemon Cannon

-module(island_utils).

-created_by('Aemon Cannon').

-include("island_manager.hrl").


-export([ parse_all_messages/1, 
	  stamp_message/3, 
	  make_client_message/3, 
	  make_heartbeat_message/2,
	  make_snapshot_req_message/2,
	  make_term_message/0,
	  encode_message/1,
	  socket_pipe/3,
	  join/2
	 ]).


%% Parse all messages in the binary Buf, return the messages and the unparsed remainder Buf.
%%
parse_all_messages(Buf) -> parse_all_messages(Buf, []).
parse_all_messages(Buf, Messages) ->
    case Buf of

	<<?MSG_TYPE_TERM:8,0:64/float,0:64/float,0:32,Rest/binary>> ->
	    {lists:reverse(Messages), Rest};

	<<Type:8,Num:64/float,Time:64/float,Len:32,Rest/binary>> when (size(Rest) >= Len) ->
	    {Payload, Remainder} = split_binary(Rest, Len),
	    parse_all_messages(Remainder, [{msg, Type, Num, Time, Payload} | Messages]);

	<<_Type:8,_Num:64/float,_Time:64/float,Len:32,Rest/binary>> when (size(Rest) < Len) ->
	    {lists:reverse(Messages), Buf};

	_Else -> 
	    {lists:reverse(Messages), Buf}
    end.

stamp_message({msg, Type, _Num, _Time, Payload}, Num, Time) -> {msg, Type, Num, Time, Payload}.

make_client_message(Num, Time, Payload) ->
    {msg, ?MSG_TYPE_NORMAL, Num, Time, Payload}.

make_heartbeat_message(Num, Time) ->
    {msg, ?MSG_TYPE_HEARTBEAT, Num, Time, <<>>}.

make_snapshot_req_message(Num, Time) ->
    {msg, ?MSG_TYPE_SNAPSHOT_REQ, Num, Time, <<>>}.

make_term_message() ->
    {msg, ?MSG_TYPE_TERM, 0, 0, <<>>}.


encode_message({msg, Type, Num, Time, Payload}) ->
    Len = size(Payload),
    <<Type:8,Num:64/float,Time:64/float,Len:32,Payload/binary>>.

socket_pipe(FromSocket, ToSocket, Len) ->
    socket_pipe(FromSocket, ToSocket, 0, Len).

socket_pipe(_FromSocket, _ToSocket, SoFar, Len) when SoFar >= Len -> ok;

socket_pipe(FromSocket, ToSocket, SoFar, Len) ->
    case gen_tcp:recv(FromSocket, 0) of
        {ok, Data} ->
	    io:format("piping data ~s~n", [Data]),
	    gen_tcp:send(ToSocket, Data),
	    NewSoFar = SoFar + size(Data),
	    if
		NewSoFar >= Len -> ok;
		true -> socket_pipe(FromSocket, ToSocket, NewSoFar, Len)
	    end;
	{error, _Reason} -> ok
    end.


%% A simple string join implementation.
join([], _Sep) -> [];
join([T], _Sep) -> T;
join([H|T], Sep) -> H ++ Sep ++ join(T, Sep).
