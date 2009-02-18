%%% Copyright 2007 Aemon Cannon

-module(croq_utils).

-created_by('Aemon Cannon').

-export([ parse_all_messages/1, 
	  stamp_message/2, 
	  create_client_message/2, 
	  create_heartbeat_message/1,
	  create_snapshot_req_message/0,
	  create_term_message/0
	 ]).


-define(MSG_TYPE_TERM, 0).
-define(MSG_TYPE_SNAPSHOT_REQ, 1).
-define(MSG_TYPE_NORMAL, 2).
-define(MSG_TYPE_HEARTBEAT, 3).

%% Parse all messages in the binary Buf, return the messages and the unparsed remainder Buf.
%%
parse_all_messages(Buf) -> parse_all_messages(Buf, []).
parse_all_messages(Buf, Messages) ->
    case Buf of
	<<Type:8,Time:64,Len:32,Rest/binary>> when ((size(Rest) >= Len) and (Len > 0)) ->
	    {Payload, Remainder} = split_binary(Rest, Len),
	    parse_all_messages(Remainder, [{msg, Type, Time, Payload} | Messages]);

	<<_Type:8,_Time:64,Len:32,Rest/binary>> when (size(Rest) < Len) ->
	    {lists:reverse(Messages), Buf};

	<<0:8,0:64,0:32,Rest/binary>> ->
	    {lists:reverse(Messages), Rest};

	_Else -> 
	    {lists:reverse(Messages), Buf}
    end.

stamp_message(<<Type:8,_Time:64,PayloadLen:32,Payload/binary>>, Time) ->
    <<Type:8,Time:64,PayloadLen:32,Payload/binary>>.

create_client_message(Time, Payload) ->
    PayloadLen = size(Payload),
    <<?MSG_TYPE_NORMAL:8,Time:64,PayloadLen:32,Payload/binary>>.

create_heartbeat_message(Time) ->
    <<?MSG_TYPE_HEARTBEAT:8,Time:64,0:32,0:8>>.

create_snapshot_req_message() ->
    <<?MSG_TYPE_SNAPSHOT_REQ:8,0:64,0:32,0:8>>.

create_term_message() ->
    <<?MSG_TYPE_TERM:8,0:64,0:32,0:8>>.
     
