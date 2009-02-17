%%% Copyright 2007 Aemon Cannon

-module(croq_utils).

-created_by('Aemon Cannon').

-export([ parse_all_messages/1, msg_term/0 ]).


-define(TERMINATOR, "\r").
msg_term() -> ?TERMINATOR.

%% Parse all messages in the binary Buf, return the messages and the unparsed remainder Buf.
%%
parse_all_messages(Buf) -> parse_all_messages(Buf, []).
parse_all_messages(Buf, Messages) ->
    case Buf of
	<<Type:8,Len:32,Rest/binary>> when ((size(Rest) >= Len) and (Len > 0)) ->
	    {Payload, Remainder} = split_binary(Rest, Len),
	    parse_all_messages(Remainder, [{msg, Type, Payload} | Messages]);

	<<_Type:8,Len:32,Rest/binary>> when (size(Rest) < Len) ->
	    {lists:reverse(Messages), Buf};

	<<0:8,0:32,Rest/binary>> ->
	    {lists:reverse(Messages), Rest};

	_Else -> 
	    {lists:reverse(Messages), Buf}
    end.

