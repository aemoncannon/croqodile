%%% Copyright 2007 Aemon Cannon

-module(croq_utils).

-created_by('Aemon Cannon').

-export([ parse_all_sentences/2, parse_one_sentence/1, sentence_term/0 ]).

-define(TERMINATOR, [12,12]).

sentence_term() -> ?TERMINATOR.

%% Parse all sentences in the string Buf, return the sentences and the remainder of the string.
parse_all_sentences(Buf, Sentences) ->
    case string:str(Buf, sentence_term()) of
	N when N /= 0 -> parse_all_sentences(string:substr(Buf, N + length(sentence_term())), 
					     Sentences ++ [string:substr(Buf, 1, N - 1)]);
	
	_ -> 		{Sentences, Buf}
    end.

%% Parse the first sentence in the string Buf, return the sentence and the remainder of the string.
parse_one_sentence(Buf) ->
    case string:str(Buf, sentence_term()) of
	N when N /= 0 -> {ok,
			  string:substr(Buf, 1, N - 1),
			  string:substr(Buf, N + length(sentence_term()))};
	
	_ -> 		{none, Buf}
    end.



