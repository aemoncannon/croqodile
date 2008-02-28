%%% Copyright 2007 Aemon Cannon

-module(croq_logger).

-created_by('Aemon Cannon').

-export([start/0, log_handler/1]).

-define(LOG_FILENAME, "router.log").

start() ->
    {ok, LogFileDevice} = file:open(?LOG_FILENAME, [write]),
    OpenMessage = io_lib:fwrite("~s logger: started, pid=~w, file=~s~n",
				[string_timestamp(), self(), ?LOG_FILENAME]),
    ok = file:write(LogFileDevice, OpenMessage),
    register(logger, spawn(?MODULE, log_handler, [LogFileDevice])),
    ok.


log_handler(LogFileDevice) ->
    receive
        {string, String} -> 
	    ok = file:write(LogFileDevice, String ++ "\n"),
	    log_handler(LogFileDevice);
	_ ->
	    log_handler(LogFileDevice)
    end.



string_timestamp() ->
    {MegaSecs,Secs,_Microsecs} = now(),
    integer_to_list(MegaSecs) ++ integer_to_list(Secs).



