-module(message_and_process).
-export([bar/0,buzz/0]).


bar()->
    timer:sleep(500),
    io:format("bar started~n"),
    io:format("bar working~n"),
    io:format("bar finished~n").

buzz()->
  receive
      stop -> io:format("Stopped~n");
      Msg -> io:format("got ~s~n",[Msg]),
	     buzz()
  end.
    
