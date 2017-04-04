-module(mailbox).
-export([receiver_in_sequence/0,receiver_in_order/0]).

%% this will make sure that the msg1 is processed first
%% 1>c(mailbox).
%% {ok,mailbox}
%% 2>Pid = spawn(mailbox,receiver_in_order,[]).
%% <0.136.0>
%% 3> P ! msg2.
%% msg2
%% 4> P ! msg2.
%% msg2
%% 5> P ! msg1.
%% msg1
%%
%% msg1  (msg1 is processed first)
%% msg2  (msg2 is processed second)


receiver_in_sequence()->
   receive
       msg1 -> io:format("~s~n",[msg1])
   end,
   receive
       msg2 -> io:format("~s~n",[msg2])
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% this will print the msgs send to the mailbox in the order in which it is received.
 

receiver_in_order()->
    receive
       Msg ->
	    io:format("~s~n",[Msg]), %% prints the pulled msg from the mailbox.
	    timer:sleep(10000), %% pulls out each msg from the mailbox after 10 seconds.
	    receiver_in_order() %% recursive call 
    end.
