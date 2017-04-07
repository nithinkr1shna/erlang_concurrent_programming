%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0,allocate/0,deallocate/1,stop/0]).

%% These are the start functions used to create and
%% initialize the server.


start()->  %%start the frequency server by calling frequency:start() from the shell.
    register(?MODULE,spawn(?MODULE,init,[])).


init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      io:format("NewFrequencies ~w~n",[NewFrequencies]),
      io:format("Reply ~w~n",[Reply]),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      io:format("~w~n",[NewFrequencies]),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case lists:keymember(Pid,2,Allocated) of
    true -> {{[Freq|Free],Allocated},{already_allocated,Pid}}; %% check for already allocated frequency for a process id Pid.
    false-> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}} %% if not already allocated , allocates a new frequency.
       
  end.

deallocate({Free, Allocated}, Freq) ->
  case length(Allocated) of %% checks for length of list Allocated
     1 -> NewAllocated=lists:keydelete(Freq, 1, Allocated), %% if length is 1 (is allocated) then add delete the frequency and add to list of 
          {[Freq|Free],  NewAllocated};                     %% free frequencies.
     0 -> io:format("Already deallocated~n"), %% if length is 0 (not allocated) then no more deallocation, no new freq is added to list Free.
          {Free, Allocated}
  end.
  
allocate()->
    ?MODULE ! {request,self(),allocate}.

deallocate(Freq)->
   ?MODULE ! {request,self(),{deallocate,Freq}}.
stop()->
    ?MODULE ! {request,self(),stop}.
