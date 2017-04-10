-module (daphnia_task_behaviour).

-type ret_init_opts() :: #{
    durable  => true,
    deadline => non_neg_integer()|infinity
}.

%% Init is called when a task is created.
%% Id is the client's (and everyone else's) reference to this specific task
%% Args is the argument term to the task given by the client.
%% A successful init must return {ok, State, Messages, Options}
%% which will return ok to the client's start_task call
%% {error, Reason}
-callback init(Id :: term(), Args :: term()) ->
    {ok, State :: term()}
  | {ok, State :: term(), Msgs :: [term()]}
  | {ok, State :: term(), Msgs :: [term()], Options :: ret_init_opts()}
  | {error, Reason :: term()}
  .

%% handle_info is called when a message arrives for the task
-callback handle_info(resume|term(), State :: term()) ->
    {ok|nowrite, State :: term(), Msgs :: [term()]}
  | {error, Reason :: term()} % task error, log and die
  | cancel   % stop the task and consider it cancelled
  | complete % stop the task and consider it completed
  .

%% handle_call is called when the task is called
%% This callback is optional.
%% If a client calls a task with no handle_call exported,
%% the client will receive an error.
-callback handle_call(term(), State :: term()) ->
    {ok|nowrite, Value :: term(), State :: term(), Msgs :: [term()]}
  | {error, Reason :: term()}   % task error, return error to client.
  | {cancel, Value :: term()}   % return Value to client, stop the task and consider it cancelled
  | {complete, Value :: term()} % return Value to client, stop the task and consider it completed
  .

-optional_callbacks([handle_call/2]).
