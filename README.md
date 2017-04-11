Daphnia
=======

Persistent Task Manager Library

## Introduction

Daphnia is a Task Manager (library)

Daphnia runs "tasks" which are erlang modules that implement the `daphnia_task_behaviour`.
A task is made up of:
- A universally unique id (`daphnia` allows any term as the id).
  If two id terms compare equal, they are considered the same task identifier.
- A task module.
  The task module must implement `daphnia_task_behaviour`, but is otherwise free to handle it's
  lifecycle as it pleases.
- Args term for the task module's `init/2`.
  Args can be any term you want. It is used to initialize the task state in the `Module:init(Id, Args)` call.


Here is a super simple Daphnia Task Module:

```erlang
-module (my_daphnia_task).
-behaviour (daphnia_task_behaviour).

-export ([init/2, handle_info/2]).

-record (state, {
    id :: term()
}).

% Initialize the task. The Args (2. argument to init/2) term is expected to be the empty list.
% We return with an ok tuple.
% the second argument is your loop state, the third is a list of messages to send
% yourself. Use this instead of `self() ! Msg` to ensure that the messages are correctly
% restored from disk should a crash happen during the init phase.
% The 4th argument is a map of options for this task. We specify that the task is
% durable and should be persited to disk (which is the default).
init(Id, []) ->
    {ok, #state{id=Id}, [], #{durable => true}}.
% The 'resume' message is sent to us if we are recovering from a crash, or other
% condition that means that we were sent to disk and is now being resumed.
handle_info(resume, State) ->
    {ok, State, []};

% other Info is sent from the outside world to the task via daphnia:notify_task/2
% here we receive anything, print it, and return `complete` which means that
% the task is complete.
handle_info(Info, #state{id=Id}=State) ->
    io:format("I'm ~p and just received", [Info]),
    complete.
```


### legal `init/2` return:

```erlang
  {ok, State :: term(), MessagesToSelf :: [term()], Options :: map()}
| {error, Reason :: any()}
```

### legal `handle_info/2` return:

```erlang
  {ok|nowrite, State :: term(), MessagesToSelf :: [term()]} % task is not yet complete
| {error, Reason :: any()} % task failed due to Reason
| cancel % task is canceled
| complete % task is succesfully completed
```


## Call a Task

A Daphnia task can implement the optional callback `handle_call/2`.
This allows you to call, sync, and retrieve a value from the task.

If you want to retrieve a value from your task state, this is easily done with a call.

Assume that our task state is `#state{value=Value}`
where `Value` is undefined if the result is not yet available.

We can implement a call to retrieve and complete the task like this:

```erlang
handle_call(result, #state{value=undefined}) -> {error, in_progress};
handle_call(result, #state{value=Value})     -> {complete, Value}.
```

This will return an error while the value is undefined. When the value exists,
the task will return it and complete itself.

### legal `handle_call/2` return:

```erlang
  {ok|nowrite, term(), State :: term(), MessagesToSelf :: [term()]} % task is not yet complete
| {error, Reason :: any()} % task failed due to Reason
| {cancel, term()}
| {complete, term()}
```

State updates works the same way as for `handle_info/2`, however,
a call is not guarantied to reach the task (although it most likely will).
All pending task calls are dropped if the task process exits for any reason.

`handle_call/2` is intended to query the existing task state.
It should not be used to introduce new information into the task state.


### Sending Messages to a Task Pid

Sending messages to a task should be done with `daphnia:notify_task/2`.
This function makes sure that messages are persisted correctly, and handled in
order.

It is also possible to send messages directly to the task pid.
However, your task pid can change when the task resumes from disk (either due to a crash or sleep)
Sending messages directly to the task pid is useful for timeouts, etc,
but you need to keep a few things in mind:

- As a sender, you are not guaranteed delivery when sending directly to a task pid.
- The order in which messages sent directly to the pid is processed in relation to
  task notifies and task calls is undefined.

For timers, make sure you initialize new ones when you task receives the `resume`
message from daphnia.

