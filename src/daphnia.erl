%%%-------------------------------------------------------------------
%% @doc daphnia public API
%% @end
%%%-------------------------------------------------------------------
-module (daphnia).

-export ([start_task/3, notify_task/2, call_task/2, call_task/3]).

-type id() :: term().

%%
%% Create a new task identified by Id,
%% implemented in the Mod task module,
%% initialized with Args as task argument.
%%
-spec start_task(term(), module(), term()) ->
    ok | {error, already_started} | {error, {invalid_task_module, atom()}} | {error, any()}.
start_task(Id, Mod, Args) ->
    daphnia_task_manager:start_task(Id, Mod, Args).


%%
%% Send a message to a daphnia task identified by Id.
%%
-spec notify_task(id(), term()) ->
    ok | {error, not_found} | {error, Reason :: any()}.
notify_task(TaskId, Info) ->
    daphnia_task_manager:notify_task(TaskId, Info).

%%
%% Send a message to a daphnia task identified by Id and wait for the response.
%%

-spec call_task(id(), term()) ->
    {ok, term()} | {error, not_callable} | {error, not_found} | {error, Reason :: any()}.
call_task(TaskId, Data) ->
    daphnia_task_manager:call_task(TaskId, Data).

-spec call_task(id(), term(), timeout()) ->
    {ok, term()} | {error, not_callable} | {error, not_found} | {error, Reason :: any()}.
call_task(TaskId, Data, Timeout) ->
    daphnia_task_manager:call_task(TaskId, Data, Timeout).
