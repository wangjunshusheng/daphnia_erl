-module (daphnia_SUITE_lifetime).
-behaviour (daphnia_task_behaviour).

-export ([init/2, handle_info/2]).

-record (state, {
    id     :: term(),
    value  :: term(),
    remain :: term(),
    notify :: term()
}).

init(Id, []) ->
    ct:pal("init task ~p", [Id]),
    % also valid return: {ok, #state{id=Id, value=undefined}, [], #{}}.
    {ok, #state{id=Id, value=undefined, remain=undefined, notify=undefined}}.

handle_info({ping, Pid}, #state{id=Id}=State) ->
    ct:pal("task lifetime ping from ~p", [Pid]),
    Pid ! {pong, Id, self()},
    {nowrite, State};
handle_info({set, Value}, #state{}=State) ->
    ct:pal("task lifetime set value to ~p", [Value]),
    {ok, State#state{value=Value}};
handle_info({get, Pid}, #state{}=State) ->
    ct:pal("task lifetime get value from ~p", [Pid]),
    Pid ! {value, self(), State#state.value},
    {nowrite, State, []};
handle_info(cancel, #state{}) ->
    ct:pal("task lifetime cancel", []),
    cancel;
handle_info(complete, #state{}) ->
    ct:pal("task lifetime complete", []),
    complete;
handle_info({call_self, N, Pid}, State) ->
    ct:pal("task set up to call itself ~p times, sending notification back", [N]),
    {ok, State#state{remain = N, notify = Pid}, [call_self]};
handle_info(call_self, #state{id = Id, remain = 0, notify = Pid} = State) ->
    ct:pal("task done calling it self, notifying caller"),
    Pid ! {done, Id, self()},
    {ok, State#state{remain = undefined, notify = undefined}, []};
handle_info(call_self, #state{remain = N} = State) ->
    ct:pal("task has called itself (~p times remaining)", [N-1]),
    {ok, State#state{remain = N-1}, [call_self]};
handle_info({schedule_noops, N}, State) ->
    ct:pal("scheduling ~p noops", [N]),
    {nowrite, State, lists:map(fun(_) -> noop end, lists:seq(1, N))};
handle_info(noop, State) ->
    ct:pal("performing noop", []),
    {nowrite, State};


handle_info(resume, State) ->
    {nowrite, State, []}.
