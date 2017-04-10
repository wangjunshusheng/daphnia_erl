-module (daphnia_SUITE_lifetime).
-behaviour (daphnia_task_behaviour).

-export ([init/2, handle_info/2]).

-record (state, {
    id    :: term(),
    value :: term()
}).

init(Id, []) ->
    ct:pal("init task ~p", [Id]),
    % also valid return: {ok, #state{id=Id, value=undefined}, [], #{}}.
    {ok, #state{id=Id, value=undefined}}.

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
handle_info({schedule_noops, N}, State) ->
    ct:pal("scheduling ~p noops", [N]),
    {nowrite, State, lists:map(fun(_) -> noop end, lists:seq(1, N))};
handle_info(noop, State) ->
    ct:pal("performing noop", []),
    {nowrite, State};


handle_info(resume, State) ->
    {nowrite, State, []}.
