-module (daphnia_SUITE_callee).
-behaviour (daphnia_task_behaviour).

-export ([init/2, handle_info/2, handle_call/2]).

-record (state, {
    id    :: term(),
    value :: term()
}).

%%
%% Test task. Allows settings result value N timees,
%% but will complete after the value is read via a call
%% 

init(Id, []) ->
    ct:pal("init task daphnia_SUITE_callee ~p", [Id]),
    {ok, #state{id=Id, value=undefined}}.
handle_info({set, Value}, #state{}=State) ->
    ct:pal("task daphnia_SUITE_callee set value to ~p", [Value]),
    {ok, State#state{value=Value}};
handle_info({sleep, Value}, #state{}=State) ->
    ct:pal("task daphnia_SUITE_callee sleeping for ~p", [Value]),
    timer:sleep(Value),
    {ok, State};
handle_info(complete, #state{}) ->
    ct:pal("task daphnia_SUITE_callee complete", []),
    complete;
handle_info(resume, State) ->
    {nowrite, State, []}.

handle_call(get, #state{value=V}) ->
    ct:pal("task daphnia_SUITE_callee complete with value ~p", [V]),
    {complete, V}.
