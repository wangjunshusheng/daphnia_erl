%% @private
-module (daphnia_task_manager).
-behaviour (gen_server).

-export ([start_link/0, start_task/3, resume_task/1, notify_task/2, call_task/2, call_task/3]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3,
          handle_cast/2, handle_info/2, code_change/3]).


-record (state, {
}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_task(term(), module(), term()) ->
    ok | {error, already_started} | {error, {invalid_task_module, atom()}} | {error, any()}.
start_task(Id, Mod, Args) when is_atom(Mod) ->
    gen_server:call(?MODULE, {start_task, Id, Mod, Args});
start_task(_Id, _Mod, _Args) ->
    error(badarg).

%% Notify task with id Id about Info. Will wake up from disk if required.
%% If the task doesn't exist, {error, not_found} is returned.
%% If the task is notified successfully, ok is returned.
-spec notify_task(term(), term()) -> ok | {error, not_found} | {error, any()}.
notify_task(Id, Info) ->
    case daphnia_task:notify(Id, Info) of
        ok ->
            ok;
        {error, noproc} ->
            % process not running. Try to resume it:
            case resume_task(Id) of
                ok ->
                    daphnia_task:notify(Id, Info);
                {error, already_started} ->
                    daphnia_task:notify(Id, Info);
                {error, _} = E ->
                    E
            end;
        {error, {gone, _}}=E ->
            E
    end.

-spec call_task(term(), term()) -> {ok, Result} | {error, Reason}
    when
        Result :: term(),
        Reason :: not_callable | not_found | {gone, complete | cancel} | term().
call_task(Id, Data) ->
    call_task(Id, Data, infinity).

-spec call_task(term(), term(), timeout()) -> {ok, Result} | {error, Reason}
    when
        Result :: term(),
        Reason :: not_callable | not_found | {gone, complete | cancel} | term().
call_task(Id, Data, Timeout) ->
    case daphnia_task:call(Id, Data, Timeout) of
        {ok, Value} ->
            {ok, Value};
        {error, noproc} ->
            % process not running. Try to resume it:
            case resume_task(Id) of
                ok ->
                    daphnia_task:call(Id, Data, Timeout);
                {error, already_started} ->
                    daphnia_task:call(Id, Data, Timeout);
                {error, _} = E ->
                    E
            end;
        {error, not_callable} ->
            {error, not_callable};
        {error, {gone, _}}=E ->
            E
    end.

-spec resume_task(term()) -> ok | {error, not_found} | {error, any()}.
resume_task(Id) ->
    % before doing a lookup, check that the task is not already running
    case gproc:lookup_pids({n,l,{task, Id}}) of
        [_Pid] ->
            {error, already_started};
        [] ->
            case gen_server:call(?MODULE, {lookup_task, Id}) of
                {ok, {Id, Mod, Args}} ->
                    start_task(Id, Mod, Args);
                {error, _} = E ->
                    E
            end
    end.

%%====================================================================
%% Callbacks
%%====================================================================

init([]) ->
    {ok, _Pid} = daphnia_task_manager_resume_worker:start_link(self()),
    {ok, #state{

    }}.

handle_call({start_task, Id, Module, Args}, _F, #state{}=State) ->
    case claims_behaviour(daphnia_task_behaviour, Module) of
        true ->
            case daphnia_task_sup:start_task(Id, Module, Args) of
                {ok, _Pid} ->
                    {reply, ok, State};
                {error, {already_started, _Pid}} ->
                    {reply, {error, already_started}, State};
                {error, Reason} ->
                    {reply, {error, {start_task_error, Reason}}, State}
            end;
        false ->
            {reply, {error, {invalid_task_module, Module}}, State};
        {error, not_a_module} ->
            {reply, {error, {not_a_module, Module}}, State}
    end;
handle_call({lookup_task, Id}, _F, #state{}=State) ->
    % if not found in ets, look in storage:
    case daphnia_storage:read_id(Id) of
        {ok, {Id, Module, Args}} ->
            {reply, {ok, {Id, Module, Args}}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call(_R, _F, #state{}=State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    lager:notice("unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

claims_behaviour(Behaviour, Module) ->
    try Module:module_info(attributes) of
        Attrs ->
            case lists:keyfind(behaviour, 1, Attrs) of
                false ->
                    false;
                {behaviour, Behaviours} ->
                    lists:any(fun(B) ->
                            B == Behaviour
                    end, Behaviours)
            end
    catch
        error:undef ->
            {error, not_a_module}
    end.




