%% @private
%%
%% Assumption: Only one task with the same ID ever exists.
%% gproc ensures this, but it is important to note that gproc
%% is the only guard for this.
%%

-module (daphnia_task).
-behaviour (gen_server).

-export ([start_link/3, notify/2, call/3]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3,
          handle_cast/2, handle_info/2, code_change/3]).

%% state record
-record (state, {
    id          :: term(),
    mod         :: module(),
    pstate      :: term(),        % the persisted state
    pmailbox    :: queue:queue(), % persisted "mailbox"
    popts       :: map(),         % map of task options
    durable     :: boolean(),
    callable    :: boolean(),     % if true, task module implements handle_call
    % in-flight calls to this task. Kept in state only, to easily drop
    % when task dies. Dealine is the monotonic_time(milli_seconds) time where this call can be dropped
    calls       :: #{reference() => {Data :: term(), From :: pid(), Deadline :: integer() | infinity}}
}).


-define (DEF_TASK_OPTS, #{
    durable  => true
    %deadline => infinity    
}).

-define (DEFAULT_NOTIFY_TIMEOUT, infinity).

%%====================================================================
%% API functions
%%====================================================================

start_link(Id, Module, Args) ->
    gen_server:start_link({via, gproc, {n,l,{task, Id}}}, ?MODULE, [Id, Module, Args], []).

%% @doc notify task with id Id about Info
%% Returns ok when message is queued and persisted.
%% @end
-spec notify(term(), term()) -> ok | {error, noproc} | {error, any()}.
notify(Id, Info) ->
    gen_call(Id, {notify, Info}, ?DEFAULT_NOTIFY_TIMEOUT).

%% @doc call task. Wait for task to process the call
%% Return {ok, Value} or an error.
%% @end
-spec call(term(), term(), timeout()) -> {ok, term()} | {error, noproc} | {error, task_call_not_supported} | {error, any()}.
call(Id, Data, Timeout) ->
    gen_call(Id, {call, Data, Timeout}, Timeout).

%%====================================================================
%% Callbacks
%%====================================================================

init([Id, Module, Args]) ->
    process_flag(trap_exit, true),
    lager:md([{daphnia_task_id, Id}, {daphnia_task_module, Module}]),
    PStateInit = case daphnia_storage:read_state(Id) of
        {ok, {ResumedPState0, ResumedPMailbox0, ResumedOpts0}} ->
            % add 'resume' atom to task mailbox as first thing,
            % so the task can set up non-persistent things like
            % internal timers etc.
            % this is a bit hacky, but Good Enough :tm: for now
            ResumedPMailbox1 = queue:in_r({notify, resume}, ResumedPMailbox0),
            {ok, resume, {ResumedPState0, ResumedPMailbox1, ResumedOpts0}};
        {error, not_found} ->
            % no state stored on disk, so we init the task module:
            case Module:init(Id, Args) of
                {error, _} = E ->
                    E;
                {ok, NewPState} ->
                    {ok, init, {NewPState, queue:new(), #{}}};
                {ok, NewPState, NewSelfMessages} ->
                    NewMailbox = queue:from_list(wrap_notifies(NewSelfMessages)),
                    {ok, init, {NewPState, NewMailbox, #{}}};
                {ok, NewPState, NewSelfMessages, NewOpts} ->
                    NewMailbox = queue:from_list(wrap_notifies(NewSelfMessages)),
                    {ok, init, {NewPState, NewMailbox, NewOpts}};
                Unknown ->
                    {error, {invalid_task_init_return, Unknown}}
            end
    end,
    case PStateInit of
        {error, Reason} ->
            {stop, Reason};
        {ok, StartMode, {PState0, PMailbox0, POpts0}} ->
            % check for task module compatability
            Callable = lists:member({handle_call, 2}, Module:module_info(exports)),
            % set up task progress if mailbox has message,
            % otherwise task will see progress when messages arrives for it
            POpts1 = opts_apply_default(POpts0),
            Durable = maps:get(durable, POpts1),
            State0 = maybe_progress(#state{
                        id=Id,
                        mod=Module,
                        pstate=PState0,
                        pmailbox=PMailbox0,
                        popts=POpts1,
                        callable=Callable,
                        durable=Durable,
                        calls=#{}
                    }),
            case {StartMode, Durable} of
                {init, true} ->
                    % task is being initialized, and is marked durable,
                    % write to storage:
                    case daphnia_storage:write_id(Id, Module, Args) of
                        {error, Reason} ->
                            {stop, {disk_persist_id_error, Reason}};
                        ok ->
                            ok = daphnia_storage:write_state(Id, PState0, PMailbox0, POpts1),
                            {ok, State0}
                    end;
                _ ->
                    % ask is either not durable, or not initializing
                    % so no need to write to storage
                    {ok, State0}
            end
    end.

handle_call({call, _, _}, _From, #state{callable=false}=State0) ->
    {reply, {error, not_callable}, State0};
handle_call({call, Data, Timeout}, From, #state{callable=true, calls=Calls0}=State0) ->
    CallRef = make_ref(),
    Deadline = case Timeout of
        infinity -> infinity;
        T        -> erlang:monotonic_time(milli_seconds)+T
    end,
    State1 = State0#state{calls=Calls0#{CallRef => {Data, From, Deadline}}},
    {ok, State2} = append_messages([{callref, CallRef}], State1),
    {noreply, maybe_progress(State2)};
handle_call({notify, Info}, _From, State0) ->
    {ok, State1} = append_messages([{notify, Info}], State0),
    {reply, ok, maybe_progress(State1)};
handle_call(_R, _F, #state{} = State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info('$task_progress', #state{calls=Calls0}=State0) ->
    case queue:out(State0#state.pmailbox) of
        {empty, _} ->
            {stop, task_progress_while_empty_queue, State0};
        {{value, {callref, CallRef}}, PMailbox1} ->
            % maybe schedule more progress
            State1 = maybe_progress(State0#state{pmailbox=PMailbox1}),
            % take CallRef from calls map. If not found, we simply skip it.
            % this is due to the call being dropped early.
            Now = erlang:monotonic_time(milli_seconds),
            case maps:take(CallRef, Calls0) of
                % if deadline in the future, or infinity, call it.
                {{Data, From, Deadline}, Calls1} when Deadline >= Now; Deadline=:= infinity ->
                    handle_task_call(Data, From, State1#state{calls=Calls1});
                % if deadline < now then don't perform the call.
                {{_, _, Deadline}, Calls1} when Deadline < Now ->
                    {noreply, State0#state{calls=Calls1}};
                error ->
                    {noreply, State1}
            end;
         {{value, {notify, Info}}, PMailbox1} ->
            case handle_task_notify(Info, State0#state{pmailbox=PMailbox1}) of
                {noreply, State1} ->
                    {noreply, maybe_progress(State1)};
                Reply ->
                    Reply
            end
    end;
handle_info(Info, State0) ->
    handle_task_notify(Info, State0).

% normal exit, the task is successful. delete the task storage.
terminate(normal, #state{id=Id}) ->
    ok = daphnia_storage:delete_task(Id);
% {shutdown, cancel} is special, is signals that this is a task cancel
terminate({shutdown, cancel}, #state{id=Id}) ->
    lager:info("task ~p canceled", [Id]),
    ok = daphnia_storage:delete_task(Id);
% supervisor shutdown handling:
terminate(shutdown, _State) ->
    ok;
% any other reason is an error:
terminate(Reason, #state{id=Id, mod=Mod, pstate=PState, pmailbox=PMailbox, popts=POpts}) ->
    % To write a good error message, we retrieve our start data from the ID file and write into the error.
    case daphnia_storage:read_id(Id) of
        {ok, {Id, Mod, Args}} ->
            MetaData = [
                {daphnia_task_args,    iolist_to_binary(io_lib:format("~p", [Args]))},
                {daphnia_task_state,   iolist_to_binary(io_lib:format("~p", [PState]))},
                {daphnia_task_mailbox, iolist_to_binary(io_lib:format("~p", [PMailbox]))},
                {daphnia_task_opts,    iolist_to_binary(io_lib:format("~p", [POpts]))}
            ],
            lager:error(MetaData, "task ~p failed with reason: ~p", [Id, Reason]);
        {error, not_found} ->
            % non-persisted task crash. We don't know the args.
            MetaData = [
                {daphnia_task_state,   iolist_to_binary(io_lib:format("~p", [PState]))},
                {daphnia_task_mailbox, iolist_to_binary(io_lib:format("~p", [PMailbox]))},
                {daphnia_task_opts,    iolist_to_binary(io_lib:format("~p", [POpts]))}
            ],
            lager:error(MetaData, "task ~p failed with reason: ~p", [Id, Reason]);
        {error, ReadErrorReason} ->
            lager:error("task ~p failed with reason ~p, reading the id _also_ failed with reason ~p", [Id, Reason, ReadErrorReason])
    end,
    ok = daphnia_storage:delete_task(Id).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

% merge default task options with argument
opts_apply_default(Opts) ->
    maps:merge(?DEF_TASK_OPTS, Opts).

% un-exit all gen_servers exist to provide a nicer interface to the task manager
% (instead of catching exits in the manager, we do it here, locally).
% Yes, I do realize how silly this looks.
gen_call(Id, Data, Timeout) ->
    try gen_server:call({via, gproc, {n,l,{task, Id}}}, Data, Timeout) of
        Response -> Response
    catch
        exit:{timeout,_} ->
            % as far as i can tell from gen.erl and gen_server.erl
            % timeout and {nodedown, Node} is the only exit that happens on _this_ side.
            % Other exists are the exit Reason of the process we are calling.
            exit(timeout);
        exit:{{nodedown, _}=R, _} ->
            exit(R);
        exit:{noproc, _} ->
            {error, noproc};
        exit:{normal, _} ->
            {error, {gone, complete}};
        exit:{{shutdown, cancel}, _} ->
            {error, {gone, cancel}};
        exit:{shutdown, _} ->     % a task exits with 'shutdown' when it wants to suspend,
            {error, noproc};      % so for all intents and purposes, report noproc
        exit:{Reason, Trace} ->
            lager:warning("task ~p went down while a call/notify was in-flight. Reason=~p", [Id, Reason]),
            {error, {exit, Reason, Trace}}
    end.

% Append messages to the persisted mailbox.
-spec append_messages([{notify, term()}|{callref, reference()}], #state{}) -> {ok, #state{}}.
append_messages(Messages, State0) ->
    append_messages(Messages, State0, write).
append_messages(Messages, State0, WriteMode) ->
    Id        = State0#state.id,
    POpts0    = State0#state.popts,
    PState0   = State0#state.pstate,
    PMailbox0 = State0#state.pmailbox,
    Durable   = State0#state.durable,
    % update mailbox
    PMailbox1 = queue:join(PMailbox0, queue:from_list(Messages)),
    State1 = State0#state{pmailbox=PMailbox1},
    % if we are running a durable task, persist the message to our internal
    % task mailbox before we return ok. This will make notify an at-least-once delivery
    case Durable andalso WriteMode =:= write of
        false -> {ok, State1};
        true ->
           ok = daphnia_storage:write_state(Id, PState0, PMailbox1, POpts0),
           {ok, State1}
    end.

handle_task_notify(Info, State0) ->
    TaskMod   = State0#state.mod,
    PState0   = State0#state.pstate,
    % match all valid returns from handle_info here (crash if wrong return)
    Ret = case TaskMod:handle_info(Info, PState0) of
        {ok, S, L} when is_list(L)      -> {ok, {write, S, L}};
        {ok, S}                         -> {ok, {write, S, []}};
        {nowrite, S, L} when is_list(L) -> {ok, {nowrite, S, L}};
        {nowrite, S}                    -> {ok, {nowrite, S, []}};
        {error, R}                      -> {error, R};
        cancel                          -> {ok, cancel};
        complete                        -> {ok, complete}
    end,
    case Ret of
        {error, Reason} ->
            {stop, {error, Reason}, State0};
        {ok, cancel} ->
            {stop, {shutdown, cancel}, State0};
        {ok, complete} ->
            {stop, normal, State0};
        {ok, {WriteMode, PState1, NewInfos}} ->
            {ok, State1} = append_messages(wrap_notifies(NewInfos), State0#state{pstate=PState1}, WriteMode),
            {noreply, State1}
    end.

handle_task_call(Data, From, State0) ->
    TaskMod   = State0#state.mod,
    PState0   = State0#state.pstate,
    % match all valid returns from handle_call here (crash if wrong return)
    Ret = case TaskMod:handle_call(Data, PState0) of
        {ok, V, S, L} when is_list(L)      -> {ok, V, {write, S, L}};
        {ok, V, S}                         -> {ok, V, {write, S, []}};
        {nowrite, V, S, L} when is_list(L) -> {ok, V, {nowrite, S, L}};
        {nowrite, V, S}                    -> {ok, V, {nowrite, S, []}};
        {error, R}                         -> {error, R};
        {cancel, V}                        -> {ok, V, cancel};
        {complete, V}                      -> {ok, V, complete}
    end,
    case Ret of
        {error, Reason} ->
            gen_server:reply(From, {error, Reason}),
            {noreply, State0};
        {ok, Value, cancel} ->
            gen_server:reply(From, {ok, Value}),
            {stop, {shutdown, cancel}, State0};
        {ok, Value, complete} ->
            gen_server:reply(From, {ok, Value}),
            {stop, normal, State0};
        {ok, Value, {WriteMode, PState1, NewInfos}} ->
            {ok, State1} = append_messages(wrap_notifies(NewInfos), State0#state{pstate=PState1}, WriteMode),
            gen_server:reply(From, {ok, Value}),
            {noreply, State1}
    end.

% Wrap a list of messages in {notify, _} tuple, for adding
% to the internal mailbox.
wrap_notifies(L) ->
    lists:map(fun(X) -> {notify, X} end, L).

% Maybe send a task progression message to self.
maybe_progress(#state{pmailbox=M}=S) ->
    case queue:is_empty(M) of
        true -> ok;
        false -> self() ! '$task_progress'
    end,
    S.
