%%
%% Manager spawns one of these on init, to go through all ids on disk
%% and call resume on the id in the manager.
%% This is a hacky naive implementation that should be improved later.
%%
-module (daphnia_task_manager_resume_worker).
-behaviour (gen_server).

-export ([start_link/1]).

%% gen_server callbacks
-export ([init/1, terminate/2, handle_call/3,
          handle_cast/2, handle_info/2, code_change/3]).

-record (state, {
    mpid :: pid()
}).


%%====================================================================
%% API functions
%%====================================================================

start_link(ManagerPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ManagerPid], []).


%%====================================================================
%% Callbacks
%%====================================================================

init([MPid]) ->
    self() ! progress,
    {ok, #state{
        mpid=MPid
    }}.

handle_call(_R, _F, #state{}=State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(Request, State) ->
    lager:notice("unhandled cast ~p", [Request]),
    {noreply, State}.

handle_info(progress, State) ->
    {ok, Ids} = daphnia_storage:list_ids(),
    {G,B} = lists:foldl(fun(Id, {Good, Bad}) ->
        case daphnia_task_manager:resume_task(Id) of
            ok ->
                {Good+1, Bad};
            {error, R} ->
                lager:warning("resuming ~p failed with reason ~p", [Id, R]),
                {Good, Bad+1}
        end
    end, {0,0}, Ids),
    lager:notice("resumed ~p task(s), failed to resume ~p", [G, B]),
    {noreply, State};
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





