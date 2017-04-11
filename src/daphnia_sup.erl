%% @private
%%%-------------------------------------------------------------------
%% @doc daphnia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(daphnia_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, [
        {daphnia_task_manager,
            {daphnia_task_manager, start_link, []},
            permanent, 10000, worker, [daphnia_task_manager]},
        {daphnia_task_sup,
            {daphnia_task_sup, start_link, []},
            permanent, 60000, worker, [daphnia_task_sup]}
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
