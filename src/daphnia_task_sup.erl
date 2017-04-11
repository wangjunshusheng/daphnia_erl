%% @private
-module (daphnia_task_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_task/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_task(Id, Mod, Args) ->
    supervisor:start_child(?SERVER, [Id, Mod, Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Opts = {simple_one_for_one, 0, 1},
    ChildSpecs = [
        {task,
            {daphnia_task, start_link, []},
            temporary, 10000, worker, [daphnia_task]}
    ],
    {ok, {Opts, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
