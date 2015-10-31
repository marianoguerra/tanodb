-module(tanodb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { tanodb_vnode_master,
                  {riak_core_vnode_master, start_link, [tanodb_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    CoverageFSMs = {tanodb_coverage_fsm_sup,
                    {tanodb_coverage_fsm_sup, start_link, []},
                    permanent, infinity, supervisor, [tanodb_coverage_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, CoverageFSMs]}}.
