
-module (mongo_sup).
-behaviour(supervisor).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").

%% API
-export ([start_link/0]).
-export ([stop_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> any().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop_link(any()) -> any().
stop_link(PoolName) ->
    supervisor:terminate_child({local, ?MODULE}, PoolName),
    supervisor:delete_child({local, ?MODULE}, PoolName).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(list()) -> any().
init([]) ->

    Config = [
        {pools, [
            {test_pool, [
                {size, 10},
                {max_overflow, 30} 
            ],[
                {database, <<"mongo_pool_test">>},
                {w_mode, safe}
            ]}
        ]}
    ],

    %% get configs
    Pools = proplists:get_value(pools, Config),
    F = fun({PoolName, SizeArgs, WorkerArgs}) ->
            PoolArgs = [{name, {local, PoolName}}, {worker_module, mc_worker}] ++ SizeArgs,
            poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
        end,
    PoolSpecs = lists:map(F, Pools),

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

