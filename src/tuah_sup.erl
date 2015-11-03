
-module(tuah_sup).
-behaviour(supervisor).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").

-import (web_worker, [app_name/0]).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> any().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(list()) -> any().
init([]) ->
    % Web = ?CHILD(tuah_srv, worker),
    % Pool = ?CHILD(mongo_pool, worker),

    Session = ?CHILD(session_worker, worker),
    Web = ?CHILD(web_worker, worker),

    %% our mongo pool
    %% get configs
    {ok, Pools} = application:get_env(app_name(), pools),
    ?DEBUG("Pools= ~p~n", [Pools]),

    F = fun({PoolName, SizeArgs, WorkerArgs}) ->
		    PoolArgs = [{name, {local, PoolName}}, {worker_module, mc_worker}] ++ SizeArgs,
		    poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
	    end,
    PoolSpecs = lists:map(F, Pools),

    [{PoolName, _, _}] = Pools,
    Mongo = ?CHILD(mongo_worker, worker, [PoolName]),

    {ok, { {one_for_one, 5, 10}, [Session, Mongo, Web | PoolSpecs]} }.

