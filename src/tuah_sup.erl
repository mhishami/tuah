
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

    Session = ?CHILD(session_worker, worker),
    Web = ?CHILD(web_worker, worker),

    %% our mongo pool
    %% get configs
    {ok, [{PoolName, SizeArgs, WorkerArgs}]} = application:get_env(app_name(), pools),
    PoolArgs = [{name, {local, PoolName}}, {worker_module, mc_worker}] ++ SizeArgs,
    PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),

    Mongo = ?CHILD(mongo_worker, worker, [PoolName]),

    {ok, { {one_for_one, 5, 10}, [Session, Mongo, Web, PoolSpecs]} }.

