
-module(tuah_sup).
-behaviour(supervisor).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").

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

init([]) ->
    % Web = ?CHILD(tuah_srv, worker),
    Session = ?CHILD(session_worker, worker),
    Pool = ?CHILD(mongo_pool, worker),
    Mongo = ?CHILD(mongo_worker, worker),
    Web = ?CHILD(web_worker, worker),
    {ok, { {one_for_one, 5, 10}, [Session, Pool, Mongo, Web]} }.

