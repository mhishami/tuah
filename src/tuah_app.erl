-module(tuah_app).
-author ('Hisham Ismail <mhishami@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    word_util:init(),
    application:start(sync),
    application:ensure_all_started(lager),
    application:ensure_all_started(mongodb),    
    application:ensure_all_started(cowboy),
    application:start(erlydtl),
    application:start(merl),

    %% set debug for console logs
    lager:set_loglevel(lager_console_backend, debug),
    
    tuah_sup:start_link().

stop(_State) ->
    ok.
