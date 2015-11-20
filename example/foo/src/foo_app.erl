-module(foo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(tuah),
    application:start(sync),

    %% set debug for console logs
    lager:set_loglevel(lager_console_backend, debug),

    foo_sup:start_link().

stop(_State) ->
    ok.
