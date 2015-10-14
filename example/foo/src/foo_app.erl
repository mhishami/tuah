-module(foo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1, res/0]).

start(_Type, _Args) ->
    application:start(sync),
    application:ensure_all_started(lager),
    application:ensure_all_started(mongodb),    
    application:ensure_all_started(cowboy),
    application:start(erlydtl),
    application:start(merl),

    %% set debug for console logs
    lager:set_loglevel(lager_console_backend, debug),

    foo_sup:start_link().

stop(_State) ->
    ok.

res() ->
    C1 = lists:foldl(
            fun(C, Acu) ->
                {match, [{A, 16}]} = re:run(C, "_controller.beam"),
                [string:sub_string(C, 22, A)|Acu]
            end, [], filelib:wildcard("lib/*/ebin/*_controller.beam")),            
    C2 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C1 ],
    C2.
