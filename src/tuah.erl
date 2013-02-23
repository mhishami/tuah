-module (tuah).
-author ('Hisham Ismail <mhishami@gmail.com').

-export ([start/0]).
-export ([stop/0]).
-export ([set/2, get/1, delete/1]).

-define (SERVER, session_srv).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
	ok = ensure_started(crypto),
    % ok = ensure_started(mnesia),
	ok = ensure_started(ranch),
	ok = ensure_started(cowboy),
	ok = ensure_started(tuah).
    
stop() ->
    ok.    
    
set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value}).
    
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).
    
delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).
    
