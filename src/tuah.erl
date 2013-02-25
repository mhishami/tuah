-module (tuah).
-author ('Hisham Ismail <mhishami@gmail.com').

-export ([start/0]).
-export ([stop/0]).
-export ([set/2, set/3, get/1, delete/1]).
-export ([reload/0, locate/1]).
-export ([sessions/0, prune/1]).

-include ("tuah.hrl").

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
	ok = ensure_started(ranch),
	ok = ensure_started(cowboy),
	ok = ensure_started(tuah).
    
stop() ->
    ok.    
    
reload() ->
    gen_server:call(?SERVER, {reload}).
    
locate(Key) ->
    % ?INFO("locate key: ~p~n", [Key]),
    gen_server:call(?SERVER, {locate, Key}).
    
sessions() ->
    gen_server:call(?SERVER, {sessions}).
    
prune(Day) ->
    gen_server:call(?SERVER, {prune, Day}).

set(Key, Value) ->
    gen_server:call(?SERVER, {set, Key, Value, 0}).

set(Key, Value, Exp) ->
    gen_server:call(?SERVER, {set, Key, Value, date_util:epoch() + Exp}).
    
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).
    
delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).
    
