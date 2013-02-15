-module (tuah).
-author ('Hisham Ismail <mhishami@gmail.com').

-export ([start/0]).
-export ([stop/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(tuah).
    
stop() ->
    ok.
    