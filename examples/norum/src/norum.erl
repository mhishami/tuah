-module(norum).

-export ([start/0]).
-export ([stop/0]).

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
      ok = ensure_started(tuah),
      ok = ensure_started(norum).

stop() ->
      application:stop(norum),
      application:stop(tuah),
      application:stop(cowboy),
      application:stop(ranch),
      application:stop(crypto).
