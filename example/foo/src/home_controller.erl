-module (home_controller).
-behaviour (tuah_controller).

-export ([handle_request/5]).
-export ([before_filter/1]).

-include ("foo.hrl").

before_filter(_SessionId) ->
	{ok, proceed}.

handle_request(<<"GET">>, _Action, _Args, _Params, _Req) ->
	{render, <<"public">>, []}.


