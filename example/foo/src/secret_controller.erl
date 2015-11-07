-module (secret_controller).
-behaviour (tuah_controller).

-export ([handle_request/5]).
-export ([before_filter/1]).

-include ("baz.hrl").

before_filter(SessionId) ->
    %% do some checking
    Sid = session_worker:get_cookies(SessionId),
    case Sid of
        {error, undefined} ->
            {redirect, <<"/auth/login">>};
        _ ->
            {ok, proceed}
    end.

handle_request(<<"GET">>, _Action, _Args, Params, _Req) ->
	Username = maps:get(<<"auth">>, Params),
	{render, <<"home">>, [{user, Username}, {menu_secret, <<"active">>}]}.


