-module (home_controller).
-export ([handle_request/5]).
-export ([before_filter/2]).

before_filter(SessionId) ->
    %% do some checking
    Sid = session_worker:get_cookies(SessionId),
    case Sid of
        {error, undefined} ->
            {redirect, <<"/auth/login">>};
        _ ->
            {ok, proceed}
    end.

handle_request(<<"GET">>, _Action, _Args, _Params, _Req) ->    
    %% / will render home.dtl
    {ok, []}.


