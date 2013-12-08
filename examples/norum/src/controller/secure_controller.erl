-module (secure_controller).

-export ([before_filter/2, handle_request/5]).

before_filter(Params, _Req) ->
    %% do some checking
    User = proplists:get_value(auth, Params, undefined),
    lager:log(info, self(), "User = ~p", [User]),
    case User of
        [] ->
            {redirect, <<"/auth/login">>};
        _ ->
            {ok, proceed}
    end.

handle_request(_Method, _Action, _Args, [{auth, User}, _, _, _] = Params, _Req) ->    
    %% / will render home.dtl
    lager:log(info, self(), "Logged in user = ~p~n", [User]),
    {ok, [{user, User}]}.

