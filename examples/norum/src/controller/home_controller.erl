-module (home_controller).
-export ([handle_request/5]).
-export ([before_filter/2]).

before_filter(Params, _Req) ->
    {ok, proceed}.

handle_request(_Method, _Action, _Args, [{auth, User}, _, _, _] = Params, _Req) ->    
    %% / will render home.dtl
    lager:log(info, self(), "User = ~p~n", [User]),
    {ok, [{user, User}]}.
