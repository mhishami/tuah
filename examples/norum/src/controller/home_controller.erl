-module (home_controller).
-export ([handle_request/5]).
-export ([before_filter/2]).

before_filter(Params, _Req) ->
      %% do some checking
      User = proplists:get_value(auth, Params, undefined),
      case User of
          undefined ->
              {redirect, <<"/auth/login">>};
          _ ->
              {ok, proceed}
      end.

handle_request(<<"GET">>, _Action, _Args, _Params, _Req) ->    
      %% / will render home.dtl
      {ok, []}.
