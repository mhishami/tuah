-module (auth_controller).
-export ([before_filter/2, handle_request/5]).

before_filter(_, _) ->
    {ok, proceed}.

handle_request(<<"GET">>, <<"login">>, Args, Params, _Req) ->
    lager:log(info, self(), "Params = ~p, ~p", [Args, Params]),
    {"login", []};

handle_request(<<"GET">>, <<"register">>, Args, Params, _Req) ->
    lager:log(info, self(), "Params = ~p, ~p", [Args, Params]),
    {"register", []};

handle_request(_, _, _, _, _) ->
    {redirect, "/"}.
    