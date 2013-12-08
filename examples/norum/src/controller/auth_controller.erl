-module (auth_controller).
-export ([before_filter/2, handle_request/5]).

before_filter(_, _) ->
    {ok, proceed}.

handle_request(<<"POST">>, <<"login">>, _Args, [_, {sid, Sid}, _, {qs_body, Vals}] = Params, Req) ->
    lager:log(info, self(), "Login Params :: ~p~n, Req = ~p~n", [Params, Req]),
    Email = proplists:get_value(<<"email">>, Vals),
    Pass = proplists:get_value(<<"password">>, Vals),
    lager:log(info, self(), "Username = ~p, Password = ~p~n", [Email, Pass]),

    case Email =:= <<"foo@bar.com">> andalso Pass =:= <<"bar">> of
        true ->
            tuah:set(Sid, Email),
            {redirect, "/secure/private"};
        false ->
            {redirect, "/"}
    end;

handle_request(<<"GET">>, <<"logout">>, _, [_, {sid, Sid}, _, _], _) ->
    tuah:delete(Sid),
    {redirect, "/"};

handle_request(<<"GET">>, Action, Args, Params, _Req) ->
    lager:log(debug, self(), "Params = ~p, ~p, ~p", [Action, Args, Params]),
    {Action, []};

handle_request(_, _, _, _, _) ->
    {redirect, "/"}.
    