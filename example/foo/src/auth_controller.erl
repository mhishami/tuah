-module (auth_controller).
-export ([handle_request/5]).
-export ([before_filter/1]).

-include("foo.hrl").

before_filter(_SessionId) ->
    {ok, proceed}.

handle_request(<<"GET">>, <<"logout">>, _Args, Params, _Req) ->
    session_worker:del_cookies(maps:get(<<"sid">>, Params)),
    {redirect, <<"/">>};

handle_request(<<"GET">>, <<"login">>, _Args, _Params, _Req) ->
    {render, <<"login">>, []};  

handle_request(<<"POST">>, <<"login">> = Action, _Args, Params, _Req) ->    
    PostVals = maps:get(<<"qs_body">>, Params),
    Username = proplists:get_value(<<"username">>, PostVals),
    Password = proplists:get_value(<<"password">>, PostVals),

    case Username =:= <<>> orelse Password =:= <<>> of
        true ->
            {render, Action, [{error, <<"All fields are required.">>}]};
        _ ->
            case mongo_worker:find(?DB_USERS, #{<<"username">> => Username}) of
                {ok, []} ->
                    {render, Action, [{error, <<"Invalid username, or password">>}]};
                {ok, [User]} ->
                    ?DEBUG("User= ~p~n", [User]),
                    HashPass = web_util:hash_password(Password),
                    Pass = maps:get(<<"password">>, User),
                    case Pass =/= HashPass of
                        true ->
                            {render, Action, [{error, <<"Invalid username, or password">>}]};
                        _ ->
                            Sid = maps:get(<<"sid">>, Params),
                            session_worker:set_cookies(Sid, Username),

                            %% redirect, assuming "main" is defined.
                            {redirect, <<"/secret">>, {cookie, <<"auth">>, Username}}
                    end
            end
    end;

handle_request(<<"GET">>, <<"register">>, _Args, _Params, _Req) -> 
    {render, <<"register">>, []};

handle_request(<<"POST">>, <<"register">> = Action, _Args, Params, _Req) ->
    PostVals = maps:get(<<"qs_body">>, Params),
    Username = proplists:get_value(<<"username">>, PostVals),
    Pass1 = proplists:get_value(<<"password">>, PostVals),
    Pass2 = proplists:get_value(<<"password2">>, PostVals),

    case Username =:= <<>> orelse Pass2 =:= <<>> orelse Pass1 =:= <<>> of
        true ->
            {render, Action, [{error, <<"All fields are required.">>} | PostVals]};
        _ ->
            case Pass1 =/= Pass2 of
                true ->
                    {render, Action, [{error, <<"Passwords are not the same">>} | PostVals]};
                _ ->
                    %% ok, we can register
                    User = #{ <<"username">> => Username, 
                              <<"password">> => web_util:hash_password(Pass2),
                              <<"created_at">> => erlang:timestamp()},
                    mongo_worker:save(?DB_USERS, User),
                    {redirect, <<"/auth/login">>}
            end
    end.
