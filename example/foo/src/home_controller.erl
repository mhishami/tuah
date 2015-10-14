-module (home_controller).
-export ([handle_request/5]).
-export ([before_filter/2]).

-export([app_name/0]).

before_filter(Params, _Req) ->
    %% do some checking
    case maps:find(<<"auth">>, Params) of
        undefined ->
            {redirect, <<"/auth/login">>};
        _ ->
            {ok, proceed}
    end.

handle_request(<<"GET">>, _Action, _Args, _Params, _Req) ->    
    %% / will render home.dtl
    {ok, []}.

app_name() ->
	P = lists:nth(2, code:get_path()),
	T = string:tokens(P, "/"),
	A = get_last_sixth(T),
	erlang:list_to_atom(A).

get_last_sixth([H|T]) ->
	case length(T) =:= 5 of
		false ->
			get_last_sixth(T);
		true ->
			H
	end.



