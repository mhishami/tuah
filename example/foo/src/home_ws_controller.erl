-module (home_ws_controller).
-behaviour (tuah_ws_controller).

-include ("foo.hrl").

-export ([version/0]).
-export ([handle_ws/2]).

version() -> 1.0.

handle_ws(Data, _Req) ->
    ?DEBUG("Data received: ~p~n", [Data]),
    case Data of
        {text, Val} ->
            Reply = jsx:decode(Val),
            ?DEBUG("Data received: ~p", [Reply]),
            {text, Reply};
        _ ->
            {text, [{ok, <<"processed">>}]}
    end.


