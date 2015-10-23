-module (web_util).
-author ('Hisham Ismail <mhishami@gmail.com').

-include ("tuah.hrl").

-export ([hash_password/1]).
-export ([hash_password/2]).
-export ([map_to_list/1]).
-export ([maps_to_list/1]).
-export ([now/0]).

-spec hash_password(binary()) -> binary().
hash_password(Password) ->
    hash_password(Password, 20).
    
-spec hash_password(binary, byte()) -> binary().   
hash_password(Password, Len) ->
    {ok, Pass} = pbkdf2:pbkdf2(sha, Password, ?SALT, 4096, Len),
    pbkdf2:to_hex(Pass).

-spec map_to_list(map()) -> list().
map_to_list(Map) ->
    lists:zip(maps:keys(Map), maps:values(Map)).

-spec maps_to_list(list()) -> list().
maps_to_list(Maps) ->
    maps_to_list(Maps, []).

maps_to_list([H|T], Accu) ->
    % ?DEBUG("Accu= ~p~n", [Accu]),
    Res = lists:zip(maps:keys(H), maps:values(H)),
    maps_to_list(T, Accu ++ [Res]);

maps_to_list([], Accu) ->
    Accu.

-spec now() -> binary().
now() ->
    format(calendar:now_to_datetime(erlang:timestamp())).

format({{Y,Mo,D}, {H,Mn,S}}) when is_float(S) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~9.6.0fZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr);

format({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).

