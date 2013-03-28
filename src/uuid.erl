-module (uuid).
-author('Hisham Ismail <mhishami@gmail.com').

-export([v4/0, to_string/1, get_parts/1, to_binary/1]).
-export ([gen/0, init/0, init/3]).
-export ([bin2hex/1]).

gen() ->
    to_string(v4()).

init() ->
    {A, B, C} = erlang:now(),
    init(A, B, C).
    
init(A, B, C) ->
    random:seed(A, B, C).
        
% Generates a random binary UUID.
v4() ->
  v4(random:uniform(round(math:pow(2, 48))) - 1, random:uniform(round(math:pow(2, 12))) - 1, random:uniform(round(math:pow(2, 32))) - 1, random:uniform(round(math:pow(2, 30))) - 1).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

% Returns a string representation of a binary UUID.
to_string(U) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).

% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

% Converts a UUID string in the format of 550e8400-e29b-41d4-a716-446655440000
% (with or without the dashes) to binary.
to_binary(U)->
    convert(lists:filter(fun(Elem) -> Elem /= $- end, U), []).

% Converts a list of pairs of hex characters (00-ff) to bytes.
convert([], Acc)->
    list_to_binary(lists:reverse(Acc));
convert([X, Y | Tail], Acc)->
    {ok, [Byte], _} = io_lib:fread("~16u", [X, Y]),
    convert(Tail, [Byte | Acc]).
    
bin2hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
        X <- binary_to_list(Bin)]).
    