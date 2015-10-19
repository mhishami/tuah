-module(upload_handler).
-behaviour(cowboy_http_handler).

-include("tuah.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-type opts() :: any().
-type state() :: any().

-spec init({atom(), http}, Req, opts())
    -> {ok, Req, state()}
    | {loop, Req, state()}
    | {loop, Req, state(), hibernate}
    | {loop, Req, state(), timeout()}
    | {loop, Req, state(), timeout(), hibernate}
    | {shutdown, Req, state()}
    | {upgrade, protocol, module()}
    | {upgrade, protocol, module(), Req, opts()}
    when Req::cowboy_req:req().

init(_, Req, _Opts) ->
    {ok, Req, undefined}.

-spec handle(Req, State) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
handle(Req, State) ->

    {ok, Type, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
    ?DEBUG("Processing request, Type = ~p, State= ~p~n", [Type, State]),
    case Type of
        {<<"multipart">>,<<"form-data">>, _} ->
            %% process multipart
            Req3 = multipart(Req2),
            Vals = cowboy_req:get(qs_vals, Req3),
            Qs = cowboy_req:get(qs, Req3),
            ?DEBUG("Vals= ~p, Qs Size= ~p~n", [Vals, length(Qs)]),
            % {ok, PostVals, Req4} = cowboy_req:body_qs(Req3),
            {ok, Req3, State};
        {<<"application">>,<<"x-www-form-urlencoded">>, _} ->
            %% process url-encoded
            {ok, PostVals, Req3} = cowboy_req:body_qs(Req2),
            ?DEBUG("PostVals= ~p~n", [PostVals]),
            {ok, Req3, State}
    end.
    

    % {ok, Headers, Req3} = cowboy_req:part(Req2),
    % {ok, Data, Req4} = cowboy_req:part_body(Req3),
    % {file, Name, Filename, ContentType, _TE}
    %     = cow_multipart:form_data(Headers),
    % ?DEBUG("Received ~p file ~p of content-type ~p as follow:~n~p~n~n",
    %     [Name, Filename, ContentType, Data]),

-spec terminate(any(), cowboy_req:req(), state()) -> ok.    
terminate(_Reason, _Req, _State) ->
    ok.

-spec multipart(cowboy_req:req()) -> cowboy_req:req().
multipart(Req) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            Req4 = case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body, Req3} = cowboy_req:part_body(Req2),
                    Vals = cowboy_req:get(qs_vals, Req3),
                    % ?DEBUG("Vals = ~p, FieldName= ~p, Value= ~p, QS= ~p~n", [
                    %     Vals, FieldName, Body, cowboy_req:get(qs, Req3)]),
                    case Vals of
                        undefined ->
                            cowboy_req:set([{qs_vals, [{FieldName, Body}]}], Req3);
                        _ ->
                            cowboy_req:set([{qs_vals, [{FieldName, Body}|Vals]}], Req3)
                    end;
                {file, FieldName, Filename, CType, _CTransferEncoding} ->
                    % ?DEBUG("FieldName= ~p, Filename= ~p, ContentType= ~p~n", [
                    %     FieldName, Filename, CType]),
                    case cowboy_req:get(qs, Req2) of
                        <<>> ->
                            Req3 = stream_file(Req2),
                            Data = cowboy_req:get(qs, Req3),
                            % ?DEBUG("Binary Data= ~p~n", [#{name => FieldName, value => Filename, data => Data}]),
                            cowboy_req:set([{qs, [#{name => FieldName, file => Filename, content_type => CType, 
                                                    data => Data}]}], Req3);
                        PrevData ->
                            Req3 = stream_file(Req2),
                            Data = cowboy_req:get(qs, Req3),
                            % ?DEBUG("Binary Data= ~p~n", [#{name => FieldName, value => Filename, data => Data}]),
                            cowboy_req:set([{qs, [#{name => FieldName, file => Filename, content_type => CType, 
                                                    data => Data} | PrevData]}], Req3)
                    end
            end,
            multipart(Req4);
        {done, Req2} ->
            Req2
    end.
 
stream_file(Req) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} ->
            % {Req2, State#state{data= << Data/binary, Body/binary >>}};
            % Req2;
            cowboy_req:set([{qs, Body}], Req2);
        {more, Body, Req2} ->
            Data = cowboy_req:get(qs, Req2),
            stream_file(cowboy_req:set([{qs, << Data/binary, Body/binary >>}], Req2))
    end.
