-module (main_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(COOKIE, <<"_tuah">>).

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
init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-spec handle(Req, State) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
handle(Req, State) ->
    
    {ok, Type, Req2} = cowboy_req:parse_header(<<"content-type">>, Req),
    ?DEBUG("Req type= ~p~n", [Type]),
    case catch Type of
        {<<"multipart">>,<<"form-data">>, _} ->
            Req3 = multipart(Req2),

            {Method, Req4} = cowboy_req:method(Req3),
            {Path, Req5} = cowboy_req:path(Req4),
            {QsVals, Req6} = cowboy_req:qs_vals(Req5),
            PostVals = QsVals,
            {Files, Req7} = cowboy_req:qs(Req6),

            % ?DEBUG("Files= ~p~n", [Files]),
            handle_http(Method, Path, QsVals, PostVals, Files, Req7, State);
        _ ->

            {Method, Req3} = cowboy_req:method(Req2),
            {Path, Req4} = cowboy_req:path(Req3),
            {QsVals, Req5} = cowboy_req:qs_vals(Req4),
            {ok, PostVals, Req6} = cowboy_req:body_qs(Req5),
            ?DEBUG("PostVals= ~p~n", [PostVals]),
            Files = [],

            handle_http(Method, Path, QsVals, PostVals, Files, Req6, State)
    end.

handle_http(Method, Path, QsVals, PostVals, Files, Req, State) ->
    Params = #{<<"qs_vals">> => QsVals, 
               <<"qs_body">> => PostVals, 
               <<"files">> => Files},
    % ?DEBUG("Params= ~p~n", [Params]),
    {Controller, Action, Args} =
        case get_path(Path) of
            [<<>>] ->
                {<<"home">>, <<"index">>, []};
            [C] ->
                {C, <<"index">>, []};
            [C, A] ->
                {C, A, []};
            [C, A | R] ->
                {C, A, R}                
        end,
    
    % ?DEBUG("Controller= ~p, Action= ~p, Args= ~p~n", [Controller, Action, Args]),
    %% get the controller, if any
    Ctrl = list_to_atom(binary_to_list(<< Controller/binary, <<"_controller">>/binary >>)),
    % ?DEBUG("Ctrl= ~p", [Ctrl]),

    {ok, Req2} = 
        case catch Ctrl:before_filter(1) of
            {'EXIT', _} ->
                %% error, try to reload the controller again
                do_error(Req, 
                    << <<"Controller not found: ">>/binary, Controller/binary,
                    <<"_controller">>/binary >>);
            {ok, _} ->
                %% ok, found controllers
                % ?DEBUG("Found controller, Ctrl= ~p~n", [Ctrl]),
                process_request(Ctrl, Controller, Method, Action, Args, Params, Req)
         end,
         
    {ok, Req2, State}.

-spec process_request(atom(), binary(), binary(), list(), list(), list(), Req) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
process_request(Ctrl, Controller, Method, Action, Args, Params, Req) ->
    
    {Sid, Req2} = prepare_cookie(Req),
    % ?DEBUG("Processing req, Sid= ~p~n", [Sid]),
    
    %% do other lookup based on this Sid, if need be.
    
    %% we can do filter here first
    P = case session_worker:get_cookies(Sid) of
            {error, undefined} -> 
                Params#{<<"auth">> => <<"">>, <<"sid">> => Sid};
            {ok, Data} -> 
                Params#{<<"auth">> => Data, <<"sid">> => Sid}
        end,
    % ?DEBUG("Checking for before_filter...~n", []),
    case catch Ctrl:before_filter(Sid) of
        {redirect, Location} ->
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req2);
        _ ->
            %% no filter,
            handle_request(Ctrl, Controller, Method, Action, Args, P, Req2)
    end.

-spec prepare_cookie(Req) -> {binary(), Req} when Req::cowboy_req:req().
prepare_cookie(Req) ->    
    {Sid, Req2} = cowboy_req:cookie(?COOKIE, Req),
    case Sid of
        undefined ->
            %% set the cookie
            Uid = web_util:hash_password(word_util:gen_pnr()),
            Req3 = cowboy_req:set_resp_cookie(?COOKIE, 
                     Uid, [{path, <<"/">>}], Req2),
            {Uid, Req3};
        _ ->
            {Sid, Req2}
    end.
                    
-spec handle_request(atom(), binary(), binary(), list(), list(), list(), Req) -> {ok, Req, State}
    | {render, binary()} 
    | {render, binary(), binary()}
    | {redirect, binary()}
    | {redirect, binary(), any()}
    | {error, binary()}
    | {json, binary()}
    when Req::cowboy_req:req(), State::state().

handle_request(Ctrl, Controller, Method, Action, Args, Params, Req) ->    
    ?DEBUG("handle_request: ~p, ~p, ~p, ~p~n", [Controller, Method, Action, Args]),
    
    %% process request
    case catch Ctrl:handle_request(Method, Action, Args, Params, Req) of
        {'EXIT', _} when is_atom(Ctrl) ->
            C = list_to_binary(atom_to_list(Ctrl)),
            do_error(Req, 
                << <<"'handle_request/4' error, or not found: ">>/binary, C/binary >>);
        {'EXIT', _} ->
            do_error(Req, 
                << <<"Erorr(s) in 'handle_request/4' in ">>/binary, Controller/binary,
                <<"_controller. Please fix it">>/binary >>);
        {render, Data} ->
            %% render template
            Template = to_atom(Controller, "_dtl"),
            render_template(Template, Data, Req);
        {render, Page, Data} ->
            Template = to_atom(Page, "_dtl"),
            render_template(Template, Data, Req);
        {redirect, Location} ->
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req);
        {redirect, Location, {cookie, Key, Val}} ->
            Req2 = cowboy_req:set_resp_cookie(Key, Val, [{path, <<"/">>}], Req),
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req2);            
        {redirect, Location, {cookie, Key, Val, [{path, Path}]}} ->
            Req2 = cowboy_req:set_resp_cookie(Key, Val, [{path, Path}], Req),
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req2);            
        {error, Message} ->
            do_error(Req, Message);
        {json, Data} ->
            case catch jsx:encode(Data) of
                {'EXIT', _} ->
                    Message = jsx:encode([{<<"error">>, <<"Error in your JSON data">>}]),
                    cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], Message, Req);
                Json ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Json, Req)
            end;
        _ ->
            % catch all
            Content = << <<"Cannot process this req: ">>/binary, <<"\r\n">>/binary,
                        <<"Controller: ">>/binary, Controller/binary, <<"\r\n">>/binary,
                        <<"Action: ">>/binary, Action/binary, <<"\r\n">>/binary,
                        <<"Args: ">>/binary, Args/binary >>,
            do_error(Req, Content)
    end.
    
-spec render_template(atom(), list(), Req) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
render_template(Template, Data, Req) ->    
    % ?DEBUG("Rendering page, Template= ~p, Data= ~p~n", [Template, Data]),
    case catch Template:render(Data) of
        {ok, Content} ->
            % ?DEBUG("Template ~p found", [Template]),
            cowboy_req:reply(200, [], Content, Req);
        {'EXIT', _} ->
            %% No template
            do_error(Req, "No template found for: " ++ atom_to_list(Template) ++ 
                ", or method not implemented.")
    end.
    
-spec do_error(Req, binary()) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
do_error(Req, Message) ->
    case code:is_loaded(error_dtl) of
        false ->
            cowboy_req:reply(404, [], Message, Req);
        _ ->
            {ok, Content} = error_dtl:render([{error, Message}]),
            cowboy_req:reply(200, [], Content, Req)
    end.    
    
-spec get_path(binary()) -> list().    
get_path(Path) ->
    tl(binary:split(Path, <<"/">>, [global])).
    
-spec to_atom(binary(), list()) -> atom().
to_atom(Name, Type) ->
    binary_to_atom(iolist_to_binary([Name, Type]), utf8).
    
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
                    % ?DEBUG("Field: {~p => ~p}, ContentType= ~p~n", [
                    %     FieldName, Filename, CType]),
                    case cowboy_req:get(qs, Req2) of
                        <<>> ->
                            Req3 = stream_file(Req2),
                            Data = cowboy_req:get(qs, Req3),
                            % ?DEBUG("Binary Data= ~p~n", [#{name => FieldName, value => Filename, data => Data}]),
                            % cowboy_req:set([{qs, [#{name => FieldName, file => Filename, content_type => CType, 
                            %                         data => Data}]}], Req3);
                            cowboy_req:set([{qs, [#{FieldName => Filename, 
                                                    <<"content-type">> => CType, 
                                                    <<"data">> => Data}]}], Req3);
                        PrevData ->
                            Req3 = stream_file(Req2),
                            Data = cowboy_req:get(qs, Req3),
                            % ?DEBUG("Binary Data= ~p~n", [#{name => FieldName, value => Filename, data => Data}]),
                            cowboy_req:set([{qs, [#{FieldName => Filename, 
                                                    <<"content-type">> => CType, 
                                                    <<"data">> => Data} | PrevData]}], Req3)
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

