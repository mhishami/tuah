-module (main_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(COOKIE, <<"_tuah">>).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Vals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

    Params = [{qs_vals, Vals}, {qs_body, PostVals}],
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
    {ok, Req6} = 
        case tuah:locate(Controller) of
            undefined ->
                %% error, try to reload the controller again
                tuah:reload(),
                case tuah:locate(Controller) of
                    undefined ->
                        do_error(Req4, 
                            << <<"Controller not found: ">>/binary, Controller/binary,
                            <<"_controller">>/binary >>);
                    Con ->
                        %% ok, found controllers
                        process_request(Con, Controller, Method, Action, Args, Params, Req5)
                end;
            Con ->
                %% ok, found controllers
                process_request(Con, Controller, Method, Action, Args, Params, Req5)
         end,
         
    {ok, Req6, State}.

process_request(Ctrl, Controller, Method, Action, Args, Params, Req) ->
    
    %% get the cookie for session id
    {Sid, Req2} = prepare_cookie(Req),
    
    %% do other lookup based on this Sid, if need be.
    
    %% we can do filter here first
    P = case tuah:get(Sid) of
            undefined -> [[], {sid, Sid}|Params];
            Data -> [{auth, Data}, {sid, Sid}|Params]
        end,
    case catch Ctrl:before_filter(P, Req2) of
        {redirect, Location} ->
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req2);
        _ ->
            %% no filter,
            handle_request(Ctrl, Controller, Method, Action, Args, P, Req2)
    end.

prepare_cookie(Req) ->
    {Sid, Req2} = cowboy_req:cookie(?COOKIE, Req),
    case Sid of
        undefined ->
            %% set the cookie
            Uuid = uuid:gen(),
            Req3 = cowboy_req:set_resp_cookie(?COOKIE, 
                     Uuid, [{path, <<"/">>}], Req2),
            {Uuid, Req3};
        _ ->
            {Sid, Req2}
    end.
            
    % io:format("sid: ~p~n", [Sid]),
    % Req2.
        
handle_request(Con, Controller, Method, Action, Args, Params, Req) ->
    %% process request
    case catch Con:handle_request(Method, Action, Args, Params, Req) of
        {'EXIT', _} when is_atom(Con) ->
            C = list_to_binary(atom_to_list(Con)),
            do_error(Req, 
                << <<"'handle_request/4' error, or not found: ">>/binary, C/binary >>);
        {'EXIT', _} ->
            do_error(Req, 
                << <<"Erorr(s) in 'handle_request/4' in ">>/binary, Controller/binary,
                <<"_controller. Please fix it">>/binary >>);
        {ok, Data} ->
            %% render template
            Template = to_atom(Controller, "_dtl"),
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
        {Page, Data} ->
            Template = to_atom(Page, "_dtl"),
            render_template(Template, Data, Req);
        _ ->
            % catch all
            Content = << <<"Cannot process this req: ">>/binary, <<"\r\n">>/binary,
                        <<"Controller: ">>/binary, Controller/binary, <<"\r\n">>/binary,
                        <<"Action: ">>/binary, Action/binary, <<"\r\n">>/binary,
                        <<"Args: ">>/binary, Args/binary >>,
            do_error(Req, Content)
    end.
    
render_template(Template, Data, Req) ->
    case catch Template:render(Data) of
        {ok, Content} ->
            cowboy_req:reply(200, [], Content, Req);
        {'EXIT', _} ->
            %% No template
            do_error(Req, "No template found for: " ++ atom_to_list(Template))
    end.
    
do_error(Req, Message) ->
    case filelib:wildcard("ebin/error_dtl.beam") of
        [] ->
            cowboy_req:reply(404, [], Message, Req);
        _ ->
            {ok, Content} = error_dtl:render([{error, Message}]),
            cowboy_req:reply(200, [], Content, Req)
    end.    
    
get_path(Path) ->
    tl(binary:split(Path, <<"/">>, [global])).
    
to_atom(Name, Type) ->
    binary_to_atom(iolist_to_binary([Name, Type]), latin1).
    
terminate(_Reason, _Req, _State) ->
	ok.
