-module (main_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Vals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

    Params = [{qs_vals, Vals}, {body_qs, PostVals}],
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
    {ok, Req6} = case find_controller(Controller) of
        {ok, _Con} ->
            %% ok, found controllers
            process_request(Controller, Method, Action, Args, Params, Req5);
        _ ->
            %% error
            Cx = to_atom(Controller, "_controller"),
            do_error(Req4, "Controller not found: " ++ atom_to_list(Cx))
    end,    
    {ok, Req6, State}.

process_request(Controller, Method, Action, Args, Params, Req) ->
    C = to_atom(Controller, "_controller"),
    
    %% get the cookie for session id
    {Sid, Req2} = prepare_cookie(Req),
    
    %% do other lookup based ont his Sid, if need be.
    
    %% spawn a new Req.
    Con = C:new(Req2, Sid),
    
    %% we can do filter here first
    case catch Con:before_filter() of
        {redirect, Location} ->
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req2);
        _ ->
            %% no filter,
            handle_request(Con, Controller, Method, Action, Args, Params, Req2)
    end.

prepare_cookie(Req) ->
    {Sid, Req2} = cowboy_req:cookie(<<"_tuah">>, Req),
    case Sid of
        undefined ->
            %% set the cookie
            Uuid = uuid:gen(),
            Req3 = cowboy_req:set_resp_cookie(<<"_tuah">>, 
                     Uuid, [{path, <<"/">>}], Req2),
            {Uuid, Req3};
        _ ->
            {Sid, Req2}
    end.
            
    % io:format("sid: ~p~n", [Sid]),
    % Req2.
        
handle_request(Con, Controller, Method, Action, Args, Params, Req) ->
    %% process request
    case catch Con:handle_request(Method, Action, Args, Params) of
        {'EXIT', _} ->
            do_error(Req, "'handle_request/4' not found: " ++ atom_to_list(Con));
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
        	cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Data, Req);            
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
    
find_controller(C) ->
    Cons = filelib:wildcard("ebin/*_controller.beam"),
    find(C, Cons).        
        
find(C, [H|T]) ->
    case re:run(H, C) of
        {match, _} ->
            {ok, H};
        _ ->
            find(C, T)
    end;
find(_C, []) -> {error, none}.

terminate(_Reason, _Req, _State) ->
	ok.
