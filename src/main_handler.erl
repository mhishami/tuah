-module (main_handler).
-author ('Hisham Ismail <mhishami@gmail.com').

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-compile(export_all).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Vals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

    Parms = [{qs, Vals}, {body, PostVals}],
    {Controller, Action, Args, Params} =
        case get_path(Path) of
            [<<>>] ->
                {<<"home">>, <<"index">>, [], Parms};
            [C] ->
                {C, <<"index">>, [], Parms};
            [C, A] ->
                {C, A, [], Parms};
            [C, A | R] ->
                {C, A, R, Parms}                
        end,
    {ok, Req6} = case get_controllers(Controller) of
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
    Con = C:new(Req),
    
    %% we can do filter here first
    
    %% and then call the controller
    case catch Con:handle_request(Method, Action, Args, Params) of
        {'EXIT', _} ->
            do_error(Req, "'handle_request/4' not found: " ++ atom_to_list(Con));
        {ok, Data} ->
            %% render template
            Template = to_atom(Controller, "_dtl"),
            case catch Template:render(Data) of
                {ok, Content} ->
                    cowboy_req:reply(200, [], Content, Req);
                {'EXIT', _} ->
                    %% No template
                    do_error(Req, "No template found for: " ++ atom_to_list(Template))
            end;
        {redirect, Location} ->
        	cowboy_req:reply(302, [{<<"Location">>, Location}], [], Req);
        {error, Message} ->
            do_error(Req, Message)            
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
    
get_controllers(C) ->
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
