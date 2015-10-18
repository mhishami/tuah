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
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {QsVals, Req4} = cowboy_req:qs_vals(Req3),
    {ok, PostVals, Req5} = cowboy_req:body_qs(Req4),

    Params = #{<<"qs_vals">> => QsVals, <<"qs_body">> => PostVals},
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
    
    ?DEBUG("Controller= ~p, Action= ~p, Args= ~p~n", [Controller, Action, Args]),
    {ok, Req6} = 
        case web_worker:get_handler(Controller) of
            error ->
                %% error, try to reload the controller again
                web_worker:reload_handlers(),
                case web_worker:get_handler(Controller) of
                    error ->
                        do_error(Req5, 
                            << <<"Controller not found: ">>/binary, Controller/binary,
                            <<"_controller">>/binary >>);
                    {ok, Ctrl} ->
                        %% ok, found controllers
                        process_request(Ctrl, Controller, Method, Action, Args, Params, Req5)
                end;
            {ok, Ctrl} ->
                %% ok, found controllers
                ?DEBUG("Ctrl= ~p~n", [Ctrl]),
                process_request(Ctrl, Controller, Method, Action, Args, Params, Req5)
         end,
         
    {ok, Req6, State}.

-spec process_request(atom(), binary(), binary(), list(), list(), list(), Req) -> {ok, Req, State}
    when Req::cowboy_req:req(), State::state().
process_request(Ctrl, Controller, Method, Action, Args, Params, Req) ->
    
    {Sid, Req2} = prepare_cookie(Req),
    ?DEBUG("Processing req, Sid= ~p~n", [Sid]),
    
    %% do other lookup based on this Sid, if need be.
    
    %% we can do filter here first
    P = case session_worker:get_cookies(Sid) of
            {error, undefined} -> 
                Params#{<<"auth">> => <<"">>, <<"sid">> => Sid};
            {ok, Data} -> 
                Params#{<<"auth">> => Data, <<"sid">> => Sid}
        end,
    ?DEBUG("Checking for before_filter...~n", []),
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
    
    ?DEBUG("handle_request: Controller= ~p, Method= ~p, Action= ~p, Args= ~p~n", 
        [Controller, Method, Action, Args]),
    
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
    ?DEBUG("Rendering page, Template= ~p, Data= ~p~n", [Template, Data]),
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
    case filelib:wildcard("ebin/error_dtl.beam") of
        [] ->
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
