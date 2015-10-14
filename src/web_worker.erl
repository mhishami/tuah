-module(web_worker).
-behaviour(gen_server).

-include("tuah.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {handlers}).

%% API.
-export([get_handler/1]).
-export([reload_handlers/0]).

get_handler(Handler) ->
    gen_server:call(?MODULE, {get_handler, Handler}).

reload_handlers() ->
    gen_server:call(?MODULE, {reload_handlers}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    Port = case application:get_env(tuah, http) of
        {ok, [{port, P}]} -> P;
        _ -> 8080
    end,

    % App = application:get_application(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, app_name(), "static",
                [{mimetypes, cow_mimetypes, all}]}},
            {'_', main_handler, []}
        ]}
    ]),
    ?DEBUG("Cowboy Dispatch= ~p~n", [Dispatch]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    ?INFO("Web server started on Port ~p~n", [Port]),

    case file:list_dir("priv/ssl") of
        {ok, _} ->
            SSLPort = case application:get_env(tuah, https) of
                          {ok, [{port, SP}]} -> SP;
                          _ -> 8443
                      end,
            {ok, _} = cowboy:start_https(https, 100, [
                          {port, SSLPort},
                          {cacertfile, "priv/ssl/cowboy-ca.crt"},
                          {certfile, "priv/ssl/server.crt"},
                          {keyfile, "priv/ssl/server.key"}], [
                          {env, [{dispatch, Dispatch}]}
                      ]),
            ?INFO("Secure web server started on Port ~p~n", [SSLPort]);
        {error, _} ->
            ?INFO("SSL is not configured.", [])
    end,

    {ok, #state{}}.

handle_call({get_handler, Handler}, _From, #state{handlers=Repo} = State) ->
    % ?DEBUG("Repo= ~p~n", [Repo]),
    Maps1 = case Repo of
                undefined -> reload();
                _ -> Repo
            end,
    % ?DEBUG("Handler= ~p~n", [maps:find(Handler, Maps1)]),
    {reply, maps:find(Handler, Maps1), State};

handle_call({reload_handlers}, _From, State) ->
    {reply, ok, State#state{handlers = reload()}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reload() ->
    C1 = lists:foldl(
            fun(C, Acu) ->
                {match, [{A, 16}]} = re:run(C, "_controller.beam"),
                [string:sub_string(C, 22, A)|Acu]
            end, [], filelib:wildcard("lib/*/ebin/*_controller.beam")),            
    C2 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C1 ],
    maps:from_list(C2).

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


