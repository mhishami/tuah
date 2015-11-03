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
-export([list_handlers/0]).
-export([reload/0]).
-export([app_name/0]).

-spec get_handler(binary()) -> {ok, atom()} | undefined.
get_handler(Handler) ->
    gen_server:call(?MODULE, {get_handler, Handler}).

-spec reload_handlers() -> {ok, any()}.
reload_handlers() ->
    gen_server:call(?MODULE, {reload_handlers}).

-spec list_handlers() -> {ok, any()}.
list_handlers() ->
    gen_server:call(?MODULE, {list_handlers}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
-spec init(list()) -> {ok, any()}.
init([]) ->
    AppName = app_name(),
    Port = case application:get_env(AppName, http) of
        {ok, [{port, P}]} -> P;
        _ -> 8080
    end,

    % App = application:get_application(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, AppName, "static",
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
            SSLPort = case application:get_env(AppName, https) of
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

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
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

handle_call({list_handlers}, _From, #state{handlers=H} = State) ->
    {reply, {ok, H}, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), any()) -> {noreply, any()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec reload() -> map().
reload() ->
    App = erlang:atom_to_list(app_name()),
    Dir = "lib/" ++ App ++ "*/ebin/*_controller.beam",
    C1 = lists:foldl(
            fun(C, Accu) ->
                L = string:tokens(C, "/"),
                L2 = lists:nth(4, L),
                L3 = string:tokens(L2, "_"),
                [lists:nth(1, L3)|Accu]
            end,
        [], filelib:wildcard(Dir)),
    C2 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C1 ],
    maps:from_list(C2).

-spec app_name() -> atom().
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


