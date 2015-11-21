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
-export([app_name/0]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
-spec init(list()) -> {ok, any()}.
init([]) ->
    AppName = foo, %app_name(),
    Port = case application:get_env(AppName, http) of
        {ok, [{port, P}]} -> P;
        _ -> 8080
    end,

    % App = application:get_application(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, AppName, "static",
                [{mimetypes, cow_mimetypes, all}]}},
            {"/ws", ws_handler, []},
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

-spec app_name() -> atom().
app_name() ->
    P = lists:nth(2, code:get_path()),
    T = lists:reverse(string:tokens(P, "/")),
    erlang:list_to_atom(lists:nth(6, T)).

