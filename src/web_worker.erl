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
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, norum_web, "static",
                [{mimetypes, cow_mimetypes, all}]}},
            {'_', main_handler, []}
        ]}
    ]),
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
    {reply, maps:find(Handler, Repo), State};

handle_call({reload_handlers}, _From, State) ->
    C1 = lists:foldl(
            fun(C, Acu) ->
                {match, [{A, 12}]} = re:run(C, "_worker.beam"),
                [string:sub_string(C, 19, A)|Acu]
            end, [], filelib:wildcard("lib/tuah-*/ebin/*_worker.beam")),            
    C2 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C1 ],
    {reply, ok, State#state{handlers = maps:from_list(C2)}};

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
