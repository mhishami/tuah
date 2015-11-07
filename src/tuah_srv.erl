-module(tuah_srv).
-author ('Hisham Ismail <mhishami@gmail.com').

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> any().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init(list()) -> {ok, any()}.
init(Args) ->
    uuid:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, static_world, "",
                [{mimetypes, cow_mimetypes, all}]}},
            {'_', main_handler, []}
        ]}
    ]),
    Port = case os:getenv("PORT") of
               false -> 8080;
               Val -> list_to_integer(Val)
           end,
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    io:format("Web server started at port ~p...~n", [Port]),
    {ok, Args}.

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

