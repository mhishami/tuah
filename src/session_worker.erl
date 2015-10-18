-module(session_worker).
-behaviour(gen_server).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {nodes}).

%% API.
-export([
        set_cookies/2,
        get_cookies/1,
        del_cookies/1,
        set_session/3,
        get_session/1
    ]).

%% ----------------------------------------------------------------------------
%% API
-spec set_cookies(binary(), binary()) -> {ok, any()} | {error, any()}.
set_cookies(Key, Val) ->
    gen_server:call(?MODULE, {set_cookies, Key, Val}).

-spec get_cookies(binary()) -> {ok, any()} | {error, any()}.
get_cookies(Key) ->
    gen_server:call(?MODULE, {get_cookies, Key}).

-spec del_cookies(binary()) -> {ok, any()} | {error, any()}.
del_cookies(Key) ->
    gen_server:call(?MODULE, {del_cookies, Key}).

-spec set_session(binary(), binary(), byte()) -> {ok, any()} | {error, any()}.
set_session(Key, Val, Days) when is_integer(Days) ->
    gen_server:call(?MODULE, {set_session, Key, Val, Days}).

-spec get_session(binary()) -> {ok, any()} | {error, any()}.
get_session(Key) ->
    gen_server:call(?MODULE, {get_session, Key}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% gen_server.
-spec init(list()) -> {ok, any()}.
init([]) ->
    % ?DEBUG("~p: Initializing...~n", [?MODULE]),
    application:stop(mnesia),
    Nodes = [node()|nodes()],
    % MnesiaDir = application:get_env(tuah, mnesia_dir),
    % application:set_env(mnesia, dir, MnesiaDir),
    % application:set_env(mnesia, dir, "priv/db"),
    mnesia:create_schema(Nodes),
    
    % rpc:multicall(Nodes, application, start, [mnesia]),
    application:start(mnesia),
    mnesia:create_table(tuah_session, [
        {attributes, record_info(fields, tuah_session)},
        {disc_copies, Nodes},
        {type, set}
    ]),
    mnesia:create_table(tuah_cookies, [
        {attributes, record_info(fields, tuah_cookies)},
        {disc_copies, Nodes},
        {type, set}
    ]),

    mnesia:wait_for_tables([tuah_session, tuah_cookies], 1000),
    {ok, #state{nodes = Nodes}}.

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
handle_call({set_cookies, Key, Val}, _From, State) ->
    F = fun() -> mnesia:write(#tuah_cookies{key=Key, val=Val}) end,
    ok = mnesia:activity(transaction, F),
    {reply, {ok, success}, State};

handle_call({get_cookies, Key}, _From, State) ->
    F = fun() -> mnesia:read({tuah_cookies, Key}) end,
    Reply = case catch mnesia:activity(transaction, F) of
                [{tuah_cookies, _K, V}] -> 
                    {ok, V};
                _ -> 
                    {error, undefined}
            end,
    {reply, Reply, State};

handle_call({del_cookies, Key}, _From, State) ->
    F = fun() -> mnesia:delete({tuah_cookies, Key}) end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};

handle_call({set_session, Key, Val, Days}, _From, State) ->
    Expiry = date_util:add(date(), {days, Days}),
    F = fun() ->
            mnesia:write(#tuah_session{key=Key, val=Val, expiry=Expiry})
        end,
    ok = mnesia:activity(transaction, F),
    {reply, {ok, success}, State};

handle_call({get_session, Key}, _From, State) ->
    F = fun() -> mnesia:read({tuah_session, Key}) end,
    Reply = case catch mnesia:activity(transaction, F) of
                {'EXIT', _} -> 
                    {error, undefined};
                [{tuah_session, K, V, Exp, Ts}] -> 
                    {ok, #{
                        <<"key">> => K, 
                        <<"val">> => V,
                        <<"expiry">> => Exp,
                        <<"timestamp">> => Ts
                    }}
            end,
    {reply, Reply, State};

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
