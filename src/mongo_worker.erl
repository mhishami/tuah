-module(mongo_worker).
-behaviour(gen_server).
-author ('Hisham Ismail <mhishami@gmail.com').

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

-record(state, {}).

%% API.
%% ----------------------------------------------------------------------------
-export([
    save/2,             %% save the records
    update/2,           %% update Doc
    find_one/2,         %% find first item by selector
    find/2,             %% find all items by selector
    find/3,             %% find all items by selector
    find/4,
    match/3,
    match/4,
    delete/2,
    test/0              %% test app
]).

-spec save(binary(), any()) -> {ok, any()} | {error, any()}.
save(Coll, Doc) ->
    gen_server:call(?MODULE, {save, Coll, Doc}).

-spec update(binary(), any()) -> {ok, any()} | {error, any()}.
update(Coll, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Doc}).

-spec find_one(binary(), any()) -> {ok, any()} | {error, any()}.
find_one(Coll, Selector) ->
    gen_server:call(?MODULE, {find_one, Coll, Selector}).

-spec find(binary(), any()) -> {ok, any()} | {error, any()}.
find(Coll, Selector) ->
    gen_server:call(?MODULE, {find, Coll, Selector, []}).

-spec find(binary(), any(), any()) -> {ok, any()} | {error, any()}.
find(Coll, Selector, Projector) ->
    gen_server:call(?MODULE, {find, Coll, Selector, Projector}).

-spec find(binary(), any(), any(), any()) -> {ok, any()} | {error, any()}.
find(Coll, Selector, Projector, Limit) ->
    gen_server:call(?MODULE, {find, Coll, Selector, Projector, Limit}).

-spec match(binary(), any(), any()) -> {ok, any()}.
match(Coll, Selector, Sort) ->
    gen_server:call(?MODULE, {match, Coll, Selector, Sort}).

-spec match(binary(), any(), any(), any()) -> {ok, any()}.
match(Coll, Selector, Sort, Limit) ->
    gen_server:call(?MODULE, {match, Coll, Selector, Sort, Limit}).

-spec delete(binary(), any()) -> {ok, any()} | {error, any()}.
delete(Coll, Selector) ->
    gen_server:call(?MODULE, {delete, Coll, Selector}).

%% gen_server implementation.
%% ----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
-spec init(list()) -> {ok, any()}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
handle_call({save, Coll, Doc}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Reply = mongo:insert(Conn, Coll, Doc),
    {reply, {ok, Reply}, State};

handle_call({update, Coll, Doc}, _From, State) ->
    ?DEBUG("Updating Doc= ~p~n", [Doc]),
    {ok, Conn} = mongo_pool:get(Coll),
    Id = maps:get(<<"_id">>, Doc),
    Reply = mongo:update(Conn, Coll, {<<"_id">>, Id}, {<<"$set">>, Doc}),
    {reply, {ok, Reply}, State};

handle_call({find_one, Coll, Selector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Res = mongo:find_one(Conn, Coll, Selector),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
            end,
    {reply, Reply, State};

handle_call({find, Coll, Selector, Projector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Cursor = mongo:find(Conn, Coll, Selector, Projector),
    Res = mc_cursor:rest(Cursor),
    mc_cursor:close(Cursor),
    {reply, {ok, Res}, State};

handle_call({find, Coll, Selector, Projector, Limit}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Cursor = mongo:find(Conn, Coll, Selector, Projector),
    Res = mc_cursor:take(Cursor, Limit),
    mc_cursor:close(Cursor),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Selector, Sort}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    {true, #{<<"result">> := Res}} = mongo:command(Conn,
        {<<"aggregate">>, Coll, <<"pipeline">>, [
            {<<"$match">>, Selector},
            {<<"$sort">>, Sort}
        ]}),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Selector, Sort, Limit}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    {true, #{<<"result">> := Res}} = mongo:command(Conn,
        {<<"aggregate">>, Coll, <<"pipeline">>, [
            {<<"$match">>, Selector},
            {<<"$sort">>, Sort},
            {<<"$limit">>, Limit}
        ]}),
    {reply, {ok, Res}, State};

handle_call({delete, Coll, Selector}, _From, State) ->
    {ok, Conn} = mongo_pool:get(Coll),
    Reply = mongo:delete(Conn, Coll, Selector),
    {reply, {ok, Reply}, State};

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

%% ----------------------------------------------------------------------------
-spec test() -> ok.
test() ->
    users:new(<<"Hisham Ismail">>, <<"hisham@mail.com">>, <<"sasa">>),
    ok.

