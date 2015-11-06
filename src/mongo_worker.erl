-module(mongo_worker).
-behaviour(gen_server).
-author ('Hisham Ismail <mhishami@gmail.com').

-include("tuah.hrl").

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {pool}).

%% API.
%% ----------------------------------------------------------------------------
-export([
    save/2,             %% save the records
    update/2,           %% update by new Doc
    update/3,           %% update by selector with new doc
    update/4,           %% update by selector with new doc, with args
    find_one/2,         %% find first item by selector
    find_one/3,         %% find first item by selector, with args - {projector, skip}
    find/2,             %% find all items by selector
    find/3,             %% find all items by selector, with args - {batchsize, skip, projector}
    match/3,            %% find items by selector, sort
    match/4,            %% find items by selector, sort, limit
    match/5,            %% find items by selector, sort, skip, limit
    delete/2,           %% delete all by selector
    delete_one/2,       %% delete one by selector
    count/2,            %% count by selector
    count/3,            %% count by selector, limit
    ensure_index/2      %% index collection by index spec - {key, name, unique, dropDups}
]).

-spec save(binary(), any()) -> {ok, any()} | {error, any()}.
save(Coll, Doc) ->
    gen_server:call(?MODULE, {save, Coll, Doc}).

-spec update(binary(), any()) -> {ok, any()} | {error, any()}.
update(Coll, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Doc}).

-spec update(binary(), any(), any()) -> {ok, any()} | {error, any()}.
update(Coll, Selector, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Selector, Doc}).

-spec update(binary(), any(), any(), list()) -> {ok, any()} | {error, any()}.
update(Coll, Selector, Doc, Args) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Selector, Doc, Args}).

-spec find_one(binary(), any()) -> {ok, any()} | {error, any()}.
find_one(Coll, Selector) ->
    gen_server:call(?MODULE, {find_one, Coll, Selector}).

% Example:
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 1}, {skip, 1}]).
% 
-spec find_one(binary(), any(), list()) -> {ok, any()} | {error, any()}.
find_one(Coll, Selector, Args) ->
    gen_server:call(?MODULE, {find_one, Coll, Selector, Args}).

-spec find(binary(), any()) -> {ok, any()}.
find(Coll, Selector) ->
    gen_server:call(?MODULE, {find, Coll, Selector, []}).

% Example
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 3}, {skip, 1}, 
%   {projector, {<<"created_at">>, 1, <<"grpid">>, 1}}]).
% 
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 3}, {skip, 1}, 
%   {projector, #{<<"created_at">> => 1, <<"grpid">> => 1}}
% ]).

-spec find(binary(), any(), list()) -> {ok, any()}.
find(Coll, Selector, Args) ->
    gen_server:call(?MODULE, {find, Coll, Selector, Args}).

-spec match(binary(), any(), any()) -> {ok, any()}.
match(Coll, Selector, Sort) ->
    gen_server:call(?MODULE, {match, Coll, Selector, Sort}).

-spec match(binary(), any(), any(), byte()) -> {ok, any()}.
match(Coll, Selector, Sort, Limit) ->
    gen_server:call(?MODULE, {match, Coll, Selector, Sort, Limit}).

-spec match(binary(), any(), any(), byte(), byte()) -> {ok, any()}.
match(Coll, Selector, Sort, Skip, Limit) ->
    gen_server:call(?MODULE, {match, Coll, Selector, Sort, Skip, Limit}).

-spec delete(binary(), any()) -> {ok, any()} | {error, any()}.
delete(Coll, Selector) ->
    gen_server:call(?MODULE, {delete, Coll, Selector}).

-spec delete_one(binary(), any()) -> {ok, any()} | {error, any()}.
delete_one(Coll, Selector) ->
    gen_server:call(?MODULE, {delete_one, Coll, Selector}).

-spec count(binary(), any()) -> {ok, any()} | {error, any()}.
count(Coll, Selector) ->
    gen_server:call(?MODULE, {count, Coll, Selector, 0}).

-spec count(binary(), any(), byte()) -> {ok, any()} | {error, any()}.
count(Coll, Selector, Limit) ->
    gen_server:call(?MODULE, {count, Coll, Selector, Limit}).

-spec ensure_index(binary(), any()) -> any().
ensure_index(Coll, IndexSpec) ->
    gen_server:call(?MODULE, {ensure_index, Coll, IndexSpec}).

%% gen_server implementation.
%% ----------------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()}.
start_link(PoolName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, PoolName, []).

%% gen_server.
-spec init(list()) -> {ok, any()}.
init(PoolName) ->
    {ok, #state{pool=PoolName}}.

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
handle_call({save, Coll, Doc}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:insert(Conn, Coll, Doc)
            end),
    {reply, {ok, Reply}, State};

handle_call({update, Coll, Doc}, _From, #state{pool=Pool} = State) ->
    Id = maps:get(<<"_id">>, Doc),
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:update(Conn, Coll, {<<"_id">>, Id}, {<<"$set">>, Doc})
            end),            
    {reply, {ok, Reply}, State};

handle_call({update, Coll, Selector, Doc}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:update(Conn, Coll, Selector, Doc)
            end),            
    {reply, {ok, Reply}, State};

handle_call({update, Coll, Selector, Doc, Args}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:update(Conn, Coll, Selector, Doc, Args)
            end),            
    {reply, {ok, Reply}, State};

handle_call({find_one, Coll, Selector}, _From, #state{pool=Pool} = State) ->    
    Res = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:find_one(Conn, Coll, Selector)
            end),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
            end,
    {reply, Reply, State};

handle_call({find_one, Coll, Selector, Args}, _From, #state{pool=Pool} = State) ->    
    Res = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:find_one(Conn, Coll, Selector, Args)
            end),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
            end,
    {reply, Reply, State};

% mongo_worker:find(<<"posts">>, 
%   {<<"title">>, #{<<"$regex">>  => <<"some*">>, <<"$options">> => <<"i">>}}, 
%   [{projector, #{<<"grpid">> => 1, <<"title">> => 1, <<"author.fullname">> => 1}}]).
% 
handle_call({find, Coll, Selector, Args}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                Cursor = mongo:find(Conn, Coll, Selector, Args),
                Results = mc_cursor:rest(Cursor),
                mc_cursor:close(Cursor),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Selector, Sort}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Selector},
                        {<<"$sort">>, Sort}
                    ]}),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Selector, Sort, Limit}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Selector},
                        {<<"$sort">>, Sort},
                        {<<"$limit">>, Limit}
                    ]}),
                Results
            end),

    {reply, {ok, Res}, State};

handle_call({match, Coll, Selector, Sort, Skip, Limit}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Selector},
                        {<<"$sort">>, Sort},
                        {<<"$skip">>, Skip},
                        {<<"$limit">>, Limit}
                    ]}),
                Results
            end),

    {reply, {ok, Res}, State};

handle_call({delete, Coll, Selector}, _From, #state{pool=Pool} = State) ->    
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:delete(Conn, Coll, Selector)
            end),
    {reply, {ok, Reply}, State};

handle_call({delete_one, Coll, Selector}, _From, #state{pool=Pool} = State) ->    
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:delete_one(Conn, Coll, Selector)
            end),
    {reply, {ok, Reply}, State};

handle_call({count, Coll, Selector, Limit}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:count(Conn, Coll, Selector, Limit)
            end),
    {reply, {ok, Reply}, State};

% Example:
% mongo_worker:ensure_index(<<"posts">>, #{<<"key">> => {<<"grpid">>, 1}}).
% 
handle_call({ensure_index, Coll, IndexSpec}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:ensure_index(Conn, Coll, IndexSpec)
            end),
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


