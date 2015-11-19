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
    update/3,           %% update by match with new doc
    update/4,           %% update by match with new doc, with args
    find_one/2,         %% find first item by match
    find_one/3,         %% find first item by match, with args - {projector, skip}
    find/2,             %% find all items by match
    find/3,             %% find all items by match, with args - {batchsize, skip, projector}
    match/3,            %% find items by match, sort
    match/4,            %% find items by match, sort, limit
    match/5,            %% find items by match, sort, skip, limit
    match_group/3,      %% find items by match, group
    match_group/4,      %% find items by match, group, sort
    match_group/5,      %% find items by match, project, group, sort
    delete/2,           %% delete all by match
    delete_one/2,       %% delete one by match
    count/2,            %% count by match
    count/3,            %% count by match, limit
    ensure_index/2      %% index collection by index spec - {key, name, unique, dropDups}
]).

-spec save(binary(), any()) -> {ok, any()} | {error, any()}.
save(Coll, Doc) ->
    gen_server:call(?MODULE, {save, Coll, Doc}).

-spec update(binary(), any()) -> {ok, any()} | {error, any()}.
update(Coll, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Doc}).

-spec update(binary(), any(), any()) -> {ok, any()} | {error, any()}.
update(Coll, Match, Doc) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Match, Doc}).

-spec update(binary(), any(), any(), list()) -> {ok, any()} | {error, any()}.
update(Coll, Match, Doc, Args) when is_map(Doc) ->
    gen_server:call(?MODULE, {update, Coll, Match, Doc, Args}).

-spec find_one(binary(), any()) -> {ok, any()} | {error, any()}.
find_one(Coll, Match) ->
    gen_server:call(?MODULE, {find_one, Coll, Match}).

% Example:
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 1}, {skip, 1}]).
% 
-spec find_one(binary(), any(), list()) -> {ok, any()} | {error, any()}.
find_one(Coll, Match, Args) ->
    gen_server:call(?MODULE, {find_one, Coll, Match, Args}).

-spec find(binary(), any()) -> {ok, any()}.
find(Coll, Match) ->
    gen_server:call(?MODULE, {find, Coll, Match, []}).

% Example
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 3}, {skip, 1}, 
%   {projector, {<<"created_at">>, 1, <<"grpid">>, 1}}]).
% 
% mongo_worker:find(<<"posts">>, {}, [{batchsize, 3}, {skip, 1}, 
%   {projector, #{<<"created_at">> => 1, <<"grpid">> => 1}}
% ]).

-spec find(binary(), any(), list()) -> {ok, any()}.
find(Coll, Match, Args) ->
    gen_server:call(?MODULE, {find, Coll, Match, Args}).

-spec match(binary(), any(), any()) -> {ok, any()}.
match(Coll, Match, Sort) ->
    gen_server:call(?MODULE, {match, Coll, Match, Sort}).

-spec match(binary(), any(), any(), byte()) -> {ok, any()}.
match(Coll, Match, Sort, Limit) ->
    gen_server:call(?MODULE, {match, Coll, Match, Sort, Limit}).

-spec match(binary(), any(), any(), byte(), byte()) -> {ok, any()}.
match(Coll, Match, Sort, Skip, Limit) ->
    gen_server:call(?MODULE, {match, Coll, Match, Sort, Skip, Limit}).

-spec match_group(Coll::binary(), Match::any(), Group::any()) -> {ok, any()}.
match_group(Coll, Match, Group) ->
    gen_server:call(?MODULE, {match_group, Coll, Match, Group}).

-spec match_group(Coll::binary(), Match::any(), Group::any(), Sort::any()) -> {ok, any()}.
match_group(Coll, Match, Group, Sort) ->
    gen_server:call(?MODULE, {match_group, Coll, Match, Group, Sort}).

-spec match_group(Coll::binary(), Match::any(), Project::any(), Group::any(), Sort::any()) -> {ok, any()}.
match_group(Coll, Match, Project, Group, Sort) ->
    gen_server:call(?MODULE, {match_group, Coll, Match, Project, Group, Sort}).

-spec delete(binary(), any()) -> {ok, any()} | {error, any()}.
delete(Coll, Match) ->
    gen_server:call(?MODULE, {delete, Coll, Match}).

-spec delete_one(binary(), any()) -> {ok, any()} | {error, any()}.
delete_one(Coll, Match) ->
    gen_server:call(?MODULE, {delete_one, Coll, Match}).

-spec count(binary(), any()) -> {ok, any()} | {error, any()}.
count(Coll, Match) ->
    gen_server:call(?MODULE, {count, Coll, Match, 0}).

-spec count(binary(), any(), byte()) -> {ok, any()} | {error, any()}.
count(Coll, Match, Limit) ->
    gen_server:call(?MODULE, {count, Coll, Match, Limit}).

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
                case catch mongo:insert(Conn, Coll, Doc) of
                    {'EXIT', Error} -> {error, Error};
                    Else -> {ok, Else}
                end
            end),
    {reply, Reply, State};

handle_call({update, Coll, Doc}, _From, #state{pool=Pool} = State) ->
    Id = maps:get(<<"_id">>, Doc),
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                case catch mongo:update(Conn, Coll, {<<"_id">>, Id}, {<<"$set">>, Doc}) of
                    {'EXIT', Error} -> {error, Error};
                    Else -> {ok, Else}
                end
            end),            
    {reply, Reply, State};

handle_call({update, Coll, Match, Doc}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                case catch mongo:update(Conn, Coll, Match, Doc) of
                    {'EXIT', Error} -> {error, Error};
                    Else -> {ok, Else}
                end
            end),            
    {reply, Reply, State};

handle_call({update, Coll, Match, Doc, Args}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool, 
            fun(Conn) ->
                case catch mongo:update(Conn, Coll, Match, Doc, Args) of
                    {'EXIT', Error} -> {error, Error};
                    Else -> {ok, Else}
                end
            end),            
    {reply, Reply, State};

handle_call({find_one, Coll, Match}, _From, #state{pool=Pool} = State) ->    
    Res = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:find_one(Conn, Coll, Match)
            end),
    Reply = case maps:size(Res) of
                0 -> {error, not_found};
                _ -> {ok, Res}
            end,
    {reply, Reply, State};

handle_call({find_one, Coll, Match, Args}, _From, #state{pool=Pool} = State) ->    
    Res = poolboy:transaction(Pool, 
            fun(Conn) ->
                mongo:find_one(Conn, Coll, Match, Args)
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
handle_call({find, Coll, Match, Args}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                Cursor = mongo:find(Conn, Coll, Match, Args),
                Results = mc_cursor:rest(Cursor),
                mc_cursor:close(Cursor),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Match, Sort}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$sort">>, Sort}
                    ]}),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match, Coll, Match, Sort, Limit}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$sort">>, Sort},
                        {<<"$limit">>, Limit}
                    ]}),
                Results
            end),

    {reply, {ok, Res}, State};

handle_call({match, Coll, Match, Sort, Skip, Limit}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$sort">>, Sort},
                        {<<"$skip">>, Skip},
                        {<<"$limit">>, Limit}
                    ]}),
                Results
            end),

    {reply, {ok, Res}, State};

handle_call({match_group, Coll, Match, Group}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$group">>, Group}
                    ]}),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match_group, Coll, Match, Group, Sort}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$group">>, Group},
                        {<<"$sort">>, Sort}
                    ]}),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({match_group, Coll, Match, Project, Group, Sort}, _From, #state{pool=Pool} = State) ->
    Res = poolboy:transaction(Pool,
            fun(Conn) ->
                {true, #{<<"result">> := Results}} = mongo:command(Conn,
                    {<<"aggregate">>, Coll, <<"pipeline">>, [
                        {<<"$match">>, Match},
                        {<<"$project">>, Project},
                        {<<"$group">>, Group},
                        {<<"$sort">>, Sort}
                    ]}),
                Results
            end),
    {reply, {ok, Res}, State};

handle_call({delete, Coll, Match}, _From, #state{pool=Pool} = State) ->    
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:delete(Conn, Coll, Match)
            end),
    {reply, {ok, Reply}, State};

handle_call({delete_one, Coll, Match}, _From, #state{pool=Pool} = State) ->    
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:delete_one(Conn, Coll, Match)
            end),
    {reply, {ok, Reply}, State};

handle_call({count, Coll, Match, Limit}, _From, #state{pool=Pool} = State) ->
    Reply = poolboy:transaction(Pool,
            fun(Conn) ->
                mongo:count(Conn, Coll, Match, Limit)
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


