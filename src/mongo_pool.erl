-module(mongo_pool).
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

-record(state, {pool}).

%% API.
-export([get/1]).
-export([peek/0]).
-export([clear_all/0]).

-spec get(binary()) -> pid().
get(Db) ->
    gen_server:call(?MODULE, {get, Db}).

-spec peek() -> {ok, any()}.
peek() ->
    gen_server:call(?MODULE, {peek}).

-spec clear_all() -> {ok, any()}.
clear_all() ->
    gen_server:call(?MODULE, {clear_all}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.
-spec init(list()) -> {ok, any()}.
init([]) ->
    random:seed(erlang:timestamp()),    
    {ok, #state{pool = maps:new()}}.

-spec handle_call(any(), any(), any()) -> {ok, any()}.
handle_call({peek}, _From, #state{pool=Pool} = State) ->
    {reply, {ok, Pool}, State};

handle_call({clear_all}, _From, #state{pool=Pool}) ->
    {reply, {ok, Pool}, #state{pool = maps:new()}};

handle_call({get, Db}, _From, #state{pool=Pool}) ->
    % ?DEBUG("ENTER: Db= ~p, Pool= ~p~n", [Db, Pool]),

    {Conn, NewPool} =  
        case catch maps:get(Db, Pool) of
            {'EXIT', {{badkey, Db}, Ex}} -> 
                ?DEBUG("Exception=~p~n", [Ex]),
                make_one(Pool, Db);

            ConnsArray ->
                Index = random:uniform(?SIZE) -1,
                % ?DEBUG("Index= ~p, Array= ~p~n", [Index, ConnsArray]),

                case catch array:get(Index, ConnsArray) of
                    undefined ->
                        make_one(Pool, Db, Index);
                    C ->
                        ?DEBUG("Returning existing conn...~n", []),
                        {C, Pool}
                end
        end,
    % ?DEBUG("EXIT: NewPool= ~p~n", [NewPool]),
    {reply, {ok, Conn}, #state{pool=NewPool}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(list(), any()) -> {noreply, any()}.
handle_info(Info, State) ->
    ?DEBUG("****************************************************************~n", []),
    ?DEBUG("Info=~p~n", [Info]),
    ?DEBUG("****************************************************************~n", []),
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------------
make_one(Pool, Db) ->
    ?DEBUG("New array created...~n", []),
    Array = array:new(?SIZE, [{fixed, true}]),
    make_one(Pool#{Db => Array}, Db, 0).

make_one(Pool, Db, Index) ->
    {ok, Conn} = mongo:connect([{database, Db}]),
    erlang:monitor(process, Conn),
    Array = maps:get(Db, Pool), 
    {Conn, Pool#{Db := array:set(Index, Conn, Array)}}.

