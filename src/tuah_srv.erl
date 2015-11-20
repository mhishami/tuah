-module(tuah_srv).
-author ('Hisham Ismail <mhishami@gmail.com').

-behaviour(gen_server).
-include ("tuah.hrl").
-define(SERVER, ?MODULE).

-record (state, {sock}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export ([ws_init/1, ws_data/1, ws_info/1, ws_terminate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> any().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ws_init(Init::any()) -> any().
ws_init(Init) ->
    gen_server:info(?MODULE, {ws_init, Init}).

-spec ws_data(Data::any()) -> any().
ws_data(Data) ->
    gen_server:call(?MODULE, {ws_data, Data}).

-spec ws_info(Info::any()) -> any().
ws_info(Info) ->
    gen_server:info(?MODULE, {ws_info, Info}).

-spec ws_terminate(Reason::any()) -> any().
ws_terminate(Reason) ->
    gen_server:info(?MODULE, {ws_term, Reason}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init(list()) -> {ok, any()}.
init(_Args) ->
    process_flag(trap_exit, true),
    uuid:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, static_world, "",
                [{mimetypes, cow_mimetypes, all}]}},
            {"/ws", ws_handler, []},                
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
    ?INFO("Web server started at port ~p...~n", [Port]),

    %% init ezmq socket
    {ok, Socket} = ezmq:start([{type, rep}]),
    ezmq:bind(Socket, tcp, 5555, []),

    ?INFO("ØMQ Socket: ~p", [Socket]),
    {ok, #state{sock=Socket}}.

-spec handle_call(any(), any(), any()) -> {ok, any()} | {error, any()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), any()) -> {noreply, any()}.
handle_cast({ws_data, Data}, #state{sock=Socket} = State) ->
    ?INFO("ØMQ Socket: ~p", [Socket]),
    ezmq:recv(Socket),
    ezmq:send(Socket, Data),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), any()) -> {noreply, any()}.
handle_info({ws_init, Init}, State) ->
    ?INFO("ØMQ initializing. Init: ~p", [Init]),
    {noreply, State};

handle_info({ws_info, Info}, #state{sock=Socket} = State) ->
    ezmq:send(Socket, Info),
    {noreply, State};

handle_info({ws_term, Reason}, State) ->
    ?ERROR("ØMQ terminating. Reason: ~p", [Reason]),
    {noreply, State};

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

