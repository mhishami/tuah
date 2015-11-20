-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
        | {normal, timeout}
        | {error, closed}
        | {remote, closed}
        | {remote, cowboy_websocket:close_code(), binary()}
        | {error, badencoding}
        | {error, badframe}
        | {error, atom()}.

-spec init({atom(), http}, Req, opts())
        -> {ok, Req, state()}
        | {loop, Req, state()}
        | {loop, Req, state(), hibernate}
        | {loop, Req, state(), timeout()}
        | {loop, Req, state(), timeout(), hibernate}
        | {shutdown, Req, state()}
        | {upgrade, protocol, module()}
        | {upgrade, protocol, module(), Req, opts()}
        when Req::cowboy_req:req().        
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

-spec websocket_init(atom(), Req, opts())
        -> {ok, Req, state()}
        | {ok, Req, state(), hibernate}
        | {ok, Req, state(), timeout()}
        | {ok, Req, state(), timeout(), hibernate}
        | {shutdown, Req}
        when Req::cowboy_req:req().
websocket_init(TransportName, Req, _Opts) ->
    tuah_srv:ws_init(TransportName),
    {ok, Req, undefined_state}.

-spec websocket_handle({text | binary | ping | pong, binary()}, Req, State)
        -> {ok, Req, State}
        | {ok, Req, State, hibernate}
        | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
        | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
        | {shutdown, Req, State}
        when Req::cowboy_req:req(), State::state().
websocket_handle(Data, Req, State) ->
    tuah_srv:ws_data(Data),
    {ok, Req, State}.

-spec websocket_info(any(), Req, State)
        -> {ok, Req, State}
        | {ok, Req, State, hibernate}
        | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
        | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
        | {shutdown, Req, State}
        when Req::cowboy_req:req(), State::state().
websocket_info(Info, Req, State) ->
    tuah_srv:ws_info(Info),
    {ok, Req, State}.

-spec websocket_terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.
websocket_terminate(Reason, _Req, _State) ->
    tuah_srv:ws_terminate(Reason),
    ok.
