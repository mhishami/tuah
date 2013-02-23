-module(session_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include ("tuah.hrl").

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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Nodes = [node()|nodes()],
    application:set_env(mnesia, dir, "priv/db"),
    mnesia:create_schema(Nodes),
    
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(tuah_session, [
        {attributes, record_info(fields, tuah_session)},
        {disc_copies, Nodes},
        {type, set}
    ]),
    mnesia:create_table(tuah_ctrls, [
        {attributes, record_info(fields, tuah_ctrls)},
        {ram_copies, Nodes},
        {type, set}
    ]),
    mnesia:wait_for_tables([tuah_session, tuah_ctrls], 1000),
    ?INFO("Session server is ready~n"),
    % tuah:reload(),
    {ok, Args}.
    
handle_call({reload}, _From, State) ->
    C1 = [ string:sub_string(C, 6) || C <- filelib:wildcard("ebin/*_controller.beam") ],
    C2 = [ string:tokens(X, ".") || X <- C1 ],
    C3 = [ H || [H|_] <- [ string:tokens(Z, "_") || Z <- [ Y || [Y, _] <- C2 ]]],
    C4 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C3 ],
    F = fun() ->
            lists:foreach(
                fun({X,Y}) ->
                    mnesia:write(#tuah_ctrls{key= X, val= Y})
                end, C4)
        end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
    
handle_call({locate, Key}, _From, State) ->
    F = fun() ->
            case mnesia:read({tuah_ctrls, Key}) of
                [] ->
                    undefined;
                [#tuah_ctrls{val=V}] ->
                    V
            end
        end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
                
handle_call({set, Key, Value, Expiry}, _From, State) ->
    F = fun() -> mnesia:write(#tuah_session{key= Key, val= Value, expiry= Expiry}) end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
    
handle_call({get, Key}, _From, State) ->
    F = fun() ->
            case mnesia:read({tuah_session, Key}) of
                [#tuah_session{val=V}] ->
                    V;
                [] ->
                    undefined
            end
        end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
    
handle_call({delete, Key}, _From, State) ->
    F = fun() -> mnesia:delete({tuah_session, Key}) end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

