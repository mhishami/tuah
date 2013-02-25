-module(session_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include ("tuah.hrl").
-include_lib("stdlib/include/qlc.hrl").

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
    C1 = lists:foldl(
            fun(C, Acu) ->
                {match, [{A, 16}]} = re:run(C, "_controller.beam"),
                [string:sub_string(C, 6, A)|Acu]
            end, [], filelib:wildcard("ebin/*_controller.beam")),            
    C2 = [ {list_to_binary(W), list_to_atom(W ++ "_controller")} || W <- C1 ],
    F = fun() ->
            lists:foreach(
                fun({X,Y}) ->
                    mnesia:write(#tuah_ctrls{key= X, val= Y})
                end, C2)
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
    
handle_call({sessions}, _From, State) ->
    F = fun() ->
            qlc:eval(qlc:q(
                [ S || S <- mnesia:table(tuah_session) ]
            ))
        end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
    
handle_call({prune, Day}, _From, State) ->
    Now = {date(), time()},
    F = fun() ->
            Q = qlc:q(
                [ S || S <- mnesia:table(tuah_session),
                       date_util:is_sooner_by(Now, S#tuah_session.timestamp, {days, Day}) ]
            ),
            Olds = qlc:e(Q),
            lists:foreach(
                fun(X) -> 
                    mnesia:delete({tuah_session, X#tuah_session.key})
                end, Olds)
        end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
                
handle_call({set, Key, Value, Expiry}, _From, State) ->
    % ?INFO("Setting: {~p, ~p, ~p}~n", [Key, Value, Expiry]),
    F = fun() -> mnesia:write(#tuah_session{key= Key, val= Value, expiry= Expiry, timestamp={date(), time()}}) end,
    Reply = mnesia:activity(transaction, F),
    {reply, Reply, State};
    
handle_call({get, Key}, _From, State) ->
    Now = date_util:epoch(),
    F = fun() ->
            case mnesia:read({tuah_session, Key}) of
                [#tuah_session{val=V, expiry=Exp}] when Exp =:= 0; Exp > Now ->
                    V;
                [#tuah_session{val=_V, expiry=Exp}] when Exp < Now ->
                    mnesia:delete({tuah_session, Key}),
                    undefined;
                [] ->
                    undefined
            end
        end,
    Reply = case catch mnesia:activity(transaction, F) of
                {'EXIT', _} -> undefined;
                Val -> Val
            end,
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

