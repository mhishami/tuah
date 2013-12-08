-module(tuah_app).
-author ('Hisham Ismail <mhishami@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    uuid:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, 
                {dir, "priv", [{mimetypes, cow_mimetypes, all}]}},
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
    lager:log(info, self(), "tuah: Web server started at port ~p...~n", [Port]),

    case file:list_dir("priv/ssl") of
        {ok, _} ->
            SSLPort = case os:getenv("SSL_PORT") of
                          false -> 8443;
                          SSLVal -> list_to_integer(SSLVal)
                      end,
            {ok, _} = cowboy:start_https(https, 100, [
                          {port, SSLPort},
                          {cacertfile, "priv/ssl/cowboy-ca.crt"},
                          {certfile, "priv/ssl/server.crt"},
                          {keyfile, "priv/ssl/server.key"}], [
                          {env, [{dispatch, Dispatch}]}
                      ]),
            lager:log(info, self(), "tuah: Secure Web server started at port ~p...~n", [SSLPort]);
        {error, _} ->
            lager:log(info, self(), "tuah: SSL is not configured.")
    end,
    
    tuah_sup:start_link().

stop(_State) ->
    ok.
