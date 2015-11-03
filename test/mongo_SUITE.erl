-module (mongo_SUITE).
-include_lib("common_test/include/ct.hrl").
-include ("tuah.hrl").

-export ([init_per_suite/1, end_per_suite/1]).
-export ([all/0, init_per_testcase/2, end_per_testcase/2]).
-export ([db_tests/1]).
 
-define (POOL, test_pool).

all() -> [db_tests].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(poolboy),
    application:start(bson),
    application:ensure_all_started(mongodb),
    mongo_sup:start_link(),
    Config.

end_per_suite(_Config) ->
    application:stop(mongodb),
    application:stop(bson),
    application:stop(poolboy),
    application:stop(crypto),

    mongo_sup:stop_link(?POOL),
    ok.

init_per_testcase(db_tests, Config) ->
    Config.

end_per_testcase(db_tests, Config) ->    
    Config.

db_tests(_Config) ->
    %% test insert
    do(?POOL, 
        fun(Conn) ->
            mongo:insert(Conn, <<"test_user">>, [
                #{'_id' => 1, name => <<"Hisham">>, email => <<"mhishami@gmail.com">>},
                #{'_id' => 2, name => <<"Haslinda">>, email => <<"haslinda@gmail.com">>}
                ])
        end),
    ok.

do(PoolName, Fun) ->
    poolboy:transaction(PoolName, Fun).

