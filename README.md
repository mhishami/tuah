![logo](https://raw.githubusercontent.com/mhishami/tuah/master/example/foo/priv/static/img/tuah-small.png)

> Tuah
> ====
> Tuah is a simple, HTTP framework, inspired by BeepBeep with Cowboy as the underlying mechanism.
> 
> Easy to extend, comes with session management and continous code compilations to make it a very productive framework to begin with.
> 
> Built on the strength of Cowboy and MongoDB, development time is greatly reduced.

Quick Start
-----------

1. Create A New Project

  Creating a new project is made simpler via the addition of `new_app.sh` script.

  ``` shell
  hisham@skrall:tuah$ ./new_app.sh foo
  Creating new app: foo
  Creating directory...
  Creating template files...
  Creating bootstrap files...
  Finishing up...
  Done. Your app is created in ../foo.
  +----------------------+
  |     Happy coding!    |
  +----------------------+
  hisham@skrall:tuah$ 

  ```
  
2. The project structure looks like

  ``` shell
  foo
  ├── Makefile
  ├── erlang.mk
  ├── include
  │   └── foo.hrl
  ├── priv
  │   └── static
  │       ├── assets/
  │       ├── css/
  │       ├── dist/  
  │       └── img
  │           ├── tuah-small.png
  │           └── tuah.png
  ├── rel
  │   ├── sys.config
  │   └── vm.args
  ├── relx
  ├── relx.config
  ├── run.sh
  ├── src
  │   ├── auth_controller.erl
  │   ├── baz.app.src
  │   ├── baz_app.erl
  │   ├── baz_sup.erl
  │   ├── home_controller.erl
  │   └── secret_controller.erl
  └── templates
      ├── error.dtl
      ├── home.dtl
      ├── login.dtl
      ├── public.dtl
      └── register.dtl

  ```
3. Start the mongodb server, as the sample application does live registration and all.

4. Run The App
> The App
> ====
> 
> This simple app does user registration, login and logout.
> Extend this further to your likings.

  ``` shell
  $ make
  $ ./run.sh console

  ```
  View the app at http://localhost:8080  
  That's it!

5. Extend the app the way you like it by adding more templates, controllers to make a full blown app.

6. Feel free to fork this. Cheers!

Mongo Backend
-------------
1. Tuah framework comes with mongo client integration. Almost *all* the APIs are supported. Feel free to browse the source code.

2. Advanced examples such as regex search, complex find and match are supported. Details query and projection operators can be found at [https://docs.mongodb.org/manual/reference/operator/](MongoDB Reference)

3. Different notations for Selector/Projector, use which one that you like. I preferred the second notation as it is easier to read and comprehend.

  ``` erlang
  mongo_worker:find(<<"posts">>, {<<"tag">>, <<"general">>, 
                                  <<"cat">>, <<"News">>}, 
    [{batchsize, 10}, {skip, 20}, 
    {projector, {<<"created_at">>, 1, <<"grpid">>, 1}}]).
  ```
  ``` erlang
  mongo_worker:find(<<"posts">>, #{<<"tag">> => <<"general">>, 
                                   <<"cat">> => <<"News">>}, 
    [{batchsize, 10}, {skip, 20}, 
    {projector, #{<<"created_at">> => 1, <<"grpid">> => 1}}]).
  ```

4. Regular expressions are also there.

  ``` erlang
  mongo_worker:find(<<"posts">>, 
    {<<"title">>, #{<<"$regex">>  => <<"some*">>, 
                    <<"$options">> => <<"i">>}}, 
    [{projector, #{<<"grpid">> => 1, 
                   <<"title">> => 1, 
                   <<"author.fullname">> => 1}}]).
  ```

Routing
-------
1. Routing is made simple in tuah, where the URL is broken up into multiple items, to be handled by the request handlers.

2. Below is the breakdown of the request URL, and the components it is broken up into:
 
  - `/post/message/20`
    - **Controller**: `post_controller.erl`
    - **Action**: `<<"message">>`
    - **Args**: `[ 20 ]`

  - `/user/view/details/100`
    - **Controller**: `user_controller.erl`
    - **Action**: `<<"view">>`
    - **Args**: `[<<"details">>, 100]`

  - `/view/message/20?float=false&data=none`
    - **Controller**: `view_controller.erl`
    - **Action**: `<<"message">>`
    - **Args**: `[ 20 ]`
    - **Params**: `#{qs_vals => [{<<"float">>, <<"false">>},
     {<<"data">>, <<"none">>}],...}`


Controllers
-----------
1. All controllers can be defined by using the `tuah_controller` behavior.

2. All handlers are in the form:

  ``` erlang
  handle_request(Method, Action, Args, Params, Req)
  ```

3. Request handler parameters:
  - `Method` - can be HTTP Method, capitalized (e.g. GET, POST, PUT, DELETE etc.)

  - `Action` - the second parameters in the URL chosen, e.g.
    - `/user/delete` : Action = `delete`
    - `/post/view` : Action = `view`

  - `Args` - the list of arguments of the URL
    - `/user/delete/20/` : Args = `[20]`
    - `/post/view/977/simple` : Args = `[ 997, <<"simple">>]`

  - `Params` - the request parameters
    - `qs_vals` : contains the `GET` query string variables
    - `qs_body` : contains the `POST` query string variables
    - `files` : contains the file uploads data
    - `auth` : contains the authentication context
    - `sid` : contains the session id

  - `Req` - the Cowboy Req parameters. It is hardly used, but just in case you want to play around with the Cowboy internals.
  

Session Context
--------------
1. Session context is carried in the `Params` in each request:
  - `sid` - The session id 
  - `auth` - The authentication context

2. Authentication context is to be used sparingly. **DO NOT** carry sensitive information in the `auth` context. Only username/email address is suffice.

Authentication
--------------
1. Authentication is done using the session context `auth` and `sid` in the `Params` of request handler.

2. Typical authentication procedures are:

  ``` erlang
  handle_request(<<"POST">>, <<"login">> = Action, _Args, Params, _Req) ->    
      PostVals = maps:get(<<"qs_body">>, Params),
      Username = proplists:get_value(<<"username">>, PostVals),
      Password = proplists:get_value(<<"password">>, PostVals),
  
      case Username =:= <<>> orelse Password =:= <<>> of
          true ->
              {render, Action, [{error, <<"All fields are required.">>}]};
          _ ->
              case mongo_worker:find(?DB_USERS, #{<<"username">> => Username}) of
                  {ok, []} ->
                      {render, Action, [{error, <<"Invalid username, or password">>}]};
                  {ok, [User]} ->
                      ?DEBUG("User= ~p~n", [User]),
                      HashPass = web_util:hash_password(Password),
                      Pass = maps:get(<<"password">>, User),
                      case Pass =/= HashPass of
                          true ->
                              {render, Action, [{error, <<"Invalid username, or password">>}]};
                          _ ->
                              Sid = maps:get(<<"sid">>, Params),
                              session_worker:set_cookies(Sid, Username),
  
                              %% redirect, assuming "secret" is defined.
                              {redirect, <<"/secret">>, {cookie, <<"auth">>, Username}}
                      end
              end
      end;
  ```

3. Logging out can be done by resetting the data in the session

  ``` erlang
  handle_request(<<"GET">>, <<"logout">>, _Args, Params, _Req) ->
        session_worker:del_cookies(maps:get(<<"sid">>, Params)),
        {redirect, <<"/">>};
  ```

Templates
---------
1. Templates are defined using `erlydtl`, which is a fork of Django Templates.

2. In each project, you should have a custom `error` template so that all errors can be shown nicely, and react accordingly.

