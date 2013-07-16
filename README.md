Tuah
====

Tuah is a simple, HTTP framework, inspired by BeepBeep with Cowboy as the underlying mechanism.

Usage
-----

1. Create A New Project

  Create a new project using rebar template

  ```` bash
  $ mkdir foo
  $ cd foo
  $ rebar create template=project
  $ rebar create template=simpleapp appid=foo
  ````

2. Create The App to Start/Stop

  ```` bash
  $ touch src/foo.erl
  ````

3. Add The Code

  The content might be similar to this

  ```` erlang

  -module (foo).

  -export ([start/0]).
  -export ([stop/0]).

  ensure_started(App) ->
        case application:start(App) of
            ok ->
                ok;
            {error, {already_started, App}} ->
                ok
        end.
    
  start() ->
        ok = ensure_started(crypto),
        ok = ensure_started(ranch),
        ok = ensure_started(cowboy),
        ok = ensure_started(tuah),
        ok = ensure_started(foo).
    
  stop() ->
        application:stop(foo),
        application:stop(tuah),
        application:stop(cowboy),
        application:stop(ranch),
        application:stop(crypto).
    
  ````

4. Implement Your Controller

  ```` bash
  $ touch src/home_controller_.erl
  ````
  
  ```` erlang

  -module (home_controller).
  -export ([handle_request/5]).
  -export ([before_filter/2]).

  before_filter(Params, _Req) ->
        %% do some checking
        User = proplists:get_value(auth, Params, undefined),
        case User of
            undefined ->
                {redirect, <<"/auth/login">>}
            _ ->
                {ok, proceed}
        end.

  handle_request(<<"GET">>, <<"api">>, _, _, _) ->
        %% return data as json data
        %%  note: value cannot be an atom.
        %%
        {json, [{username, <<"hisham">>}, {password, <<"sa">>}]};
      
  handle_request(<<"GET">>, Action, _Args, _Params, _Req) ->
        %% /home/foo -> will render foo.dtl since Action == foo
        %% /home/bar -> will render bar.dtl since Action == bar
        %% /home/foo/bar -> will render foo.dtl too. <<"bar">> appears in Args
        {Action, []}
    
  handle_request(<<"GET">>, _Action, _Args, _Params, _Req) ->    
        %% / will render home.dtl
        {ok, []};
      
  handle_request(<<"POST">>, <<"login">>, _, [{auth, _}, {sid, Sid}, {qs_vals, _}, {qs_body, Vals}], _Req) ->
        Username = proplists:get_value(<<"email">>, Vals),
        Password = proplists:get_value(<<"password">>, Vals),
  
        %% authenticate the user
    
        %% set the session
        tuah:set(Sid, <<"foo@bar.com">>),
  
        %% redirect
        {redirect, "/"};
    
  handle_request(_, _, _, _, _) ->
        {error, <<"Opps, Forbidden">>}.

  ````

5. Do The Templates

  ```` bash
  $ mkdir templates
  $ cd templates
  $ touch home.dtl
  ````

  And complete the home.dtl using ErlyDTL specs.
  Put all the static files in the `priv` directory, and prepend it with `static` name, i.e.
  
  ``` bash
  +-- Project
    +-- deps
    +-- ebin
    +-- include
    +-- priv
      +-- assets
        +-- css
          - bootstrap.css
          - ...
        +-- js
        +-- img      
  ```

  The corresponding `href` will be like:
  ``` html
  <link href="/static/assets/css/bootstrap.css" rel="stylesheet">
  ```

6. Run The App

  Edit the Makefile a bit

  ``` bash
  dev:
  	@erl +A 10 -sname foo -pa ebin include deps/*/ebin deps/*/include ebin include \
  		-boot start_sasl -s reloader \
  		-s tuah -s foo
  ```

  And run it.    

  ``` shell
  $ make; make dev
  ```
  
7. Deploy to Heroku

  Once everything is fine, we can then deploy it to Heroku
  
  ``` bash
  $ heroku create <<your app>> --stack cedar \
    --buildpack https://github.com/archaelus/heroku-buildpack-erlang.git    
  ```
  
  Create a Profile that contains the above running instructions
  ``` bash
  $ cat Procfile
  web: erl -sname foo -pa ebin include deps/*/ebin deps/*/include ebin include \
  		-boot start_sasl -s reloader \
  		-s tuah -s foo
  ```
  
  Specify Erlang version
  
  ``` bash
  $ cat .preferred_otp_version

  echo OTP_R15B01 > .preferred_otp_version
  ```
  
  Add all to git, and do
  
  ``` bash
  $ git push heroku master
  $ heroku ps:scale worker=1
  ```
  
  That's it!


Notes
=====

1. Reply can be done in several ways in the controller:
  ``` erlang
  {redirect, "/page"}   %% redirect to page_controller -> page.dtl
  {ok, Data}            %% return the page for the current controller with Data
  {<<"login">>, Data}   %% return login.dtl view with Data
  {json, DataList}      %% return json data from erlang list
  ```
  
2. Customize your error view with custom error.dtl page that takes `{{ error }}` as the message

  
