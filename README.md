Tuah
====

Tuah is a simple, HTTP framework, inspired by BeepBeep with Cowboy as the underlying mechanism.

Usage
-----

1. Create A New Project

  Create a new project using rebar template

  ``` bash
  $ mkdir foo
  $ cd foo
  $ rebar create template=project
  $ rebar create template=simpleapp appid=foo
  ```

2. Create The App to Start/Stop

  ``` bash
  $ touch src/foo.erl
  ```

3. Add The Code

  The content might be similar to this

  ``` erlang

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
      ok = ensure_started(tuah),
      ok = ensure_started(foo).
    
  stop() ->
      application:stop(foo),
      application:stop(tuah),
      application:stop(ranch),
      application:stop(crypto).
    
  ```

4. Implement Your Controller

  ``` bash
  $ touch src/home_controller_.erl
  ```
  
  ``` erlang

  -module (home_controller, [Req]).
  -export ([handle_request/4]).
  -export ([before_filter/0]).

  before_filter() ->
      %% do some checking
      
      %% {redirect, <<"/login">>}
      {ok, proceed}.
  
  handle_request(<<"GET">>, Action, Args, Params) ->    
      {ok, []};
    
  handle_request(<<"POST">>, <<"login">>, _, [{qs_vals, _}, {body_qs_, Vals}] = Params) ->
      Username = proplists:get_value(<<"email">>, Vals),
      Password = proplists:get_value(<<"password">>, Vals),
    
      %% authenticate the user
    
      %% redirect
      {redirect, "/"};
    
  handle_request(_, _, _, _) ->
      {error, "Opps, Forbidden"}.

  ```

5. Do The Templates

  ``` bash
  $ mkdir templates
  $ cd templates
  $ touch home.dtl
  ```

  And complete the home.dtl using ErlyDTL specs.
  Put all the static files in the priv directory, and prepend it with `static` name, i.e.

  ``` html
  <link href="/static/assets/css/bootstrap.css" rel="stylesheet">
  ```

6. Run The App

  Edit the Makefile a bit

  ``` bash
  dev:
  	@erl +A 10 -sname foo -pa ebin include deps/*/ebin deps/*/include ebin include \
  		-boot start_sasl \
  		-s tuah -s foo
  ```

  And run it.    

  ``` shell
  $ make; make dev
  ```


Notes
=====

1. Reply can be done in several ways in the controller:
  ``` erlang
  {redirect, "/page"}   %% redirect to page_controller -> page.dtl
  {ok, Data}            %% return the page for the current controller with Data
  {<<"login">>, Data}   %% return login.dtl view with Data
  ```
  
2. Customize your error view with custom error.dtl page that takes `{{ error }}` as the message

  
