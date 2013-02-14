Tuah
====

Tuah is a simple, HTTP framework, inspired by BeepBeep with Cowboy as the underlying mechanism.

Usage
-----

1. Create A New Project

  Create a new project using rebar template

  ``` shell
  $ mkdir foo
  $ cd foo
  $ rebar create template=project
  $ rebar create template=simpleapp appid=foo

  ```

2. Create The App to Start/Stop

  ``` shell
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
      ok = ensure_started(tuah),
      ok = ensure_started(foo).
    
  stop() ->
      application:stop(foo),
      application:stop(tuah).
    
  ```

4. Implement Your Controller

  ``` shell
  $ touch src/home_controller_.erl

  ```
  ``` erlang

  -module (home_controller, [Req]).
  -export ([handle_request/4]).

  handle_request(<<"GET">>, Action, Args, Params) ->    
      {ok, []};
    
  handle_request(<<"POST">>, <<"login">>, _, [{qs, _}, {body, Vals}] = Params) ->
      Username = proplists:get_value(<<"email">>, Vals),
      Password = proplists:get_value(<<"password">>, Vals),
    
      %% authenticate the user
    
      %% redirect
      {redirect, "/"};
    
  handle_request(_, _, _, _) ->
      {error, "Opps, Forbidden"}.

  ```

5. Do The Templates

  ``` shell
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

  ``` shell
  dev:
  	@erl +A 10 -sname foo -pa ebin include deps/*/ebin deps/*/include ebin include \
  		-boot start_sasl \
  		-s tuah -s foo

  ```

  And run it.    

  ``` shell
  $ make; make dev

  ```


  * That's it. Enjoy!