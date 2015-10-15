Tuah
====

Tuah is a simple, HTTP framework, inspired by BeepBeep with Cowboy as the underlying mechanism.

Usage
-----

1. Create A New Project

  Create a new project using rebar template

  ```` bash
  $ mkdir ~/Projects/Erlang/foo
  $ cd ~/Projects/Erlang/foo
  $ wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
  ````

  Create a new file called Makefile, and add the content as per below
  ```` bash
  $ cat Makefile
  PROJECT = foo
  DEPS = tuah sync eunit_formatters

  tuah_dep = git http://github.com/mhishami/tuah master

  include erlang.mk
  ````

  Create a skeleton for the OTP project
  ```` bash
  $ make bootstrap bootstrap-rel
  ````

2. Create The App to Start/Stop

  ```` bash
  $ touch src/foo.erl
  ````

3. Add The Code

  The content might be similar to these:

  ````erlang
  %% file: foo.app.src
  {application, foo, [
    {description, "App Foo"},
    {vsn, "v0.1"},
    {modules, []},
    {registered, [foo_sup]},
    {applications, [
          kernel,
          stdlib,
          tuah, 
          cowboy,
          erlydtl,
          jsx,
          lager,
          cowlib,
          ranch,
          mongodb,
          bson
    ]},
  {mod, {foo_app, []}}
  ]}.
  ````

  ```` erlang

  -module(foo_app).
  -behaviour(application).

  -export([start/2]).
  -export([stop/1]).

  start(_Type, _Args) ->
      application:start(sync),
      application:ensure_all_started(lager),
      application:ensure_all_started(mongodb),    
      application:ensure_all_started(cowboy),
      application:start(erlydtl),

      %% set debug for console logs
      lager:set_loglevel(lager_console_backend, debug),

      foo_sup:start_link().

  stop(_State) ->
      ok.
    
  ````

4. Implement Your Controller

  ```` bash
  $ touch src/home_controller_.erl
  ````
  
  ```` erlang

  -module (home_controller).
  -export ([handle_request/5]).
  -export ([before_filter/2]).

  before_filter(SessionId) ->
      %% do some checking
      Sid = session_worker:get_cookies(SessionId),
      case Sid of
          {error, undefined} ->
              {redirect, <<"/auth/login">>};
          _ ->
              {ok, proceed}
      end.

  handle_request(<<"GET">>, <<"api">>, _, _, _) ->
      %% return data as json data
      %%  note: value cannot be an atom.
      %%
      {json, [{username, <<"hisham">>}, {password, <<"sa">>}]};
      
  handle_request(<<"GET">>, <<"/">>, _Args, _Params, _Req) ->    
      %% Action / will render home.dtl
      {render, []};
      
  handle_request(<<"GET">>, <<"login">> = Action, _Args, _Params, _Req) ->    
      %% Action login will render login.dtl
      {render, Action, []};

  handle_request(<<"POST">>, <<"login">> = Action, _, Params, _) ->
      {ok, PostVals} = maps:find(<<"qs_body">>, Params),
      Email = proplists:get_value(<<"email">>, PostVals, <<"">>),
      Password = proplists:get_value(<<"password">>, PostVals, <<"">>),
  
      %% authenticate the user
  
      %% set the session id, and user email
      Sid = web_util:hash_password(word_util:gen_pnr()),
      session_worker:set_cookies(Sid, Email),

      %% redirect
      {redirect, <<"/">>};
    
  handle_request(_, _, _, _, _) ->
      {error, <<"Opps, Forbidden">>}.

  ````

5. Do The Templates

  ```` bash
  $ mkdir templates
  $ cd templates
  $ touch home.dtl
  ````

  We will be using a bootstrap sample page for this example. 
  ```` bash
  $ cd ~/Projects/Web
  $ git clone https://github.com/twitter/bootstrap.git
  ````
  
  Put all the static files in the `priv` directory, and prepend it with `static` name, i.e.
  ```` bash
  $ cd ~/Projects/Erlang/foo
  $ mkdir -p priv/static/css
  $ cp -r ~/Projects/Web/bootstrap/dist priv/static/.
  $ cp -r ~/Projects/Web/bootstrap/assets priv/static/.
  $ cp ~/Projects/Web/bootstrap/docs/examples/jumbotron-narrow/index.html templates/base.dtl
  $ cp ~/Projects/Web/bootstrap/docs/examples/jumbotron-narrow/jumbotron-narrow.css priv/static/css/style.css
  ````

  Replace all references to css and js files
  Edit `base.dtl` header file to be:

  ``` html

    <!-- Bootstrap core CSS -->
    <link href="/static/dist/css/bootstrap.min.css" rel="stylesheet">
    <script src="/static/assets/dist/js/ie-emulation-modes-warning.js"></script>


    <!-- Custom styles for this template -->
    <link href="/static/css/style.css" rel="stylesheet">

  ```
  Edit the `base.dtl` file to include the `content` block to be:
  
  ``` html
  
      {% block content %}
      <div class="jumbotron">
        <h1>Jumbotron heading</h1>
        ...
    
        ...
           <h4>Subheading</h4>
          <p>Maecenas sed diam eget risus varius blandit sit amet non magna.</p>
        </div>
      </div>
      {% endblock %}
    
  ```
  
  Edit `home.dtl` to use the said template
  
  ```` bash
  $ cat home.dtl
  
  {% extends "base.dtl" %}
  
  ````
  
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

  
6. Run The App

  ``` shell
  $ make; make run
  ```
  
7. Deploy to Heroku

  Once everything is fine, we can then deploy it to Heroku

  Add git repo, and remove `ebin/*` from your `.gitignore` dir as heroku will not compile the dtl template we compiled earlier.
  
  ```` bash
  $ git init
  ````
  
  Edit `.gitignore`
  ```` bash
  $ cat .gitignore
  deps/*
  priv/db
  log/*
  
  $ git commit -am 'initial commit'
  ````
  
  
  ``` bash
  $ heroku create <<your app>> --stack cedar \
      --buildpack https://github.com/archaelus/heroku-buildpack-erlang.git    
  ```
  
  Create a Profile that contains the above running instructions. Ensure all are in one line.
  ``` bash
  $ cat Procfile
  web: erl -sname foo -pa ebin include deps/*/ebin deps/*/include ebin include __
    -boot start_sasl -s reloader -s tuah -s foo -noshell -noinput
  ```
  
  Specify Erlang version
  
  ``` bash
  $ echo OTP_R15B01 > .preferred_otp_version
  ```
  
  Add all to git, and do
  
  ``` bash
  $ git add .
  $ git commit -am 'commit to heroku'
  $ git push heroku master
  $ heroku ps:scale worker=1
  $ heroku open
  ```
  
  That's it!


Notes
=====

1. Reply can be done in several ways in the controller:
  ``` erlang
  {redirect, <<"/page">>}     %% redirect to page_controller -> page.dtl
  {render, Data}              %% return the page for the current controller with Data
  {render, <<"adm">>, Data}   %% render the page adm.dtl in the current controller with data
  {json, DataList}            %% return json data from erlang list
  ```
  
2. Customize your error view with custom error.dtl page that takes `{{ error }}` as the message

  
