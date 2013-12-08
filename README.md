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
  $ rebar create template=project
  $ rebar create template=simpleapp appid=foo
  ````

  Add tuah in the `rebar.config` file
  ```` bash
  $ cat rebar.config
  ...
  {deps, [
      {tuah, ".*", {git, "git://github.com/mhishami/tuah.git", "master"}}
  ]}.
  ...
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
                {redirect, <<"/auth/login">>};
            _ ->
                {ok, proceed}
        end.

  handle_request(<<"GET">>, <<"api">>, _, _, _) ->
        %% return data as json data
        %%  note: value cannot be an atom.
        %%
        {json, [{username, <<"hisham">>}, {password, <<"sa">>}]};
      
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

  We will be using a bootstrap sample page for this example. 
  ```` bash
  $ cd ~/Projects/Web
  $ git clone https://github.com/twitter/bootstrap.git
  ````
  
  Put all the static files in the `priv` directory, and prepend it with `static` name, i.e.
  ```` bash
  $ cd ~/Projects/Erlang/foo
  $ cp -r ~/Projects/Web/bootstrap/dist priv/assets
  $ cp -r ~/Projects/Web/bootstrap/docs-assets/ico priv/assets/.
  $ cp ~/Projects/Web/bootstrap/docs/examples/jumbotron-narrow/index.html templates/base.dtl
  $ cp ~/Projects/Web/bootstrap/docs/examples/jumbotron-narrow/jumbotron-narrow.css priv/assets/css/style.css
  ````

  Replace all references to css and js files
  Edit `base.html` header file to be:

  ``` html

    <!-- Bootstrap core CSS -->
    <link href="/static/assets/css/bootstrap.css" rel="stylesheet">

    <!-- Custom styles for this template -->
    <link href="/static/assets/css/style.css" rel="stylesheet">

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

  Edit the Makefile a bit

  ``` bash
  dev:
  	@erl +A 10 -sname foo \
      -pa ebin include deps/*/ebin deps/*/include ebin include \
  		-boot start_sasl -s reloader -s tuah -s foo
  ```

  And run it.    

  ``` shell
  $ make; make dev
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
  {redirect, "/page"}   %% redirect to page_controller -> page.dtl
  {ok, Data}            %% return the page for the current controller with Data
  {<<"login">>, Data}   %% return login.dtl view with Data
  {json, DataList}      %% return json data from erlang list
  ```
  
2. Customize your error view with custom error.dtl page that takes `{{ error }}` as the message

  
