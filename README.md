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
  │       └── dist/  
  ├── rel
  │   ├── sys.config
  │   └── vm.args
  ├── relx.config
  ├── run.sh
  ├── src
  │   ├── foo.app.src
  │   ├── foo_app.erl
  │   ├── foo_sup.erl
  │   └── home_controller.erl
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

  ```
  mongo_worker:find(<<"posts">>, {<<"tag">>, <<"general">>, 
                                  <<"cat">>, <<"News">>}, 
    [{batchsize, 10}, {skip, 20}, 
    {projector, {<<"created_at">>, 1, <<"grpid">>, 1}}]).
   
  mongo_worker:find(<<"posts">>, #{<<"tag">> => <<"general">>, 
                                   <<"cat">> => <<"News">>}, 
    [{batchsize, 10}, {skip, 20}, 
    {projector, #{<<"created_at">> => 1, <<"grpid">> => 1}}]).
  ```

4. Regular expressions are also there.

  ```
  mongo_worker:find(<<"posts">>, 
    {<<"title">>, #{<<"$regex">>  => <<"some*">>, 
                    <<"$options">> => <<"i">>}}, 
    [{projector, #{<<"grpid">> => 1, 
                   <<"title">> => 1, 
                   <<"author.fullname">> => 1}}]).
  ```
