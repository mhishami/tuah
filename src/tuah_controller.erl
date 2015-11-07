-module (tuah_controller).
-author ('Hisham Ismail <mhishami@gmail.com').

-callback before_filter(SessionId)
	-> {ok, proceed}					%% proceed with the calling page
	|  {redirect, Path}					%% redirect to authentication, for example
	when SessionId::binary(),
		 Path::binary().

-callback handle_request(Method, Action, Args, Params, Req) 
	-> {render, Template}				%% render page template in the name of controller
	|  {render, Template, Data}			%% render page template, and data
	|  {redirect, Location}				%% redirect to URL
	|  {redirect, Location, Cookies}	%% refirect with cookies added
	|  {json, Data}						%% reply with JSON data
	|  {error, Message}					%% show error message
	when Method::binary(),
	     Action::binary(),
	     Args::list(),
	     Params::map(),
	     Req::cowboy_req:req(),
	     Template::binary(),
	     Data::binary(),
	     Location::list(),
	     Cookies::list(),
	     Message::binary().

