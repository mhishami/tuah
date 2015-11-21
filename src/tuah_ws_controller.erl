-module (tuah_ws_controller).
-author ('Hisham Ismail <mhishami@gmail.com').

-callback version() 
	-> integer().	%% return the version no.

-callback handle_ws(Data, Req) 
	-> {Data} 
	|  {error, Reason}
	when Data::{text | ping | pong | binary, binary()}, 
		 Req::cowboy_req:req(),
		 Reason::binary().

