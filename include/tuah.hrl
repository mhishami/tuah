

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(DEBUG(Text, Args), lager:log(debug, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).
-define(INFO(Text, Args), lager:log(info, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).
-define(ERROR(Text, Args), lager:log(error, ?MODULE, "~p:~p: " ++ Text, [?MODULE, ?LINE | Args])).

-define(SALT, <<"02f40807f3abd51aefb1f77d9d1535cc4e6a12e0">>).
-define(SIZE, 5).

%% record to keep the key, value
-record(tuah_session, {key, val, expiry, timestamp = {date(), time()}}).
-record(tuah_cookies, {key, val}).
