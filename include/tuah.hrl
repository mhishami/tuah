
-define (INFO(Name), error_logger:info_msg(Name)).
-define (INFO(Name, Args), error_logger:info_msg(Name, Args)).

%% record to keep the key, value
-record(tuah_session, {key, val, expiry}).
-record(tuah_ctrls, {key, val}).
