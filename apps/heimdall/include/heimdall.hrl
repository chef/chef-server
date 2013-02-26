%% 32-character hex string, as a binary (well, as close to that as we
%% can get)
-type auth_id() :: <<_:256>>.
-type auth_type() :: actor | container | group | object.
