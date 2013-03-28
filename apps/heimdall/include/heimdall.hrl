%% 32-character hex string, as a binary (well, as close to that as we
%% can get)
-type auth_id() :: string().
-type auth_type() :: actor | container | group | object.
-type permission() :: create | read | update | delete | grant.
-type request_id() :: <<_:192>>. %% 24-byte binary; a base64-encoded MD5 digest of an Erlang reference

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L,D), proplists:get_value(X, L, D)).
