%% For the moment we're using string despite our informal standard of using binary
%% for this sort of thing in other projects (mainly so our JSON encoding/decoding
%% works smoothly).  We're doing this because the current version of the API barely
%% interacts with JSON and spends a lot of time interacting with Web Machine which
%% uses lists instead (we get authz_ids both from headers and the URL structure
%% itself in multiple ways).  So, for the time being, we're going with lists.  This
%% will VERY LIKELY change in V2 (presuming we decide to take a more JSON-oriented
%% approach to the API).
-type auth_id() :: string().
%% If undefined looks a bit strange here, it's because we explicitly pass it to
%% bifrost_db:create/3:
-type requestor_id() :: auth_id() | superuser | undefined.
-type auth_type() :: actor | container | group | object.
-type permission() :: create | read | update | delete | grant.
-type request_id() :: <<_:192>>. %% 24-byte binary; a base64-encoded MD5 digest of an Erlang reference

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L,D), proplists:get_value(X, L, D)).
