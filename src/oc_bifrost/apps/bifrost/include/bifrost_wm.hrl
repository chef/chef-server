%% We'd love to include webmachine.hrl instead, but unfortunately it contains
%% a function declaration so breaks all of our behavior callbacks.
-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("mixer/include/mixer.hrl").
-include("bifrost.hrl").

%% Share resource state shared by all bifrost resource modules
%%
%% All ACL resources follow the pattern:
%%
%% /<type>/<id>/acl/<action>/<member_type>/<member_id>
%%
%% Group resources follow this pattern:
%%
%% /groups/<id>/<member_type>/<member_id>
%%
%% authz_id = <id>, request_type = <type>, otherwise <action>, <member_type> and
%% <member_id> are the same.
-record(base_state, {

          %% Who are we working with?  This is the object in question, i.e., for
          %% /<type>/<id> this is <id>
          authz_id :: auth_id() | undefined,

          %% What is the superuser ID?
          superuser_id :: auth_id() | undefined,

          %% What kind of resource are we talking about here?
          request_type :: auth_type() | undefined,

          %% What type of ACE request is this?
          action :: permission() | undefined,

          %% What member type are we checking for?  This will always be an actor or
          %% a group
          member_type :: auth_type() | undefined,

          %% What's the ID of the member
          member_id :: auth_id() | undefined,

          %% Who's asking?
          requestor_id :: requestor_id() | undefined,

          %% Which module is handling this request?
          module :: atom() | undefined,

          %% A unique request identifier, used for aggregating metrics
          reqid :: request_id() | undefined,

          %% Proplist of config info to pass to stats_hero
          metrics_config :: [{atom(), term()}] | undefined

         }).

-type base_state() :: #base_state{}.
-type wm_req() :: #wm_reqdata{}.
