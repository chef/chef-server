-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("mixer/include/mixer.hrl").

-type permission() :: create | read | update | delete | grant.
-type entity_type() :: actor | group | container | object.

%% Share resource state shared by all heimdall resource modules
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
          authz_id :: binary(),

          %% What kind of resource are we talking about here?
          request_type :: entity_type(),

          %% What is the ACL on the resource?
          acl :: term(),

          %% What type of ACE request is this?
          action :: permission(),

          %% What member type are we checking for?  This will always be an actor or
          %% a group
          member_type :: entity_type(),

          %% What's the ID of the member
          member_id :: binary(),

          %% Who's asking?
          requestor_id :: binary(),

          %% Which module is handling this request?
          module :: atom()
         }).
