-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("mixer/include/mixer.hrl").

-type permission() :: create | read | update | delete | grant.
-type entity_type() :: actor | group | container | object.

%% Share resource state shared by all heimdall resource modules
-record(base_state, {

          %% Who are we working with?
          authz_id :: binary(),

          %% What kind of resource are we talking about here?
          request_type :: entity_type(),

          %% Who's asking?
          requestor_id :: binary(),

          %% Which module is handling this request?
          module :: atom()
         }).
