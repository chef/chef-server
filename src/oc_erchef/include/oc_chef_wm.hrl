%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Stephen Delano <stephen@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @author many more...
%%
%% Copyright 2011-2015 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-include_lib("webmachine/include/wm_reqdata.hrl").
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("ej/include/ej.hrl").
-include("oc_chef_types.hrl").

-include_lib("stats_hero/include/stats_hero.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.
%% Since CS12, we have a default-org mode. Under this mode,
%% certain endpoints can be routed from
%% /ENDPOINT -> /organizations/DEFAULT_ORG/ENDPOINT
%% This is intended for backwards compatibility with
%% Open Source Chef 11 and earlier, and is not intended to
%% be carried forward. These are used to match on the strings
%% defined in dispatch.conf
%% NOTE: /users is specifically excluded from this list
-define(OSC11_COMPAT_RESOURCES,
    [
        "cookbooks",
        "clients",
        "data",
        "environments",
        "nodes",
        "principals",
        "roles",
        "sandboxes",
        "search",
        "runs",
        "groups",    %% Not technically OSC, but it was decided to route these too
        "containers",
        "organizations"
    ]
).

-define(BAD_DATE_MESSAGE(FieldName), erlang:iolist_to_binary([<<"Field ">>,FieldName,<<" is invalid. ">>,<<"All dates must be a valid date in ISO8601 form of exactly YYYY-MM-DDThh:mm:ss, eg 2099-02-28T01:00:00, or the string \"infinity\". All times are assumed UTC, so do not include a Z on the end of your date.">>])).

-type permission() :: create | delete | read | update.

-type container_name() :: cookbook
                        | data
                        | environment
                        | node
                        | role
                        | client
                        | container
                        | sandbox
                        | policies
                        | user
                        | organization
                        | group
                        | cookbook_artifact
                        | policy_group.

-type auth_tuple() ::
        {create_in_container, container_name()}
      | {object, object_id(), permission()}
      | {container, container_name(), permission()}.

-type wm_req() :: #wm_reqdata{}.


-record(key_context, {
          type,
          object_ej,
          object_name,
          object_authz_id,
          key_ej,  % ejson to inject into body response

          % deprecated support for api v0 in client/user creation:
          private_key
         }).
%% Shared resource state shared by all chef_wm resource modules.
-record(base_state, {
          %% Concrete resource impl
          resource_mod :: atom(),
          %% unique request ID from nginx header (or generated if not
          %% found) set by chef_wm_util:read_req_id.
          reqid :: binary(),

          %% The name of the HTTP request header containing the unique ID set by the load
          %% balancer
          reqid_header_name :: string(),

          %% String containing chef server product version.
          %% TODO this is also set as part of x-ops-api-info, which could be confusing.
          %% Let's make this reporting of version a versioned change itself:
          api_version :: string(),

          %% This represents the version which which the server API is expected
          %% to comply, as agreed upon between client and server.
          %% We will not allow it to be
          %% undefined by default, because in error conditions where it does not get set we will
          %% still attempt to capture it to log in finish_request.
          server_api_version  = -1 :: integer() | 'bad_value_requested',

          %% OTP information for the Erchef server in {ReleaseName, OtpVersion} form.
          otp_info :: {string(), string()},

          %% Indicates what variant of Chef Server this is (e.g. "osc" => Open Source Chef,
          %% "opc" = Opscode Private Chef, etc).
          server_flavor :: string(),

          %% A fun/1 that closes over the request headers and returns
          %% header values as binaries or 'undefined'.
          %% chef_rest_wm:init sets this.
          header_fun = undefined :: fun((binary()|string()) -> binary() | 'undefined')
                                  | 'undefined',

          %% Message added to erchef log messages (not user visible).
          %% Used to pass extra info usually in non-200 cases to the
          %% shared request logging code.
          %%
          %% Formatted using ~p, but expected to be reasonably small.
          %% If log_msg is remains undefined during finish_request,
          %% then it won't be added into the log annotations.
          log_msg = undefined :: term(),

          %% Time drift in seconds allowed between the timestamp in a
          %% singed request and the clock on the server.  Set in
          %% app.config {chef_rest, auth_skey}.
          auth_skew = 900 :: non_neg_integer(),

          %% The GUID for the organization name that appears in the
          %% request URL.  This gets set in chef_wm_base:malformed_request
          organization_guid :: object_id(),

          %% The name of the organization parsed from the request URL.
          %% Set by chef_rest_wm:service_available.
          organization_name :: binary() | ?OSC_ORG_NAME,

          %% The authz identifier of the org, populated based on the presence
          %% of an organization name in the request url
          organization_authz_id :: object_id(),

          %% Run time configurable dark launch info
          %% Set by chef_rest_wm:service_available.
          %% In open source chef this is left undefined
          %% In OPC/OHC it will either contain dark launch info or no_header
          darklaunch = undefined :: any(), %% do not want to expose darklaunch record globally

          %% Opaque db connection context record as returned by
          %% chef_db:make_context.  Allows db layer access to request
          %% ID.  Set in chef_rest_wm:service_available
          chef_db_context :: chef_db:db_context(),

          %% Opaque db connection context record as returned by chef_authz:make_context.
          chef_authz_context :: oc_chef_authz:oc_chef_authz_context(),

          %% AuthzId for the actor making the request.
          requestor_id :: object_id(),

          %% Details for The actor making the request.
          requestor :: #chef_requestor{} | #chef_client{},

          %% A record containing resource-specific state.
          resource_state :: resource_state(),

          %% Turn this on if superuser is allowed to bypass security checks for
          %% this endpoint.
          superuser_bypasses_checks = false :: true | false,

          %% A proplist of config for metric reporting and stats_hero integration.
          metrics_config :: [{atom(), term()}],

          %% list of resource arguments passed in on init of each resource by webmachine
          %% This will be set to 'Value' of a tuple in the resource argument list in
          %% dispatch.conf matching the form {resource_args, Value}
          resource_args :: atom(),

          % Multiple resources may create a key at time of object creation.  Expose it
          % in base_state so that it's available for common handling.
          key_context ::undefined | #key_context{}

         }).

-type chef_wm_create_update_response() :: {true | {halt, 403 | 500 | 409}, wm_req(), #base_state{}}.

-record(client_state, {
          client_data,
          client_authz_id,
          generated_private_key,
          chef_client :: #chef_client{} | not_found
         }).

-record(cookbook_state, {
          %% authz id of the cookbook
          authz_id,
          %% authz id for the cookbooks container
          cookbook_container_id,
          %% EJson representation of a cookbook version
          cookbook_data,
          %% cookbook name from the URL
          cookbook_name,
          %% cookbook version from the URL
          cookbook_version,
          chef_cookbook_version :: #chef_cookbook_version{} | undefined,
          %% number of versions to display when doing cookbook list
          num_versions :: non_neg_integer() | all | undefined
         }).

-record(cookbook_artifact_version_state, {
          authz_id,
          cookbook_artifact_version_data :: ejson_term(),
          oc_chef_cookbook_artifact_version :: #oc_chef_cookbook_artifact_version{}
         }).

-record(cookbook_artifacts_state, {
          oc_chef_cookbook_artifacts :: [#oc_chef_cookbook_artifact{}]
         }).

-record(data_state, {
          data_bag_name,
          data_bag_item_name,
          data_bag_item_ejson,
          data_bag_authz_id,
          chef_data_bag :: #chef_data_bag{} | undefined,
          chef_data_bag_item :: #chef_data_bag_item{} | undefined
         }).

-record(environment_state, {
          environment_data,
          environment_authz_id,
          chef_environment :: #chef_environment{},

          %% Used for when we're returning environment-filtered cookbook version info
          num_versions :: num_versions(),

          %% Used when we're grabbing specific cookbooks filtered through an environment
          %% `all' indicates that all cookbooks should be returned (duh)
          cookbook :: binary() | all
         }).

-record(node_state, {
          environment_name,
          node_data,
          node_authz_id,
          chef_node :: #chef_node{}
         }).

-record(role_state, {
          env_run_list_only = false :: boolean(),
          %% EJson-encoded representation of a Role
          role_data,
          role_authz_id,
          chef_role :: #chef_role{}
         }).

-record(sandbox_state, {
          id,
          sandbox_authz_id,
          sandbox_data,
          chef_sandbox
          }).

-record(search_state, {
          solr_query = undefined,
          partial_paths = []
         }).

-record(principal_state, {
          name,
          public_key,
          type,
          authz_id
         }).

-record(depsolver_state, {
          chef_environment :: #chef_environment{},
          %% environment within which to depsolve from the URL
          environment_name :: binary(),
          %% list of required cookbooks from POST.  These have been processed
          %% and if there was a version in the recipe the we store it as a
          %% cookbook name, version tuple
          run_list_cookbooks :: [binary() | {binary(), binary()}]
        }).

-record(user_state, {
          user_data,
          user_authz_id,
          generated_private_key,
          chef_user :: #chef_user{} | not_found
      }).

-record(container_state, {
          container_data,
          container_authz_id,
          oc_chef_container :: #oc_chef_container{}
         }).


-record(control_state, {
          control_data,
          control_group_id
         }).

-record(group_state, {
          group_data,
          group_authz_id,
          oc_chef_group :: #oc_chef_group{}
         }).

-record(policy_state, {
          policy_data,
          policy_authz_id,
          created_policy = false,
          policy_group_authz_id,
          created_policy_group = false,
          policy_data_for_response,
          policy_assoc_exists = false,
          oc_chef_policy_group_revision_association :: #oc_chef_policy_group_revision_association{}
         }).

-record(named_policy_revisions_state, {
          policy_name,
          create_policy = false,
          policy_record,
          policy_revision_data,
          policy_authz_id,
          policy_data_for_response
         }).

-record(named_policy_named_rev_state, {
          policy_name,
          revision_id,
          policy_record,
          policy_revision_record,
          policy_authz_id
         }).

-record(policy_group_state, {
          policy_group,
          policy_group_authz_id
         }).

-record(acl_state, {
          type,
          authz_id,
          acl_data,
          granular :: 'granular' | undefined

         }).

-record(association_state, {
          data :: jiffy:json_value(),
          user,
          org_user_association,
          org_user_invite,
          user_name
         }).

-record(organization_state, {
          organization_data,
          organization_authz_id,
          oc_chef_organization :: #oc_chef_organization{}
         }).

-record(key_state, {
          type,
          full_type,
          parent_id,
          parent_authz_id,
          parent_name :: binary(),
          key_data,
          generated_private_key,
          chef_key :: #chef_key{}
         }).
-type key_state() :: #key_state{}.

-record(object_identifier_state, {id :: object_id(),
                                  authz_id :: object_id(),
                                  org_id :: object_id()}).

-type resource_state() ::   undefined |
                            #client_state{} |
                            #cookbook_state{} |
                            #cookbook_artifact_version_state{} |
                            #environment_state{} |
                            #node_state{} |
                            #role_state{} |
                            #sandbox_state{} |
                            #data_state{} |
                            #container_state{} |
                            #depsolver_state{} |
                            #group_state{} |
                            #policy_state{} |
                            #organization_state{} |
                            #user_state{} |
                            #principal_state{} |
                            [#principal_state{}] |
                            #object_identifier_state{} |
                            #search_state{} |
                            #association_state{} |
%% Sometimes resourse_state() is a chef_object stub. A possible refactor here would be to
%% teach oc_chef_wm_base:list_objects_json how to determine which chef_object stub to build
%% based on the state type, or to give base_state{} a field for chef_object_stub to use here
%% instead. This would make the intention of the resource_state field clearer
                            #chef_client{}   |
                            #chef_data_bag{} |
                            #chef_environment{} |
                            #chef_role{}.

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).

-define(REDACTED_PASSWORD, <<"*******">>).
