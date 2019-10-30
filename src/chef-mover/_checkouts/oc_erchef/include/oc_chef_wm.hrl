%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Stephen Delano <stephen@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @author many more...
%%
%% Copyright 2011-2014 Chef Software, Inc. All Rights Reserved.
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
-include_lib("eunit/include/eunit.hrl").
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

-type permission() :: create | delete | read | update.

-type container_name() :: cookbook |
                          data |
                          environment |
                          node |
                          role |
                          client |
                          container |
                          sandbox.

-type auth_tuple() :: {object, object_id(), permission()} |
                      {container, container_name(), permission()}.

-type wm_req() :: #wm_reqdata{}.

%% Shared resource state shared by all chef_wm resource modules.
-record(base_state, {
          %% Concrete resource impl
          resource_mod :: atom() | undefined,
          %% unique request ID from nginx header (or generated if not
          %% found) set by chef_wm_util:read_req_id.
          reqid :: binary() | undefined,

          %% The name of the HTTP request header containing the unique ID set by the load
          %% balancer
          reqid_header_name :: string() | undefined,

          %% String containing API version info for the chef server
          api_version :: string() | undefined,

          %% OTP information for the Erchef server in {ReleaseName, OtpVersion} form.
          otp_info :: {string(), string()} | undefined,

          %% Indicates what variant of Chef Server this is (e.g. "osc" => Open Source Chef,
          %% "opc" = Opscode Private Chef, etc).
          server_flavor :: string() | undefined,

          %% A fun/1 that closes over the request headers and returns
          %% header values as binaries or 'undefined'.
          %% chef_rest_wm:init sets this.
          header_fun = undefined :: fun((binary()|string()) -> binary() | 'undefined'),

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
          organization_guid :: object_id() | undefined,

          %% The name of the organization parsed from the request URL.
          %% Set by chef_rest_wm:service_available.
          organization_name :: binary() | ?OSC_ORG_NAME | undefined,

          %% The authz identifier of the org, populated based on the presence
          %% of an organization name in the request url
          organization_authz_id :: object_id() | undefined,

          %% Run time configurable dark launch info
          %% Set by chef_rest_wm:service_available.
          %% In open source chef this is left undefined
          %% In OPC/OHC it will either contain dark launch info or no_header
          darklaunch = undefined :: any(), %% do not want to expose darklaunch record globally

          %% Opaque db connection context record as returned by
          %% chef_db:make_context.  Allows db layer access to request
          %% ID.  Set in chef_rest_wm:service_available
          chef_db_context :: chef_db:db_context() | undefined,

          %% Opaque db connection context record as returned by chef_authz:make_context.
          chef_authz_context :: chef_authz:chef_authz_context() | undefined,

          %% AuthzId for the actor making the request.
          requestor_id :: object_id() | undefined,

          %% Details for The actor making the request.
          requestor :: #chef_client{} | #chef_user{} | undefined,

          %% A record containing resource-specific state.
          resource_state :: tuple() | undefined,

          %% Turn this on if superuser is allowed to bypass security checks for
          %% this endpoint.
          superuser_bypasses_checks = false :: true | false,

          %% A proplist of config for metric reporting and stats_hero integration.
          metrics_config :: [{atom(), term()}] | undefined,

          %% list of resource arguments passed in on init of each resource by webmachine
          %% This will be set to 'Value' of a tuple in the resource argument list in
          %% dispatch.conf matching the form {resource_args, Value}
          resource_args :: list() | undefined
         }).

-record(client_state, {
          client_data,
          client_authz_id,
          chef_client :: #chef_client{} | not_found | undefined
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
          chef_environment :: #chef_environment{} | undefined,

          %% Used for when we're returning environment-filtered cookbook version info
          num_versions :: num_versions() | undefined,

          %% Used when we're grabbing specific cookbooks filtered through an environment
          %% `all' indicates that all cookbooks should be returned (duh)
          cookbook :: binary() | all | undefined
         }).

-record(node_state, {
          environment_name,
          node_data,
          node_authz_id,
          chef_node :: #chef_node{} | undefined
         }).

-record(role_state, {
          env_run_list_only = false :: boolean(),
          %% EJson-encoded representation of a Role
          role_data,
          role_authz_id,
          chef_role :: #chef_role{} | undefined
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
          chef_environment :: #chef_environment{} | undefined,
          %% environment within which to depsolve from the URL
          environment_name :: binary() | undefined,
          %% list of required cookbooks from POST.  These have been processed
          %% and if there was a version in the recipe the we store it as a
          %% cookbook name, version tuple
          run_list_cookbooks :: [binary() | {binary(), binary()}] | undefined
        }).

-record(user_state, {
          user_data,
          user_authz_id,
          chef_user :: #chef_user{} | undefined
      }).

-record(container_state, {
          container_data,
          container_authz_id,
          oc_chef_container :: #oc_chef_container{} | undefined
         }).


-record(control_state, {
          control_data,
          control_group_id
         }).

-record(group_state, {
          group_data,
          group_authz_id,
          oc_chef_group :: #oc_chef_group{} | undefined
         }).

-record(acl_state, {
          type,
          authz_id,
          acl_data
         }).

-record(association_state, {
          data,
          user,
          org_user_association,
          org_user_invite,
          user_name
         }).

-record(organization_state, {
          organization_data,
          organization_authz_id,
          oc_chef_organization :: #oc_chef_organization{} | undefined
         }).

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).
