%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @doc Resource module for Environment Roles endpoint
%% Copyright 2012-2018 Chef Software, Inc.
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


-module(chef_wm_environment_roles).

%% chef_wm behaviour callbacks
-include("oc_chef_wm.hrl").
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0]).
%% validate_request/3 is mixed in below

-mixin([{oc_chef_wm_base , [forbidden/2,
                            is_authorized/2,
                            service_available/2,
                            content_types_accepted/2,
                            content_types_provided/2,
                            finish_request/2,
                            malformed_request/2,
                            validate_request/3,
                            ping/2]}]).

%% Webmachine callbacks implented in this module (i.e., not mixed-in)
-export([allowed_methods/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #role_state{}}.

request_type() ->
    "roles".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = RoleState} = State) ->
    RoleName = chef_wm_util:object_name(role, Req),
    EnvName = chef_wm_util:object_name(environment, Req),
    case chef_db:fetch(#chef_role{org_id = OrgId, name = RoleName}, DbContext) of
        not_found ->
            Message = chef_wm_util:not_found_message(role, RoleName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = role_not_found}};
        #chef_role{authz_id = AuthzId} = Role ->
            case environment_exists(EnvName, OrgId, DbContext) of
                true ->
                    %% check authz here
                    RoleState1 = RoleState#role_state{chef_role = Role},
                    State1 = State#base_state{resource_state = RoleState1},
                    {{object, AuthzId}, Req, State1};
                false ->
                    Message = chef_wm_util:not_found_message(environment, EnvName),
                    Req2 = chef_wm_util:set_json_body(Req, Message),
                    {{halt, 404}, Req2, State#base_state{log_msg = env_not_found}}
            end
    end.

to_json(Req, #base_state{resource_state = #role_state{
                           chef_role = #chef_role{
                             serialized_object = JSON}}} = State) ->
    EnvName = chef_wm_util:object_name(environment, Req),
    RoleJson = chef_json:decode(chef_db_compression:decompress(JSON)),
    RunList = retrieve_run_list(EnvName, RoleJson),
    Body = {[{<<"run_list">>, RunList}]},
    {chef_json:encode(Body), Req, State}.

retrieve_run_list(<<"_default">>, RoleJson) ->
  ej:get({<<"run_list">>}, RoleJson, null);
retrieve_run_list(EnvName, RoleJson) ->
  ej:get({"env_run_lists", EnvName}, RoleJson, null).

environment_exists(<<"_default">>, _OrgName, _DbContext) ->
    %% The default environment always exists, by definition
    true;
environment_exists(Name, OrgId, DbContext) ->
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            false;
        #chef_environment{} ->
            true
    end.
