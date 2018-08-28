%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Falcon <seth@chef.io>
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


-module(chef_wm_named_role).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(Config) ->
    %% this resource serves named roles as well as the /roles/:role/environments endpoint
    %% that returns just the names of the environments that a given role has
    %% environment-specific run lists for. The behavior is determined by the value of init
    %% parameter 'env_run_list_only'.
    EnvRunListOnly = (true =:= proplists:get_value(env_run_list_only, Config)),
    {ok, #role_state{env_run_list_only = EnvRunListOnly}}.

request_type() ->
    "roles".

allowed_methods(Req, #base_state{resource_state = RoleState} = State) ->
    Methods = case RoleState#role_state.env_run_list_only of
                  true ->
                      ['GET'];
                  false ->
                      ['GET', 'PUT', 'DELETE']
              end,
    {Methods, Req, State}.

validate_request(Method, Req, State) when Method == 'GET';
                                          Method == 'DELETE' ->
    {Req, State};
validate_request('PUT', Req, #base_state{resource_state = RoleState} = State) ->
    Name = chef_wm_util:object_name(role, Req),
    Body = wrq:req_body(Req),
    {ok, Role} = chef_role:parse_binary_json(Body, {update, Name}),
    {Req, State#base_state{resource_state = RoleState#role_state{role_data = Role}}}.

%% Memoize the container id so we don't hammer the database
-spec auth_info(wm_req(), chef_wm:base_state()) ->
                       chef_wm:auth_info_return().
auth_info(Req, #base_state{chef_db_context = DbContext,
                           resource_state = RoleState,
                           organization_guid = OrgId} = State) ->
    RoleName = chef_wm_util:object_name(role, Req),
    case chef_db:fetch(#chef_role{org_id = OrgId, name = RoleName}, DbContext) of
        not_found ->
            Message = chef_wm_util:not_found_message(role, RoleName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = role_not_found}};
        #chef_role{authz_id = AuthzId} = Role ->
            RoleState1 = RoleState#role_state{chef_role = Role},
            State1 = State#base_state{resource_state = RoleState1},
            {{object, AuthzId}, Req, State1}
    end.

%% Org is checked for in malformed_request/2, role is checked for in forbidden/2;
%% if we get this far, it exists.
resource_exists(Req, State) ->
    {true, Req, State}.

to_json(Req, #base_state{resource_state = #role_state{
                           env_run_list_only = EnvRunListOnly,
                           chef_role = #chef_role{
                             serialized_object = Gzip}}} = State) ->
    JSON = chef_db_compression:decompress(Gzip),
    case EnvRunListOnly of
        true ->
            Environments = chef_role:environments(chef_json:decode(JSON)),
            {chef_json:encode(Environments), Req, State};
        false ->
            {JSON, Req, State}
    end.

from_json(Req, #base_state{resource_state = #role_state{chef_role = Role,
                                                        role_data = RoleData}} = State) ->
    oc_chef_wm_base:update_from_json(Req, State, Role, RoleData).

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #role_state{
                                                     chef_role = Role}
                                } = State) ->

    ok = oc_chef_wm_base:delete_object(DbContext, Role, RequestorId),

    Json = chef_db_compression:decompress(Role#chef_role.serialized_object),
    {true, wrq:set_resp_body(Json, Req), State}.

%% Private utility functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
