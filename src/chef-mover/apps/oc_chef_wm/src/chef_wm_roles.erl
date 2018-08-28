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


-module(chef_wm_roles).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2,
                           {list_objects_json/2, to_json}]}]).

-export([allowed_methods/2,
         conflict_message/1,
         create_path/2,
         from_json/2,
         resource_exists/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #role_state{}}.

request_type() ->
    "roles".

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, #base_state{organization_guid = OrgId} = State) ->
    %% Put a stub chef_role record into the resource_state. This allows us to use shared
    %% code for generating the map of name => URL returned for GET /roles.  OrgId is set via
    %% malformed_request.
    {Req, State#base_state{resource_state = #chef_role{org_id = OrgId}}};
validate_request('POST', Req, #base_state{resource_state = RoleState} = State) ->
    Body = wrq:req_body(Req),
    {ok, Role} = chef_role:parse_binary_json(Body, create),
    {Req, State#base_state{resource_state = RoleState#role_state{role_data = Role}}}.

auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('POST', Req, State) ->
    {{create_in_container, role}, Req, State};
auth_info('GET', Req, State) ->
    {{container, role}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #role_state{role_data = RoleData}}=State) ->
    Name = ej:get({<<"name">>}, RoleData),
    {binary_to_list(Name), Req, State}.

from_json(Req, #base_state{resource_state =
                               #role_state{role_data = RoleData,
                                           role_authz_id = AuthzId}} = State) ->
    oc_chef_wm_base:create_from_json(Req, State, chef_role, {authz_id, AuthzId}, RoleData).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

-spec conflict_message(binary()) -> ejson_term().
conflict_message(_Name) ->
    {[{<<"error">>, [<<"Role already exists">>]}]}.
