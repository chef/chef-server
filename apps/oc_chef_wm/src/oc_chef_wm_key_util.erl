%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_key_util).

-include("../../include/oc_chef_wm.hrl").

-export([validate_get_request/5,
	 auth_info/2,
	 make_resource_state_for_object/4]).

validate_get_request(Req, State, Ctx, ObjectType, OrgId) ->
    ObjectName = chef_wm_util:object_name(ObjectType, Req),
    ResourceState = make_resource_state_for_object(Ctx, ObjectType, ObjectName, OrgId),
    {Req, State#base_state{resource_state = ResourceState}}.

make_resource_state_for_object(DbContext, client, Name, OrgId) ->
    make_resource_state_for_object(client, chef_db:fetch(#chef_client{name = Name, org_id = OrgId}, DbContext));
make_resource_state_for_object(DbContext, user, Name, _OrgId) ->
    make_resource_state_for_object(user, chef_db:fetch(#chef_user{username = Name}, DbContext)).

make_resource_state_for_object(_Type, not_found) ->
    #key_state{};
make_resource_state_for_object(Type, Object) ->
    FullType = list_to_existing_atom(atom_to_list(Type) ++ "_key"),
    #key_state{type = Type, full_type = FullType, parent_name = chef_object:name(Object),
               parent_authz_id = chef_object:authz_id(Object), parent_id = chef_object:id(Object)}.

%% Permissions:
%% The permissions model is the same between oc_chef_wm_keys and oc_chef_wm_named_key
%% Keys are considered an attribute of the object that owns them:
%%   * target object (user or client) must exist
%%   * In order to view a target's keys, requestor will need read access to that target. (handled in default is_authorized)
%%   * In order to update/delete/add a key for a target, requestor will need update access to that target.
%%   * If requestor is a user and target is a client, user must in the same org as the client
%%   * If requestor is a user and target is self, requestor gets full permissions without specific acl checks.
%%
%% Before proceeding to authorize, first ensure that the owning actor (client or user) specified in the url exists.
auth_info(Req, #base_state{resource_args = TargetType, resource_state = #key_state{parent_id = undefined}} = State) ->
    Name = chef_wm_util:object_name(TargetType, Req),
    Message = chef_wm_util:not_found_message(TargetType, Name),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = {TargetType, not_found}}};
%% If requestor is a user and target is self, requestor is authorized
auth_info(Req, #base_state{ requestor_id = RequestorAuthzId,
                            resource_state = #key_state{parent_authz_id = RequestorAuthzId}} = State) ->
    {authorized, Req, State};
auth_info(Req, #base_state{ resource_mod = Mod } = State) ->
    Mod:auth_info(wrq:method(Req), Req, State).

