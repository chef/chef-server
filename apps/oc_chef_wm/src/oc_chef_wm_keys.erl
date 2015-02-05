%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
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

-module(oc_chef_wm_keys).

-include("../../include/oc_chef_wm.hrl").
%-include("include/oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).


%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         to_json/2 ]).

-export([allowed_methods/2,
         create_path/2,
         conflict_message/1,
         from_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #keys_state{}}.

request_type() ->
    "keys".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request('GET', Req, #base_state{resource_args = ObjectType, chef_db_context = Ctx, organization_guid = OrgId} = State) ->
    ObjectName = chef_wm_util:object_name(ObjectType, Req),
    ResourceState = make_resource_state_for_object(Ctx, ObjectType, ObjectName, OrgId),
    {Req, State#base_state{resource_state = ResourceState }}.

make_resource_state_for_object(DbContext, client, Name, OrgId) ->
    make_resource_state_for_object(client, chef_db:fetch(#chef_client{name = Name, org_id = OrgId}, DbContext));
make_resource_state_for_object(DbContext, user, Name, _OrgId) ->
    make_resource_state_for_object(user, chef_db:fetch(#chef_user{username = Name}, DbContext)).

make_resource_state_for_object(_Type, not_found) ->
    #keys_state{};
make_resource_state_for_object(Type, Object) ->
    #keys_state{type = Type, name = chef_object:name(Object), authz_id = chef_object:authz_id(Object), id = chef_object:id(Object)}.


%% Permissions:
%% The permissios model here and in named_key will be the same
%% Keys are considered an attribute of the object that owns them:
%%   * target object (user or client) must exist
%%   * In order to view a target's keys, requestor will need read access to that target. (handled in default is_authorized)
%%   * In order to update/delete/add a key for a target, requestor will need update access to that target.
%%   * If requestor is a user and target is a client, user must in the same org as the client
%%   * If requestor is a user and target is self, requestor gets full permissions without specific acl checks.
%%
%% Before proceeding to authorize, first ensure that the owning actor (client or user) specified in the url exists.
auth_info(Req, #base_state{resource_args = TargetType, resource_state = #keys_state{id = undefined}} = State) ->
    Name = chef_wm_util:object_name(TargetType, Req),
    Message = chef_wm_util:not_found_message(TargetType, Name),
    Req1 = chef_wm_util:set_json_body(Req, Message),
    {{halt, 404}, Req1, State#base_state{log_msg = {TargetType, not_found}}};
%% If requestor is a user and target is self, requestor is authorized
auth_info(Req, #base_state{ requestor_id = RequestorAuthzId,
                            resource_state = #keys_state{authz_id = RequestorAuthzId}} = State) ->
    {authorized, Req, State};
auth_info(Req, State) ->
    auth_info(wrq:method(Req), Req, State).

auth_info('GET', Req, #base_state{resource_state = #keys_state{ authz_id = AuthzId }}= State) ->
    {{actor, AuthzId}, Req, State};
auth_info(_Method, Req, #base_state{resource_state = #keys_state{ authz_id = AuthzId}}= State) ->
    {{actor, AuthzId, update}, Req, State}.

create_path(_Req, _State) ->
    error(unsupported).

to_json(Req, #base_state{ chef_db_context = DbContext,
                          resource_state = #keys_state{type = Type, id = Id, name = ObjectName} } = State) ->
    case chef_db:list(#chef_key{id = Id}, DbContext) of
        Keys when is_list(Keys) ->
            KeyType = list_to_existing_atom(atom_to_list(Type) ++ "_key"),
            RouteFun = oc_chef_wm_routes:bulk_route_fun(KeyType, ObjectName, Req),
            EJ = chef_key:ejson_from_list(Keys, RouteFun),
            {chef_json:encode(EJ), Req, State};
        Error ->
            {{halt, 500}, Req, State#base_state{log_msg = Error }}
    end.

malformed_request_message(Any, Req, State) ->
    chef_wm_malformed:malformed_request_message(Any, Req, State).
conflict_message(_Name) ->
    ok.

from_json(_, _) ->
    error(unsupported).

