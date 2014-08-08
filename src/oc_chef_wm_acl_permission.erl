%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@getchef.com>
%% Copyright 2014 Chef, Inc. All Rights Reserved.
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

-module(oc_chef_wm_acl_permission).

-include_lib("chef_wm/include/chef_wm.hrl").
-include_lib("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         from_json/2
        ]).

-define(DEFAULT_HEADERS, []).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(Config) ->
    AclType = ?gv(acl_object_type, Config),
    {ok, #acl_state{type = AclType}}.

request_type() ->
    "acl".

allowed_methods(Req, State) ->
    {['PUT'], Req, State}.

validate_request('PUT', Req, #base_state{chef_db_context = DbContext,
                                         organization_guid = OrgId,
                                         organization_name = OrgName,
                                         resource_state = #acl_state{type = Type} = 
                                             AclState} = State) ->
    Body = wrq:req_body(Req),
    Ace = chef_json:decode_body(Body),
    Part = list_to_binary(wrq:path_info(acl_permission, Req)),
    case chef_object_base:strictly_valid(acl_spec(Part), [Part], Ace) of
        ok ->
            oc_chef_wm_acl:validate_authz_id(Req, State,
                                             AclState#acl_state{acl_data = Ace},
                                             Type, OrgId, OrgName, DbContext);
        Other ->
            throw(Other)
    end.

auth_info(Req, State) ->
    oc_chef_wm_acl:check_acl_auth(Req, State).

from_json(Req, #base_state{organization_guid = OrgId,
                           resource_state = AclState} = State) ->
    Part = wrq:path_info(acl_permission, Req),
    case update_from_json(AclState, Part, OrgId) of
        forbidden ->
            {{halt, 400}, Req, State};
        bad_actor ->
            % We don't actually know which one, so can't use a more specific
            % message without checking every single object (which we don't yet
            % do efficiently); we only get back a list of Ids that has a
            % different length than the list of names we started with.
            Msg = <<"Invalid/missing actor in request body">>,
            Msg1 = {[{<<"error">>, [Msg]}]},
            Req1 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 400}, Req1, State#base_state{log_msg = bad_actor}};
        bad_group ->
            Msg = <<"Invalid/missing group in request body">>,
            Msg1 = {[{<<"error">>, [Msg]}]},
            Req1 = wrq:set_resp_body(chef_json:encode(Msg1), Req),
            {{halt, 400}, Req1, State#base_state{log_msg = bad_group}};
        _Other ->
            % So we return 200 instead of 204, for backwards compatibility:
            Req1 = wrq:set_resp_body(<<"{}">>, Req),
            {true, Req1, State}
    end.

%% Internal functions

acl_spec(Part) ->
    {[
      {Part,
       {[
         {<<"actors">>, {array_map, string}},
         {<<"groups">>, {array_map, string}}
        ]}}
     ]}.

% Translate types; in ACLs, everything is an object, actor, group, or container
acl_path(node, AuthzId, Part) ->
    acl_path(object, AuthzId, Part);
acl_path(role, AuthzId, Part) ->
    acl_path(object, AuthzId, Part);
acl_path(data_bag, AuthzId, Part) ->
    acl_path(object, AuthzId, Part);
acl_path(environment, AuthzId, Part) ->
    acl_path(object, AuthzId, Part);
acl_path(cookbook, AuthzId, Part) ->
    acl_path(object, AuthzId, Part);
acl_path(client, AuthzId, Part) ->
    acl_path(actor, AuthzId, Part);
acl_path(user, AuthzId, Part) ->
    acl_path(actor, AuthzId, Part);
acl_path(organization, AuthzId, Part) ->
    acl_path(container, AuthzId, Part);
acl_path(Type, AuthzId, Part) ->
    "/" ++ atom_to_list(Type) ++ "s/" ++ binary_to_list(AuthzId) ++ "/acl/" ++ Part.

update_from_json(#acl_state{type = Type, authz_id = AuthzId, acl_data = Data},
                 Part, OrgId) ->
    try
        update_part(Part, Data, Type, AuthzId, OrgId)
    catch
        throw:forbidden ->
            forbidden;
        throw:bad_actor ->
            forbidden;
        throw:bad_group ->
            forbidden
    end.

convert_group_names_to_ids(GroupNames, OrgId) ->
    oc_chef_group:find_group_authz_ids(GroupNames, OrgId, fun chef_sql:select_rows/1).

convert_actor_names_to_ids(Names, OrgId) ->
    ClientIds = oc_chef_group:find_client_authz_ids(Names, OrgId,
                                                    fun chef_sql:select_rows/1),
    UserIds = oc_chef_group:find_user_authz_ids(Names, fun chef_sql:select_rows/1),
    ClientIds ++ UserIds.

names_to_ids(Ace, OrgId) ->
    ActorNames = ej:get({<<"actors">>}, Ace),
    GroupNames = ej:get({<<"groups">>}, Ace),
    ActorIds = convert_actor_names_to_ids(ActorNames, OrgId),
    % Check to make sure everything got converted; if something is missing,
    % there was an invalid actor or group name in the request body
    case length(ActorNames) == length(ActorIds) of
        false ->
            throw(bad_actor);
        _ ->
            GroupIds = convert_group_names_to_ids(GroupNames, OrgId),
            case length(GroupNames) == length(GroupIds) of
                false ->
                    throw(bad_group);
                _ ->
                    Ace1 = ej:set({<<"actors">>}, Ace, ActorIds),
                    ej:set({<<"groups">>}, Ace1, GroupIds)
            end
    end.

update_part(Part, AceRecord, Type, AuthzId, OrgId) ->
    Ids = names_to_ids(ej:get({Part}, AceRecord), OrgId),
    Data = ejson:encode(Ids),
    Path = acl_path(Type, AuthzId, Part),
    SuperuserId = envy:get(oc_chef_authz, authz_superuser_id, binary),
    Result = oc_chef_authz_http:request(Path, put, ?DEFAULT_HEADERS, Data, SuperuserId),
    case Result of
        {error, forbidden} ->
            throw(forbidden);
        Other ->
            Other
    end.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
