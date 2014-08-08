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

-module(oc_chef_wm_acl).

-include_lib("chef_wm/include/chef_wm.hrl").
-include_lib("oc_chef_wm.hrl").

% For getting out the ReqId for stats_hero:
-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

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
         to_json/2,
         % Also used by oc_chef_wm_acl_permission:
         validate_authz_id/7,
         check_acl_auth/2
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
    {['GET'], Req, State}.

validate_request('GET', Req, #base_state{chef_db_context = DbContext,
                                         organization_guid = OrgId,
                                         organization_name = OrgName,
                                         resource_state = #acl_state{type = Type} = 
                                             AclState} = State) ->
    validate_authz_id(Req, State, AclState, Type, OrgId, OrgName, DbContext).

auth_info(Req, State) ->
    check_acl_auth(Req, State).

to_json(Req, #base_state{resource_state = AclState} = State) ->
    case fetch(AclState) of
        forbidden ->
            {{halt, 403}, Req, State#base_state{log_msg = acl_not_found}};
        Ejson ->
            Json = chef_json:encode(Ejson),
            {Json, Req, State}
    end.

%% Also used by oc_chef_wm_acl_permission

validate_authz_id(Req, State, AclState, Type, OrgId, OrgName, DbContext) ->
    Name = chef_wm_util:object_name(Type, Req),
    try
        AuthzId = case Type of
                      cookbook ->
                          fetch_cookbook_id(DbContext, Name, OrgName);
                      NotCookbook ->
                          fetch_id(NotCookbook, DbContext, Name, OrgId)
                  end,
        AclState1 = AclState#acl_state{authz_id = AuthzId},
        % We're doing our own checks, so don't fail on superuser (also, not
        % doing this results in spurious auth failures):
        {Req, State#base_state{resource_state = AclState1,
                               superuser_bypasses_checks = true}}
    catch
        throw:{not_found, Name} ->
            Message = chef_wm_util:not_found_message(Type, Name),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = acl_not_found}}
    end.

check_acl_auth(Req, #base_state{requestor_id = RequestorId,
                                resource_state = #acl_state{type = Type,
                                                            authz_id = AuthzId}} = State) ->
    Path = acl_auth_path(Type, AuthzId, RequestorId),
    SuperuserId = envy:get(oc_chef_authz, authz_superuser_id, binary),
    Check = oc_chef_authz_http:request(Path, get, ?DEFAULT_HEADERS, [], SuperuserId),
    case Check of
        ok ->
            {authorized, Req, State};
        {error, not_found} ->
            {{halt, 403}, Req, State};
        Other ->
            Other
    end.

%% Internal functions

% TODO: we only need the authz id, so grabbing complete objects is wasteful.
% Also, this might be more suited to be moved to oc_chef_wm_util or
% something. In the meantime, this gets us up and running.
fetch_id(organization, _DbContext, _Name, _OrgId) ->
    % TODO: This needs to be implemented; orgs not in SQL yet.  Will also
    % require additional changes elsewhere to work
    throw(not_implemented);
fetch_id(user, DbContext, Name, _OrgId) ->
    case chef_db:fetch(#chef_user{username = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_user{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(client, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_client{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(container, DbContext, Name, OrgId) ->
    case chef_db:fetch(#oc_chef_container{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #oc_chef_container{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(data_bag, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_data_bag{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_data_bag{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(node, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_node{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_node{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(role, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_role{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_role{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(group, #context{reqid = ReqId}, Name, OrgId) ->
    % Yes, this is ugly, but functionally it's identical to the internal logic
    % of a regular group fetch, minus expanding the group members and such.
    % And the regular group fetch was breaking for some reason I couldn't
    % figure out, and at least this avoids that and doesn't spent time on
    % extra requests
    case stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_object:default_fetch(#oc_chef_group{org_id = OrgId,
                                                                           name = Name},
                                                            fun chef_sql:select_rows/1)
                          end) of
        not_found ->
            throw({not_found, Name});
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(environment, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            throw({not_found, Name});
        #chef_environment{authz_id = AuthzId} ->
            AuthzId
    end.

% TODO: bespoke code; cookbooks retrieval is by orgname instead of ID
fetch_cookbook_id(DbContext, Name, OrgName) ->
    % cookbook endpoint pattern is utterly different from the others, generic
    % fetch does not handle cookbooks (and, well, versioning)
    case chef_db:fetch_latest_cookbook_version(DbContext, OrgName, Name) of
        not_found ->
            throw({not_found, Name});
        {cookbook_exists, AuthzId} ->
            % unclear when this can happen; I assume for wrong version but
            % won't happen with 'latest' version?  But still checking for it
            % here.
            AuthzId;
        #chef_cookbook_version{authz_id = AuthzId} ->
            AuthzId
    end.

% Translate types; in authz, everything is an object, actor, group, or container
acl_path(node, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(role, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(data_bag, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(environment, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(cookbook, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(client, AuthzId) ->
    acl_path(actor, AuthzId);
acl_path(user, AuthzId) ->
    acl_path(actor, AuthzId);
acl_path(organization, AuthzId) ->
    acl_path(container, AuthzId);
acl_path(Type, AuthzId) ->
    "/" ++ atom_to_list(Type) ++ "s/" ++ binary_to_list(AuthzId) ++ "/acl".

fetch(#acl_state{type = Type, authz_id = AuthzId}) ->
    Path = acl_path(Type, AuthzId),
    SuperuserId = envy:get(oc_chef_authz, authz_superuser_id, binary),
    Result = oc_chef_authz_http:request(Path, get, ?DEFAULT_HEADERS, [], SuperuserId),
    case Result of 
        {ok, Record} ->
            ids_to_names(Record);
        {error, forbidden} ->
            forbidden;
        Other ->
            Other
    end.

acl_auth_path(Type, AuthzId, RequestorId) ->
    acl_path(Type, AuthzId) ++ "/grant/actors/" ++ binary_to_list(RequestorId).

convert_group_ids_to_names(AuthzIds) ->
    oc_chef_group:find_groups_names(AuthzIds, fun chef_sql:select_rows/1).

convert_actor_ids_to_names(AuthzIds) ->
    {ClientNames, RemainingAuthzIds} =
        oc_chef_group:find_clients_names(AuthzIds, fun chef_sql:select_rows/1),
    {UserNames, DefunctActorAuthzIds} =
        oc_chef_group:find_users_names(RemainingAuthzIds, fun chef_sql:select_rows/1),
    {ClientNames ++ UserNames, DefunctActorAuthzIds}.

process_part(Part, Record) ->
    Members = ej:get({Part}, Record),
    ActorIds = ej:get({<<"actors">>}, Members),
    GroupIds = ej:get({<<"groups">>}, Members),
    {ActorNames, DefunctActorAuthzIds} = convert_actor_ids_to_names(ActorIds),
    {GroupNames, DefunctGroupAuthzIds} = convert_group_ids_to_names(GroupIds),
    % We do this for groups, probably good to do it here too
    oc_chef_authz_cleanup:add_authz_ids(DefunctActorAuthzIds, DefunctGroupAuthzIds),
    Members1 = ej:set({<<"actors">>}, Members, ActorNames),
    Members2 = ej:set({<<"groups">>}, Members1, GroupNames),
    ej:set({Part}, Record, Members2).

ids_to_names(Record) ->
    Record1 = process_part(<<"create">>, Record),
    Record2 = process_part(<<"read">>, Record1),
    Record3 = process_part(<<"update">>, Record2),
    Record4 = process_part(<<"delete">>, Record3),
    process_part(<<"grant">>, Record4).

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
