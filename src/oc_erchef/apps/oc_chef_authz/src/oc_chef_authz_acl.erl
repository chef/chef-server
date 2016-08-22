%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2014-2016 Chef Software, Inc.
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

-module(oc_chef_authz_acl).

-include("oc_chef_types.hrl").
-include("chef_types.hrl").

-export([acl_path/2,
         acl_path/3,
         acl_auth_path/3,
         fetch_id/4,
         fetch_cookbook_id/3,
         fetch/2,
         has_grant_on/3,
         validate_actors_clients_users/2,
         update_part/5,
         acl_spec/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(DEFAULT_HEADERS, []).

% For getting out the ReqId for stats_hero: (not necessary once fetch_id is fixed for groups)
-record(context, {server_api_version,
                  reqid :: binary(),
                  darklaunch = undefined}).

acl_spec(Part) ->
    {[
      {Part,
       {[
         {<<"groups">>, {array_map, string}},
         {<<"actors">>, {array_map, string}},
         {{opt, <<"clients">>}, {array_map, string}},
         {{opt, <<"users">>}, {array_map, string}}
        ]}}
     ]}.

% If client is present, user must be present
% If user is present, client must be present.
% If clients and users are present, actors must be an empty list.
%
% This is to ensure 'least surprise' - if you submit a
% request containing actors, clients, and users it's not
% going to be clear which one(s) are picked up for the update.
%
% This function assumes `acl_spec` has been applied in order to ensure
% that `groups` and `actors`  are always present and that
% the data types provided for each attribute are correct.
validate_actors_clients_users(Part, FullACL) ->
    ACL = ej:get({Part}, FullACL),
    Actors = ej:get({<<"actors">>}, ACL),
    Clients = ej:get({<<"clients">>}, ACL),
    Users = ej:get({<<"users">>}, ACL),
    case {Clients, Users} of
        {undefined, undefined} ->
            ok;
        {_, undefined} ->
            throw({one_requires_all, <<"clients">>, [<<"users">>]});
        {undefined, _} ->
            throw({one_requires_all, <<"users">>, [<<"clients">>]});
        {_, _} ->
            case Actors of
                [] -> ok;
                _ -> throw(actors_must_be_empty)
            end
    end.

update_part(Part, AceRecord, Type, AuthzId, OrgId) ->
    Ids = names_to_ids(ej:get({Part}, AceRecord), OrgId),
    Data = chef_json:encode(Ids),
    Path = acl_path(Type, AuthzId, Part),
    SuperuserId = envy:get(oc_chef_authz, authz_superuser_id, binary),
    Result = oc_chef_authz_http:request(Path, put, ?DEFAULT_HEADERS, Data, SuperuserId),
    case Result of
        {error, forbidden} ->
            throw(forbidden);
        Other ->
            Other
    end.

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
            not_found;
        #chef_user{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(client, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #chef_client{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(container, DbContext, Name, OrgId) ->
    case chef_db:fetch(#oc_chef_container{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #oc_chef_container{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(data_bag, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_data_bag{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #chef_data_bag{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(node, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_node{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #chef_node{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(role, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_role{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #chef_role{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(policy, DbContext, Name, OrgId) ->
    case chef_db:fetch(#oc_chef_policy{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #oc_chef_policy{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(policy_group, DbContext, Name, OrgId) ->
    case chef_db:fetch(#oc_chef_policy_group{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #oc_chef_policy_group{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(group, #context{server_api_version = ApiVersion, reqid = ReqId}, Name, OrgId) ->
    % Yes, this is ugly, but functionally it's identical to the internal logic
    % of a regular group fetch, minus expanding the group members and such.
    % And the regular group fetch was breaking for some reason I couldn't
    % figure out, and at least this avoids that and doesn't spent time on
    % extra requests
    case stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_object_default_callbacks:fetch(#oc_chef_group{server_api_version = ApiVersion,
                                                                                     org_id = OrgId,
                                                                                     name = Name},
                                                                      fun chef_sql:select_rows/1)
                          end) of
        not_found ->
            not_found;
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId
    end;
fetch_id(environment, DbContext, Name, OrgId) ->
    case chef_db:fetch(#chef_environment{org_id = OrgId, name = Name}, DbContext) of
        not_found ->
            not_found;
        #chef_environment{authz_id = AuthzId} ->
            AuthzId
    end.

fetch_cookbook_id(DbContext, Name, OrgId) ->
    % cookbook endpoint pattern is utterly different from the others, generic
    % fetch does not handle cookbooks (and, well, versioning)
    case chef_db:fetch_latest_cookbook_version(DbContext, OrgId, Name) of
        not_found ->
            not_found;
        #chef_cookbook_version{authz_id = AuthzId} ->
            AuthzId
    end.

%% Refactor this a bit
fetch(Type, AuthzId) ->
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

%%
%% bifrost has a an API
%% <OBJECTTYPE>/<OBJECTID>/grant/actors/<ACTORID> to check if actor has grant ace or not.
%%
%% This API only supports actors not groups, and other ACE types are
%% difficult since grant is required to read them.
has_grant_on(ObjectType, ObjectId, ActorId) ->
    Path = acl_auth_path(ObjectType, ObjectId, ActorId),
    SuperuserId = envy:get(oc_chef_authz, authz_superuser_id, binary),
    Check = oc_chef_authz_http:request(Path, get, ?DEFAULT_HEADERS, [],
                                       SuperuserId),
    case Check of
        ok ->
            true;
        {error, not_found} ->
            false;
        Other ->
            Other
    end.

%% Convert names provde in the ACE to their corresponding
%% authz ids.
names_to_ids(ACE, OrgId) ->
    ActorIds = case ej:get({<<"clients">>}, ACE) of
        undefined ->
            fetch_actors(OrgId, ej:get({<<"actors">>}, ACE));
        Clients ->
            safe_fetch_ids(user, undefined, ej:get({<<"users">>}, ACE)) ++
            safe_fetch_ids(client, OrgId, Clients)
    end,
    GroupIds = safe_fetch_ids(group, OrgId, ej:get({<<"groups">>}, ACE)),

    % Though we split out clients/users, the authz API
    % requires a unified list of actor IDs:
    ACE1 = ej:set({<<"actors">>}, ACE, ActorIds),
    ej:set({<<"groups">>}, ACE1, GroupIds).

%% Retrieves authz records for the given set of names
%% and returns a list of authz ids.
%% If the list of retrieved names + IDs
%% does not match the list of names provided, this raises
%% {invalid, Type, MissingList}}
safe_fetch_ids(_Type, _OrgId, []) ->
    % Save the cost of a query for an empty list -
    % fairly common for users and to a lesser extent groups.
    [];
safe_fetch_ids(Type, OrgId, Names) ->
    Records = oc_chef_authz_db:authz_records_by_name(Type, OrgId, Names),
    FoundNames = lists:sort(names_from_records(Records)),
    GivenNames = lists:sort(Names),
    case GivenNames -- FoundNames of
        [] ->
            ':+1:';
        Missing ->
            throw({invalid, Type, Missing})
    end,
    ids_from_records(Records).

fetch_actors(OrgId, ActorNames) ->
    {ok, Actors} = oc_chef_authz_db:find_org_actors_by_name(OrgId, ActorNames),
    {Missing, Remaining} = lists:partition(fun is_missing_actor/1, Actors),
    {Ambiguous, Valid} = lists:partition(fun is_ambiguous_actor/1, Remaining),
    case {Valid, Missing, Ambiguous} of
        {Ids, [], []}      -> ids_from_records(Ids);
        {_, Missing, []}   -> throw({bad_actor, names_from_records(Missing)});
        {_, [], Ambiguous} -> throw({ambiguous_actor, names_from_records(Ambiguous)});
        {_, _, Ambiguous}  -> throw({ambiguous_actor, names_from_records(Ambiguous)})
    end.

names_from_records(Records) ->
    [ name_from_record(R) || R  <- Records].
name_from_record({Name, _,  _}) ->
    Name;
name_from_record({Name, _}) ->
    Name.

ids_from_records(Records) -> [ id_from_record(R) || R <- Records ].

id_from_record({_, AuthzId}) ->
    AuthzId;
id_from_record({_, UserAuthzId, null}) ->
    UserAuthzId;
id_from_record({_, null, ClientAuthzId}) ->
    ClientAuthzId.

is_missing_actor({_, null, null}) ->
    true;
is_missing_actor({_, _, _}) ->
    false.

is_ambiguous_actor({_, UserAZ, ClientAZ}) when UserAZ =/= null andalso
                                               ClientAZ =/= null ->
    true;
is_ambiguous_actor({_, _, _}) ->
    false.

%%
%% Reverse mapping of ids to names (this should have a lot of commonality with groups)
%%
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


% Path helper functions
% Translate types; in ACLs, everything is an object, actor, group, or container
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
    acl_path(object, AuthzId);
acl_path(policy, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(policy_group, AuthzId) ->
    acl_path(object, AuthzId);
acl_path(Type, AuthzId) ->
    "/" ++ type_to_resource(Type) ++ "/" ++ binary_to_list(AuthzId) ++ "/acl".

acl_path(Type, AuthzId, Part) ->
    acl_path(Type,AuthzId) ++ "/" ++ Part.

acl_auth_path(Type, AuthzId, RequestorId) ->
    acl_path(Type, AuthzId) ++ "/grant/actors/" ++ binary_to_list(RequestorId).

type_to_resource(actor) ->
    "actors";
type_to_resource(container) ->
    "containers";
type_to_resource(group) ->
    "groups";
type_to_resource(object) ->
    "objects".
