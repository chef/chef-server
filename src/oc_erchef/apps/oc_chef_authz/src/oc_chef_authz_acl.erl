%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%%
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

-include("chef_types.hrl").
-include("oc_chef_types.hrl").

-export([acl_path/2,
         acl_path/3,
         acl_auth_path/3,
         fetch_id/4,
         fetch_cookbook_id/3,
         fetch/3,
         fetch/4,
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

-spec acl_spec(binary()) -> ejson_term().
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

-spec update_part(string(), ejson_term(), chef_type() | chef_authz_type(), id(), id())->
    {ok, ejson_term()}.
update_part(Part, AceRecord, Type, AuthzId, OrgId) ->
    Ids = names_to_ids(ej:get({Part}, AceRecord), OrgId),
    Data = chef_json:encode(Ids),
    Path = acl_path(Type, AuthzId, Part),
    Result = oc_chef_authz_http:request(Path, put, ?DEFAULT_HEADERS, Data, superuser_id()),
    case Result of
        {error, forbidden} ->
            throw(forbidden);
        Other ->
            Other
    end.

% TODO: we only need the authz id, so grabbing complete objects is wasteful.
% Also, this might be more suited to be moved to oc_chef_wm_util or
% something. In the meantime, this gets us up and running.
-spec fetch_id(chef_type(), term(), binary(), id()) -> not_found | id().
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

-spec fetch_cookbook_id(term(), binary(), id()) -> not_found | id().
fetch_cookbook_id(DbContext, Name, OrgId) ->
    % cookbook endpoint pattern is utterly different from the others, generic
    % fetch does not handle cookbooks (and, well, versioning)
    case chef_db:fetch_latest_cookbook_version(DbContext, OrgId, Name) of
        not_found ->
            not_found;
        #chef_cookbook_version{authz_id = AuthzId} ->
            AuthzId
    end.

-spec fetch(chef_type(), binary(), id()) -> list() | {error, term()}.
fetch(Type, OrgId, AuthzId) ->
    fetch(Type, OrgId, AuthzId, undefined).

fetch(Type, OrgId, AuthzId, Granular) ->
    Path = acl_path(Type, AuthzId),
    Result = oc_chef_authz_http:request(Path, get, ?DEFAULT_HEADERS, [], superuser_id()),
    case Result of
        {ok, Record} ->
            convert_all_ids_to_names(OrgId, Record, Granular);
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
-spec has_grant_on(chef_type() | chef_authz_type(), id(), id()) ->
    true | false | {error, term()}.
has_grant_on(ObjectType, ObjectId, ActorId) ->
    Path = acl_auth_path(ObjectType, ObjectId, ActorId),
    Check = oc_chef_authz_http:request(Path, get, ?DEFAULT_HEADERS, [],
                                       superuser_id()),
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
    Context = oc_chef_authz_scoped_name:initialize_context(OrgId),
    ActorIds = case ej:get({<<"clients">>}, ACE) of
        undefined ->
            fetch_actors(Context, ej:get({<<"actors">>}, ACE));
        Clients ->
            safe_fetch_ids(user, Context, ej:get({<<"users">>}, ACE)) ++
            safe_fetch_ids(client, Context, Clients)
    end,
    GroupIds = safe_fetch_ids(group, Context, ej:get({<<"groups">>}, ACE)),

    % Though we split out clients/users, the authz API
    % requires a unified list of actor IDs:
    ACE1 = ej:set({<<"actors">>}, ACE, ActorIds),
    ej:set({<<"groups">>}, ACE1, GroupIds).

%% Retrieves authz records for the given set of names
%% and returns a list of authz ids.
%% If the list of retrieved names + IDs
%% does not match the list of names provided, this raises
%% {invalid, Type, MissingList}}


safe_fetch_ids(_Type, _Context, []) ->
    % Save the cost of a query for an empty list -
    % fairly common for users and to a lesser extent groups.
    [];
safe_fetch_ids(Type, Context, Names) ->
    case oc_chef_authz_scoped_name:names_to_authz_id(Type, Names, Context) of
        {AuthzIds, []} ->
            AuthzIds;
        {_, Errors} ->
            ErrorNames = lists:flatten([Name || {_, Name} <- Errors ]),
            throw({invalid, Type, ErrorNames})
    end.

fetch_actors(Context, ActorNames) ->
    case oc_chef_authz_scoped_name:names_to_authz_id(actor, ActorNames, Context) of
        {AuthzIds, []} ->
            AuthzIds;
        {_, [{Error, Names} | _]}
          when Error =:= ill_formed_name orelse
               Error =:= inappropriate_scoped_name orelse
               Error =:= orgname_not_found orelse
               Error =:= not_found ->
            throw({bad_actor, Names});
        {_, [{ambiguous, Names} | _]} ->
            throw({ambiguous_actor, Names})
    end.


%%
%% Reverse mapping of ids to names (this should have a lot of commonality with groups)
%%
convert_all_ids_to_names(OrgId, Record, Granular) ->
    convert_ids_to_names_in_part([<<"create">>, <<"read">>, <<"update">>,
                                  <<"delete">>, <<"grant">>],
                                 OrgId, Record, Granular).

-spec convert_ids_to_names_in_part(list(binary()), binary(), ejson_term(), granular|undefined) -> ejson_term().
convert_ids_to_names_in_part([], _OrgId, Record, _Granular) ->
    Record;
convert_ids_to_names_in_part([Part | Rest], OrgId, Record, Granular) ->
    Members = ej:get({Part}, Record),
    ActorIds = ej:get({<<"actors">>}, Members),
    GroupIds = ej:get({<<"groups">>}, Members),
    Context = oc_chef_authz_scoped_name:initialize_context(OrgId),
    {ClientNames, UserNames, GroupNames} = oc_chef_authz_scoped_name:convert_ids_to_names(ActorIds, GroupIds, Context),
    Members1 = part_with_actors(Members, ClientNames, UserNames, Granular),
    Members2 = ej:set({<<"groups">>}, Members1, GroupNames),
    NewRecord = ej:set({Part}, Record, Members2),
    convert_ids_to_names_in_part(Rest, OrgId, NewRecord, Granular).

part_with_actors(PartRecord, Clients, Users, granular) ->
    PartRecord0 = ej:set({<<"users">>}, PartRecord, Users),
    PartRecord1 = ej:set({<<"clients">>}, PartRecord0, Clients),
    ej:set({<<"actors">>}, PartRecord1, []);
part_with_actors(PartRecord, Clients, Users, _) ->
    ej:set({<<"actors">>}, PartRecord, Clients ++ Users).

superuser_id() ->
    {ok, Id} = chef_secrets:get(<<"oc_bifrost">>, <<"superuser_id">>),
    Id.

% Path helper functions
% Translate types; in ACLs, everything is an object, actor, group, or container

-spec acl_path(chef_type() | chef_authz_type(), id() ) -> string().
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


-spec acl_path(chef_type() | chef_authz_type(), id(), string() ) -> string().
acl_path(Type, AuthzId, Part) ->
    acl_path(Type,AuthzId) ++ "/" ++ Part.

-spec acl_auth_path(chef_type() | chef_authz_type(), id(), binary() ) -> string().
acl_auth_path(Type, AuthzId, RequestorId) ->
    acl_path(Type, AuthzId) ++ "/grant/actors/" ++ binary_to_list(RequestorId).

-spec type_to_resource(chef_authz_type()) -> string().
type_to_resource(actor) ->
    "actors";
type_to_resource(container) ->
    "containers";
type_to_resource(group) ->
    "groups";
type_to_resource(object) ->
    "objects".
