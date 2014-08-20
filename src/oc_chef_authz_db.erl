%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @author Marc Paradise <marc@getchef.com>
%% @doc authorization - Interface to the opscode authorization servize
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

-module(oc_chef_authz_db).

-include_lib("oc_chef_authz/include/oc_chef_types.hrl").

-export([container_record_to_authz_id/2,
         fetch_container/3,
         fetch_global_group_authz_id/3,
         fetch_group_authz_id/3,
         make_context/2,
         statements/1
        ]).

%-ifdef(TEST).
-compile([export_all]).
%-endif.

-include("oc_chef_authz.hrl").
-include("oc_chef_authz_db.hrl").
-include_lib("sqerl/include/sqerl.hrl").

-define(gv(Key, PList), proplists:get_value(Key, PList)).
-define(user_db, "opscode_account").
-define(auth_join_db, "opscode_account").

statements(pgsql) ->
    [
     {user_in_org,
      <<"SELECT count(*)"
        "  FROM users u, org_user_associations assoc, orgs o"
        " WHERE u.id = assoc.user_id AND o.id = assoc.org_id"
        "   AND u.username = $1 AND o.name = $2;">>},

     % Org-user invites
     {insert_org_user_invite,
      <<"INSERT INTO org_user_invites (id, org_id, user_id, last_updated_by, created_at, updated_at)"
        " VALUES ($1, $2, $3, $4, $5, $6)">>},
     {delete_org_user_invite_by_id, <<"DELETE FROM org_user_invites WHERE id= $1">>},
     {find_org_user_invite_by_id, <<"SELECT i.id id, o.id as org_id, o.name as org_name, u.id as user_id, u.username as user_name, i.last_updated_by, i.created_at, i.updated_at "
                                    "  FROM org_user_invites i, orgs o, users u "
                                    " WHERE i.id = $1 "
                                    "   AND user_id = u.id "
                                    "   AND org_id = o.id ">>},
     {list_org_user_invites , <<"SELECT i.id as id, u.username as user_name "
                                "  FROM org_user_invites i, users u "
                                " WHERE i.org_id = $1 "
                                "   AND i.user_id = u.id "
                                " ORDER BY user_name">>},
     {list_user_org_invites, <<"SELECT i.id as id, o.name as org_name "
                                "  FROM org_user_invites i, orgs o, users u "
                                " WHERE i.user_id = $1 "
                                "   AND i.org_id = o.id "
                                "   AND u.id = i.user_id "
                                " ORDER BY org_name">>},
     % Org-user associations
     {insert_org_user_association,
      <<"INSERT INTO org_user_associations (org_id, user_id, last_updated_by, created_at, updated_at)"
        " VALUES ($1, $2, $3, $4, $5)">>},
     {delete_org_user_association_by_ids,
      <<"DELETE FROM org_user_associations WHERE org_id= $1 AND user_id= $2">>},
     {find_org_user_association_by_ids,
      <<"SELECT org_id, user_id, username as user_name, a.last_updated_by, a.created_at, a.updated_at"
        "  FROM org_user_associations a, users u"
        " WHERE org_id= $1 AND user_id= $2 AND a.user_id = u.id">>},

     {list_user_org_associations, <<"SELECT o.name as name, o.full_name as full_name "
                                    "  FROM org_user_associations a, orgs o"
                                    " WHERE a.org_id = o.id "
                                    "   AND user_id = $1">>},

     % Note here that because an org association isn't an 'object' per se, the form is different -
     % we just need a list of names.
     {list_org_user_associations, <<"   SELECT username as user_name"
                                    "     FROM org_user_associations a, users u"
                                    "    WHERE org_id = $1 AND a.user_id = u.id"
                                    " ORDER BY user_name">>},

     {insert_organization,
      <<"INSERT INTO orgs (id, authz_id, name, full_name,"
        " assigned_at, last_updated_by, created_at, updated_at) VALUES"
        " ($1, $2, $3, $4, $5, $6, $7, $8)">>},
     {update_organization_by_id,
      <<"UPDATE orgs SET last_updated_by= $1, updated_at= $2, name= $3, full_name= $4"
        "WHERE id= $5">>},
     {delete_organization_by_id, <<"DELETE FROM orgs WHERE id= $1">>},
     {find_organization_by_id,
      <<"SELECT id, authz_id, name, full_name, assigned_at, last_updated_by, created_at, updated_at"
        " FROM orgs "
        " WHERE id= $1 LIMIT 1">>},
     {find_organization_by_name,
      <<"SELECT id, authz_id, name, full_name, assigned_at, last_updated_by, created_at, updated_at"
        " FROM orgs "
        " WHERE name= $1 LIMIT 1">>},
     {list_organizations, <<"SELECT name FROM orgs">>},
     {find_container_by_orgid_name,
      <<"SELECT id, authz_id, org_id, name, last_updated_by, created_at, updated_at"
        " FROM containers "
        " WHERE (org_id = $1 AND name = $2) LIMIT 1">>},
     {insert_container,
      <<"INSERT INTO containers (id, authz_id, org_id, name,"
        " last_updated_by, created_at, updated_at) VALUES"
        " ($1, $2, $3, $4, $5, $6, $7)">>},
     {update_container_by_id,
      <<"UPDATE containers SET last_updated_by= $1, updated_at= $2, name= $3"
        "WHERE id= $4">>},
     {delete_container_by_id, <<"DELETE FROM containers WHERE id= $1">>},
     {list_containers_for_org, <<"SELECT name FROM containers WHERE org_id= $1">>},
     {list_groups_for_org, <<"SELECT name FROM groups WHERE org_id= $1">>},
     {find_group_by_orgid_name,
      <<"SELECT id, authz_id, org_id, name, last_updated_by, created_at, updated_at"
        " FROM groups"
        " WHERE (org_id = $1 AND name = $2) LIMIT 1">>},
     {insert_group,
      <<"INSERT INTO groups (id, authz_id, org_id, name,"
        " last_updated_by, created_at, updated_at) VALUES"
        " ($1, $2, $3, $4, $5, $6, $7)">>},
     {update_group_by_id,
      <<"UPDATE groups SET last_updated_by= $1, updated_at= $2, name= $3"
        "WHERE id= $4">>},
     {delete_group_by_id, <<"DELETE FROM groups WHERE id= $1">>},
     {find_client_name_in_authz_ids,
      <<"SELECT name, authz_id FROM clients WHERE authz_id = ANY($1)">>},
     {find_client_authz_id_in_names,
      <<"SELECT authz_id FROM clients WHERE org_id = $1 AND name = ANY($2)">>},
     {find_user_name_in_authz_ids,
      <<"SELECT username, authz_id FROM users WHERE authz_id = ANY($1)">>},
     {find_user_authz_id_in_names,
      <<"SELECT authz_id FROM users WHERE username = ANY($1)">>},
     {find_group_name_in_authz_ids,
      <<"SELECT name, authz_id FROM groups WHERE authz_id = ANY($1)">>},
     {find_group_authz_id_in_names,
      <<"SELECT authz_id FROM groups WHERE org_id = $1 AND name = ANY($2)">>}
    ].
%
% Opscode Chef_views.
%
-define(mixlib_auth_client_design,
        "Mixlib::Authorization::Models::Client-fec21b157b76e08b86e92ef7cbc2be81").

-define(mixlib_auth_container_design,
        "Mixlib::Authorization::Models::Container-f6aead5acfa18f649f9f951ad5570324").

-define(mixlib_auth_cookbook_design,
        "Mixlib::Authorization::Models::Cookbook-98638da9d7bfb86c50d448361c5ce691").

-define(mixlib_auth_data_bag_design,
        "Mixlib::Authorization::Models::DataBag-ff80eed5150b3cf59c54630503d83bb5").

-define(mixlib_auth_environment_design,
        "Mixlib::Authorization::Models::Environment-bd1a09cec7a4655bf54f4b98ec5ec156").

-define(mixlib_auth_group_design,
        "Mixlib::Authorization::Models::Group-59a505c964199e318b67910cc642a062").

-define(mixlib_auth_node_design,
        "Mixlib::Authorization::Models::Node-8554f173ac3e9bfa55a0836e19b0f232").

-define(mixlib_auth_role_design,
        "Mixlib::Authorization::Models::Role-793b383e56e849fca5901cd66b92bde7").

-define(mixlib_auth_join_design,
        "Mixlib::Authorization::AuthJoin-25834c5a8d6a9586adb05320f3f725e8").

-define(client_design, "clients").
-define(cookbook_design, "cookbooks").
-define(data_bag_design, "data_bags").
-define(data_bag_item_design, "data_bag_items").
-define(environment_design, "environments").
-define(node_design, "nodes").
-define(role_design, "roles").
-define(user_design, "users").

-spec make_context(binary(), term()) -> #oc_chef_authz_context{}.
make_context(ReqId, Darklaunch) when is_binary(ReqId) ->
    Host = envy:get(oc_chef_authz, couchdb_host, string),
    Port = envy:get(oc_chef_authz, couchdb_port, pos_integer),
    S = couchbeam:server_connection(Host, Port, "", []),
    #oc_chef_authz_context{reqid = ReqId,
                           otto_connection = S,
                           darklaunch = Darklaunch}.

-spec fetch_container(oc_chef_authz_context(),
                      object_id() | undefined,
                      container_name()) -> #chef_container{} |
                                           not_found |
                                           {error, _}.
fetch_container(#oc_chef_authz_context{otto_connection = Server},
                undefined, ContainerName) ->
    % Containers with no org are global containers - these have not yet been migrated
    % so force them to couch regardless of darklaunch.
    fetch_container_couchdb(Server, undefined, ContainerName);
fetch_container(#oc_chef_authz_context{otto_connection=Server,
                                       darklaunch = Darklaunch} = Ctx,
                OrgId, ContainerName) ->
    case xdarklaunch_req:is_enabled(<<"couchdb_containers">>, Darklaunch) of
        true ->
            fetch_container_couchdb(Server, OrgId, ContainerName);
        false ->
            fetch_container_sql(Ctx, OrgId, ContainerName)
    end.


fetch_global_group_authz_id(#oc_chef_authz_context{otto_connection=Server, darklaunch = _Darklaunch} = _C,
                   OrgName, GroupName) ->
    RealGroupName = lists:flatten(io_lib:format("~s_~s", [OrgName, GroupName])),
    fetch_group_authz_id_couchdb(Server, undefined, RealGroupName).

fetch_container_couchdb(Server, OrgId, ContainerName) ->
    case fetch_by_name(Server, OrgId, ContainerName, authz_container) of
        {ok, Container} ->
            Id = ej:get({<<"_id">>}, Container),
            AuthzId = fetch_auth_join_id(Server, Id, user_to_auth),
            Name = ej:get({<<"containername">>}, Container),
            Path = ej:get({<<"containerpath">>}, Container),
            Updated =  ej:get({<<"requester_id">>}, Container),
            #chef_container{id = Id,
                            authz_id = AuthzId,
                            org_id = OrgId,
                            name = Name,
                            path = Path,
                            last_updated_by = Updated
                           };
        {not_found, _} ->
            not_found
    end.

%% @doc Retrieve the authz ID for a given group in an organaization.
%%
%% This is not `fetch_group`, because currently, we have no need for
%% complete group "objects" in the system.  This is only really used
%% to retrieve the clients group, so that we may add newly-created
%% clients to it (we could also start deleting clients from it, too).
-spec fetch_group_authz_id(Context :: oc_chef_authz_context(),
                           OrgId :: binary() | undefined,
                           GroupName :: binary()) ->  object_id() |
                                                      {not_found, authz_group}.
fetch_group_authz_id(#oc_chef_authz_context{otto_connection=Server,
                                            darklaunch = Darklaunch} = Ctx,
                     OrgId, GroupName) ->
    case xdarklaunch_req:is_enabled(<<"couchdb_groups">>, Darklaunch) of
        true ->
            fetch_group_authz_id_couchdb(Server, OrgId, GroupName);
        false ->
            fetch_group_authz_id_sql(Ctx, OrgId, GroupName)
    end.

fetch_group_authz_id_couchdb(Server, OrgId, GroupName) ->
    case fetch_by_name(Server, OrgId, GroupName, authz_group) of
        {ok, Group} ->
            Id = ej:get({<<"_id">>}, Group),
            AuthzId = fetch_auth_join_id(Server, Id, user_to_auth),
            AuthzId;
        {not_found, authz_group} ->
            {not_found, authz_group}
    end.

-spec container_record_to_authz_id(any(), any()) -> object_id().
container_record_to_authz_id(#oc_chef_authz_context{}, #chef_container{authz_id = Id}) ->
    Id.

-spec fetch_by_name(couchbeam:server(),
                    binary() | 'not_found' | undefined,
                    binary() | string(),
                    authz_type() | atom()) ->
                           {ok, [{binary(), _}]} | {'not_found', atom() | 'org'}.
%% @doc Fetch from the mixlib-authz records in couchdb
%%
fetch_by_name(_Server, not_found, _Name, _Type) ->
    {not_found, org};
fetch_by_name(Server, OrgId, Name, Type) when is_list(Name) ->
    fetch_by_name(Server, OrgId, list_to_binary(Name), Type);
fetch_by_name(Server, OrgId, Name, Type) when is_binary(Name) andalso (is_binary(OrgId)
                                                                       orelse OrgId =:= undefined) ->
    {Design, ViewName} = design_and_view_for_type(Type),
    ChefDb = dbname(OrgId),
    {ok, Db} = couchbeam:open_db(Server, ChefDb, []),
    {ok, View} = couchbeam:view(Db, {Design, ViewName}, [{key, Name}]),
    case couchbeam_view:first(View) of
        {ok, {Row}} ->
            Id = ?gv(<<"id">>, Row),
            case couchbeam:open_doc(Db, Id) of
                {error, not_found} -> {not_found, Type};
                %% FIXME: why are we unpacking the ejson format?
                {ok, {Doc}} -> {ok, Doc}
            end;
        {ok, []} -> {not_found, Type};
        {error, not_found} -> {not_found, Type}
    end.

-spec fetch_auth_join_id(couchbeam:server(), db_key(), auth_to_user|user_to_auth) -> binary() | {not_found, term()}.
fetch_auth_join_id(Server, Id, Direction) when is_list(Id) ->
    fetch_auth_join_id(Server, list_to_binary(Id), Direction);
fetch_auth_join_id(Server, Id, Direction) when is_binary(Id) ->
    {FieldName, ViewName} =
        case Direction of
%%            auth_to_user -> { <<"user_object_id">>, "by_auth_object_id"};
            user_to_auth -> { <<"auth_object_id">>, "by_user_object_id"}
        end,
    {ok, Db} = couchbeam:open_db(Server, ?auth_join_db, []),
    {ok, View} = couchbeam:view(Db, {?mixlib_auth_join_design, ViewName},
                                [{key, Id}, {include_docs, true}]),
    case couchbeam_view:first(View) of
        {ok, []} -> {not_found, missing};
        {ok, Row} -> ej:get({<<"doc">>, FieldName}, Row);
        Why -> {not_found, Why}
    end.

%% @doc Return the CouchDB design doc and view name for `Type`
%%
%% design_and_view_for_type(authz_client) ->
%%     {?mixlib_auth_client_design, "by_clientname"};
design_and_view_for_type(authz_container) ->
    {?mixlib_auth_container_design, "by_containername"};
design_and_view_for_type(authz_group) ->
    {?mixlib_auth_group_design, "by_groupname"}.
%% design_and_view_for_type(authz_node) ->
%%     {?mixlib_auth_node_design, "by_name"};
%% design_and_view_for_type(authz_role) ->
%%     {?mixlib_auth_role_design, "by_name"};
%% design_and_view_for_type(AppType) ->
%%     design_and_view_for_app_type(AppType).

%% design_and_view_for_app_type(chef_client) ->
%%     {?client_design, "all_id"};
%% design_and_view_for_app_type(chef_data_bag) ->
%%     {?data_bag_design, "all_id"};
%% design_and_view_for_app_type(chef_data_bag_item) ->
%%     {?data_bag_item_design, "all_id"};
%% design_and_view_for_app_type(chef_environment) ->
%%     {?environment_design, "all_id"};
%% design_and_view_for_app_type(chef_node) ->
%%     {?node_design, "all_id"};
%% design_and_view_for_app_type(chef_role) ->
%%     {?role_design, "all_id"}.

-spec dbname(binary()|undefined) -> <<_:40,_:_*8>>.
% If org id is not provided, then the DB returned is the account db.
dbname(undefined) ->
    <<"opscode_account">>;
dbname(OrgId) ->
    <<"chef_", OrgId/binary>>.

-spec fetch_container_sql(#oc_chef_authz_context{}, binary(), binary()) -> #chef_container{} |
                                                                           not_found |
                                                                           {error, _}.
fetch_container_sql(#oc_chef_authz_context{reqid = ReqId}, OrgId, Name) ->
    %% since ?FIRST uses record_info, it can't be placed within the fun.
    Transform = ?FIRST(chef_container),
    case stats_hero:ctime(ReqId,
                          %% aggregate perf timing with other sql queries
                          {chef_sql, fetch_container_sql},
                          fun() ->
                                  sqerl:select(find_container_by_orgid_name, [OrgId, Name], Transform)
                          end) of
        {ok, #chef_container{} = C} ->
            C;
        {ok, none} ->
            not_found;
        {error, Error} ->
            {error, Error}
    end.
-spec fetch_group_authz_id_sql(Context :: oc_chef_authz_context(),
                               OrgId :: binary(),
                               GroupName :: binary()) ->  object_id() |
                                                          {not_found, authz_group} |
                                                          {error, _}.
fetch_group_authz_id_sql(#oc_chef_authz_context{reqid = ReqId}, OrgId, Name) ->
    %% since ?FIRST uses record_info, it can't be placed within the fun.
    case stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_object:default_fetch(#oc_chef_group{
                                                    org_id = OrgId,
                                                    name = Name},
                                                    fun chef_sql:select_rows/1)
                          end) of
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId;
        not_found ->
            {not_found, authz_group};
        {error, _} = Error ->
            Error
    end.
