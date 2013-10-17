%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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
         fetch_group_authz_id/3,
         make_context/2,
         statements/1
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-include("oc_chef_authz.hrl").
-include("oc_chef_authz_db.hrl").
-include_lib("sqerl/include/sqerl.hrl").

-define(gv(Key, PList), proplists:get_value(Key, PList)).
-define(user_db, "opscode_account").
-define(auth_join_db, "opscode_account").

statements(pgsql) ->
    [
     {find_container_by_orgid_name,
      <<"SELECT id, authz_id, org_id, name, last_updated_by, created_at, updated_at"
        " FROM containers "
        " WHERE (org_id = $1 AND name = $2) LIMIT 1">>},
     {find_group_by_orgid_name,
      <<"SELECT id, authz_id, org_id, name, last_updated_by, created_at, updated_at"
        " FROM groups"
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
        " ($1, $2, $3, $4, $5, $6, $7)">>}
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

-spec make_context(binary(), term()) -> #oc_chef_authz_context{}.
make_context(ReqId, Darklaunch) when is_binary(ReqId) ->
    Host = envy:get(oc_chef_authz, couchdb_host, string),
    Port = envy:get(oc_chef_authz, couchdb_port, pos_integer),
    S = couchbeam:server_connection(Host, Port, "", []),
    #oc_chef_authz_context{reqid = ReqId,
                           otto_connection = S,
                           darklaunch = Darklaunch}.

-spec fetch_container(oc_chef_authz_context(),
                      object_id(),
                      container_name()) -> #chef_container{} |
                                           not_found |
                                           {error, _}.
fetch_container(#oc_chef_authz_context{otto_connection=Server,
                                       darklaunch = Darklaunch} = Ctx,
                OrgId, ContainerName) ->
    case xdarklaunch_req:is_enabled(<<"couchdb_containers">>, Darklaunch) of
        true ->
            fetch_container_couchdb(Server, OrgId, ContainerName);
        false ->
            fetch_container_sql(Ctx, OrgId, ContainerName)
    end.

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
                           OrgId :: binary(),
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
                    binary() | 'not_found',
                    binary() | string(),
                    authz_type() | atom()) ->
                           {ok, [{binary(), _}]} | {'not_found', atom() | 'org'}.
%% @doc Fetch from the mixlib-authz records in couchdb
%%
fetch_by_name(_Server, not_found, _Name, _Type) ->
    {not_found, org};
fetch_by_name(Server, OrgId, Name, Type) when is_list(Name), is_binary(OrgId) ->
    fetch_by_name(Server, OrgId, list_to_binary(Name), Type);
fetch_by_name(Server, OrgId, Name, Type) when is_binary(Name), is_binary(OrgId) ->
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
%% design_and_view_for_type(authz_cookbook) ->
%%     {?mixlib_auth_container_design, "by_display_name"};
%% design_and_view_for_type(authz_data_bag) ->
%%     {?mixlib_auth_data_bag_design, "by_name"};
%% design_and_view_for_type(authz_environment) ->
%%     {?mixlib_auth_environment_design, "all_id"};
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

-spec dbname(binary()) -> <<_:40,_:_*8>>.
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
                                  chef_sql:fetch(#oc_chef_group{
                                                   org_id = OrgId,
                                                   name = Name})
                          end) of
        #oc_chef_group{authz_id = AuthzId} ->
            AuthzId;
        not_found ->
            {not_found, authz_group};
        {error, _} = Error ->
            Error
    end.
