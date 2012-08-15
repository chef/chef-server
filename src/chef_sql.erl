%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Mark Anderson <mark@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author James Casey <james@opscode.com>
%% @author Mark Mzyk <mmzyk@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_sql).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.


-export([fetch_user/1,
         %% checksum ops
         mark_checksums_as_uploaded/2,
         non_uploaded_checksums/2,

         %% data_bag ops
         fetch_data_bag/2,
         fetch_data_bags/1,
         create_data_bag/1,
         delete_data_bag/1,

         %% data_bag_item ops
         fetch_data_bag_item/3,
         fetch_data_bag_items/2,
         fetch_data_bag_item_ids/2,
         bulk_get_data_bag_items/1,
         create_data_bag_item/1,
         delete_data_bag_item/1,
         update_data_bag_item/1,

         %% environment ops
         fetch_environment/2,
         fetch_environments/1,
         bulk_get_environments/1,
         create_environment/1,
         delete_environment/1,
         update_environment/1,

         %% client ops
         fetch_client/2,
         fetch_clients/1,
         bulk_get_clients/1,
         create_client/1,
         delete_client/1,
         update_client/1,

         %% node ops
         fetch_node/2,
         fetch_nodes/1,
         fetch_nodes/2,
         bulk_get_nodes/1,
         create_node/1,
         delete_node/1,
         update_node/1,
         %% role ops
         fetch_role/2,
         fetch_roles/1,
         bulk_get_roles/1,
         create_role/1,
         delete_role/1,
         update_role/1,

         %% cookbook version ops
         cookbook_exists/2,

         fetch_cookbook_version/2,
         fetch_cookbook_versions/1,
         fetch_latest_cookbook_version/2,
         fetch_latest_cookbook_recipes/1,
         create_cookbook_version/1,
         update_cookbook_version/1,
         delete_cookbook_version/1,
         fetch_all_cookbook_version_dependencies/1,
         fetch_environment_filtered_cookbook_versions/4,

         %% Sandbox Ops
         create_sandbox/1,
         fetch_sandbox/2,
         delete_sandbox/1,

         sql_now/0,
         ping/0,
         statements/1
        ]).

-include_lib("sqerl/include/sqerl.hrl").
-include_lib("chef_objects/include/chef_types.hrl").

-type delete_query() :: delete_cookbook_version_by_id |
                        delete_data_bag_by_id |
                        delete_data_bag_item_by_id |
                        delete_environment_by_id |
                        delete_client_by_id |
                        delete_node_by_id |
                        delete_role_by_id |
                        delete_sandbox_by_id.

sql_now() -> calendar:now_to_universal_time(os:timestamp()).

-spec ping() -> pong | pang.
ping() ->
    try
        case sqerl:select(ping, [], rows_as_scalars, [ping]) of
            {ok, [<<"pong">>]} -> pong;
            _Else -> throw(pang)
        end
    catch
        How:Why ->
            error_logger:error_report({chef_sql, ping, How, Why}),
            pang
    end.

-spec fetch_user(bin_or_string()) -> {ok, #chef_user{} | not_found } | {error, term()}.
%% @doc Return user data for the given username
%%
%%
fetch_user(UserName) ->
    case sqerl:select(find_by_username_for_authn, [UserName], ?FIRST(chef_user)) of
        {ok, none} ->
            {ok, not_found};
        {ok, #chef_user{}=User} ->
            {ok, User};
        {error, Error} -> {error, Error}
    end.

%% node ops

-spec fetch_node(bin_or_string(), bin_or_string()) -> {ok, #chef_node{} | not_found} | {error, term()}.
fetch_node(OrgId, NodeName) ->
    fetch_object(OrgId, NodeName, chef_node).

-spec fetch_nodes(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%% @doc Return list of node names for a given organization
fetch_nodes(OrgId) ->
    fetch_objects(OrgId, chef_node).

-spec fetch_nodes(bin_or_string(), bin_or_string()) -> {ok, [binary()] | not_found} |
                                                       {error, term()}.
%% @doc Return list of node names for given organization and environment
fetch_nodes(OrgId, EnvName) ->
    case sqerl:select(list_env_nodes_for_org, [OrgId, EnvName], rows_as_scalars, [name]) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

-spec create_node(#chef_node{}) -> {ok, 1} | {error, term()}.
create_node(#chef_node{}=Node) ->
    create_object(Node).

-spec delete_node(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_node(NodeId) when is_list(NodeId) ->
    delete_node(list_to_binary(NodeId));
delete_node(#chef_node{id=NodeId})->
    delete_node(NodeId);
delete_node(NodeId) when is_binary(NodeId) ->
    delete_object(delete_node_by_id, NodeId).

-spec update_node(#chef_node{}) -> {ok, 1 | not_found} | {error, term()}.
update_node(#chef_node{environment = Environment,
                       last_updated_by = LastUpdatedBy,
                       updated_at = UpdatedAt,
                       serialized_object = Object,
                       id = Id}) ->
    UpdateFields = [Environment, LastUpdatedBy, UpdatedAt, Object, Id],
    do_update(update_node_by_id, UpdateFields).

-spec bulk_get_nodes([binary()]) -> {ok, [binary()] | not_found} |
                                    {error, term()}.
bulk_get_nodes(Ids) ->
    bulk_get_objects(node, Ids).

%% role ops

-spec fetch_role(bin_or_string(), bin_or_string()) ->
                        {ok, #chef_role{} | not_found} | {error, term()}.
fetch_role(OrgId, RoleName) ->
    fetch_object(OrgId, RoleName, chef_role).

-spec fetch_roles(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%% @doc Return list of role names for a given organization
fetch_roles(OrgId) ->
    fetch_objects(OrgId, chef_role).

-spec bulk_get_roles([binary()]) -> {ok, [binary()] | not_found} |
                                    {error, term()}.
bulk_get_roles(Ids) ->
    bulk_get_objects(role, Ids).

-spec create_role(#chef_role{}) -> {ok, 1} | {error, term()}.
create_role(#chef_role{}=Role) ->
    create_object(Role).

-spec delete_role(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_role(RoleId) when is_list(RoleId) ->
    delete_role(list_to_binary(RoleId));
delete_role(#chef_role{id=RoleId})->
    delete_role(RoleId);
delete_role(RoleId) when is_binary(RoleId) ->
    delete_object(delete_role_by_id, RoleId).

-spec update_role(#chef_role{}) -> {ok, 1 | not_found} | {error, term()}.
update_role(#chef_role{last_updated_by = LastUpdatedBy,
                       updated_at = UpdatedAt,
                       serialized_object = Object,
                       id = Id}) ->
    UpdateFields = [LastUpdatedBy, UpdatedAt, Object, Id],
    do_update(update_role_by_id, UpdateFields).

%% data_bag_item ops

-spec fetch_data_bag_item(bin_or_string(), bin_or_string(), bin_or_string()) ->
                        {ok, #chef_data_bag_item{} | not_found} | {error, term()}.
fetch_data_bag_item(OrgId, DataBagName, DataBagItemName) ->
    %% This is coded following fetch_object in the hopes of factoring it back in when we
    %% have another case that differs in arity. Perhaps cookbook versions.
    RecordName = chef_data_bag_item,
    {QueryName, FirstRecordTxfm} = query_and_txfm_for_record(fetch, RecordName),
    case sqerl:select(QueryName, [OrgId, DataBagName, DataBagItemName], FirstRecordTxfm) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when RecordName =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_data_bag_items(bin_or_string(), bin_or_string()) ->
                                  {ok, [binary()]} | {error, term()}.
%% @doc Return list of data_bag_item names for a given organization
fetch_data_bag_items(OrgId, DataBagName) ->
    %% Again, coding this following fetch_objects in hopes of future refactoring
    RecordName = chef_data_bag_item,
    QueryName = list_query_for(RecordName),
    case sqerl:select(QueryName, [OrgId, DataBagName], rows_as_scalars, [item_name]) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_data_bag_item_ids(bin_or_string(), bin_or_string()) ->
                                     {ok, [binary()]} | {error, term()}.
%% @doc Return list of data_bag_item names for a given organization
fetch_data_bag_item_ids(OrgId, DataBagName) ->
    %% Again, coding this following fetch_objects in hopes of future refactoring
    QueryName = find_data_bag_item_id_by_orgid_name,
    case sqerl:select(QueryName, [OrgId, DataBagName], rows_as_scalars, [id]) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

-spec bulk_get_data_bag_items([binary()]) -> {ok, [binary()] | not_found} |
                                             {error, term()}.
bulk_get_data_bag_items(Ids) ->
    bulk_get_objects(data_bag_item, Ids).

-spec create_data_bag_item(#chef_data_bag_item{}) -> {ok, 1} | {error, term()}.
create_data_bag_item(#chef_data_bag_item{}=DataBagItem) ->
    create_object(DataBagItem).

-spec delete_data_bag_item(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_data_bag_item(DataBagItemId) when is_list(DataBagItemId) ->
    delete_data_bag_item(list_to_binary(DataBagItemId));
delete_data_bag_item(#chef_data_bag_item{id=DataBagItemId})->
    delete_data_bag_item(DataBagItemId);
delete_data_bag_item(DataBagItemId) when is_binary(DataBagItemId) ->
    delete_object(delete_data_bag_item_by_id, DataBagItemId).

-spec update_data_bag_item(#chef_data_bag_item{}) -> {ok, 1 | not_found} | {error, term()}.
update_data_bag_item(#chef_data_bag_item{last_updated_by = LastUpdatedBy,
                       updated_at = UpdatedAt,
                       serialized_object = Object,
                       id = Id}) ->
    UpdateFields = [LastUpdatedBy, UpdatedAt, Object, Id],
    do_update(update_data_bag_item_by_id, UpdateFields).

%% environment ops

-spec fetch_environment(bin_or_string(), bin_or_string()) ->
                        {ok, #chef_environment{} | not_found} | {error, term()}.
fetch_environment(OrgId, EnvironmentName) ->
    fetch_object(OrgId, EnvironmentName, chef_environment).

-spec fetch_environments(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%% @doc Return list of environment names for a given organization
fetch_environments(OrgId) ->
    fetch_objects(OrgId, chef_environment).

-spec bulk_get_environments([binary()]) -> {ok, [binary()] | not_found} |
                                           {error, term()}.
bulk_get_environments(Ids) ->
    bulk_get_objects(environment, Ids).

-spec create_environment(#chef_environment{}) -> {ok, 1} | {error, term()}.
create_environment(#chef_environment{}=Environment) ->
    create_object(Environment).

-spec delete_environment(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_environment(EnvironmentId) when is_list(EnvironmentId) ->
    delete_environment(list_to_binary(EnvironmentId));
delete_environment(#chef_environment{id=EnvironmentId})->
    delete_environment(EnvironmentId);
delete_environment(EnvironmentId) when is_binary(EnvironmentId) ->
    delete_object(delete_environment_by_id, EnvironmentId).

-spec update_environment(#chef_environment{}) -> {ok, 1 | not_found} | {error, term()}.
update_environment(#chef_environment{last_updated_by = LastUpdatedBy,
                       updated_at = UpdatedAt,
                       name = Name,
                       serialized_object = Object,
                       id = Id}) ->
    UpdateFields = [LastUpdatedBy, UpdatedAt, Name, Object, Id],
    do_update(update_environment_by_id, UpdateFields).

%% client ops

-spec fetch_client(bin_or_string(), bin_or_string()) ->
                          {ok, #chef_client{} | not_found} | {error, term()}.
fetch_client(OrgId, ClientName) ->
    fetch_object(OrgId, ClientName, chef_client).

-spec fetch_clients(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%% @doc Return list of client names for a given organization
fetch_clients(OrgId) ->
    fetch_objects(OrgId, chef_client).

-spec bulk_get_clients([binary()]) -> {ok, [ [proplists:property()] ] | not_found} |
                                      {error, term()}.
%% The client table does not have a serialized_object field
%% so we need to construct a binary JSON representation of the table
%% for serving up in search results.
%%
%% Note this return a list of proplists, different from the other bulk_get_X
%% calls
bulk_get_clients(Ids) ->
    Query = bulk_get_query_for_count(client, length(Ids)),
    case sqerl:select(Query, Ids) of
        {ok, none} ->
            {ok, not_found};
        {ok, L} when is_list(L) ->
            {ok, L};
        {error, Error} ->
            {error, Error}
    end.

-spec create_client(#chef_client{}) -> {ok, 1} | {error, term()}.
create_client(#chef_client{}=Client) ->
    create_object(Client).

-spec delete_client(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_client(ClientId) when is_list(ClientId) ->
    delete_client(list_to_binary(ClientId));
delete_client(ClientId) when is_binary(ClientId) ->
    delete_object(delete_client_by_id, ClientId).

-spec update_client(#chef_client{}) -> {ok, 1 | not_found} | {error, term()}.
update_client(#chef_client{last_updated_by = LastUpdatedBy,
                           updated_at = UpdatedAt,
                           name = Name,
                           public_key = PublicKey,
                           pubkey_version = PubkeyVersion,
                           id = Id}) ->
    UpdateFields = [LastUpdatedBy, UpdatedAt, Name, PublicKey, PubkeyVersion, Id],
    do_update(update_client_by_id, UpdateFields).

%% data_bag ops

-spec fetch_data_bag(bin_or_string(), bin_or_string()) ->
                        {ok, #chef_data_bag{} | not_found} | {error, term()}.
fetch_data_bag(OrgId, DataBagName) ->
    fetch_object(OrgId, DataBagName, chef_data_bag).

-spec fetch_data_bags(bin_or_string()) -> {ok, [binary()]} | {error, term()}.
%% @doc Return list of data_bag names for a given organization
fetch_data_bags(OrgId) ->
    fetch_objects(OrgId, chef_data_bag).

-spec create_data_bag(#chef_data_bag{}) -> {ok, 1} | {error, term()}.
create_data_bag(#chef_data_bag{}=DataBag) ->
    create_object(DataBag).

-spec delete_data_bag(bin_or_string()) -> {ok, 1 | 'none' | 'not_found'} | {error, term()}.
delete_data_bag(DataBagId) when is_list(DataBagId) ->
    delete_data_bag(list_to_binary(DataBagId));
delete_data_bag(#chef_data_bag{id=DataBagId})->
    delete_data_bag(DataBagId);
delete_data_bag(DataBagId) when is_binary(DataBagId) ->
    delete_object(delete_data_bag_by_id, DataBagId).

-spec fetch_cookbook_versions(OrgId::object_id()) ->
    {ok, [versioned_cookbook()]} | {error, term()}.
%% @doc Return list of [cookbook name, version()] for a given organization.  The list is returned sort
%% by name, major, minor, patch fields.
fetch_cookbook_versions(OrgId) ->
    QueryName = list_cookbook_versions_by_orgid,
    case sqerl:select(QueryName, [OrgId], rows, []) of
        {ok, L} when is_list(L) ->
            {ok,
             [ [Name, triple_to_version_tuple(Major, Minor, Patch) ] || [Name, Major, Minor, Patch]  <- L]
            };
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Fetch up to `NumberOfVersions' most recent versions of each
%% cookbook within an organization.  Just returns a proplist mapping
%% cookbook name -> concatenated version (i.e., <<"1.0.0">> instead of
%% {1,0,0}).
%%
%% If `NumberOfVersions' is the atom `all', then information for all
%% versions of all cookbooks is returned.
%%
%% If the given `CookbookName' is the atom 'all', information
%% for all cookbooks is retrieved; otherwise only information for the
%% specified cookbook is returned.
-spec fetch_latest_cookbook_versions(OrgId :: object_id(),
                                     CookbookName :: binary() | all,
                                     NumberOfVersions :: num_versions()) ->
                                            {ok, [{CookbookName :: binary(), VersionString :: binary()}]} |
                                            {error, term()}.
fetch_latest_cookbook_versions(_OrgId, CookbookName, NumberOfVersions) when is_binary(CookbookName),
                                                                            is_integer(NumberOfVersions) ->
    %% This function head is solely to serve as an active guard
    %% against trying to grab less than all versions of a specific
    %% cookbook.  Due to the way the code and database are currently
    %% structured, that is actually completely unnecessary.
    %%
    %% We currently only need to get information about a single
    %% cookbook in the context of filtering through an environment,
    %% and we always need all versions of a cookbook for that.
    %%
    %% As things stand, the only
    %% time we should ever fetch "some versions of one cookbook" in
    %% this function is when we are sure that an environment would
    %% never have any constraints, and that can only happen with the
    %% "_default" environment.  However, this special case is not
    %% called extremely often, and would actually require special
    %% handling, including a new prepared statement (plus stored
    %% procedure on MySQL) specifically to handle this case.
    %%
    %% Thus this function head is a guard against problems that any
    %% potentially incomplete future refactoring may bring.  That is,
    %% if you ever actually see this error, you have broken something.
    throw({error, {invalid_combination, [CookbookName, NumberOfVersions]}});
fetch_latest_cookbook_versions(OrgId, CookbookName, NumberOfVersions) ->
    Result = case {CookbookName, NumberOfVersions} of
                 {all, all} ->
                     %% All versions of all cookbooks
                     sqerl:select(fetch_all_cookbook_versions_by_orgid, [OrgId]);
                 {all, _} ->
                     %% Some versions of all cookbooks
                     sqerl:select(fetch_recent_cookbook_versions_by_orgid_and_num, [OrgId, NumberOfVersions]);
                 {_, all} ->
                     %% All versions of one cookbook
                     sqerl:select(fetch_all_cookbook_versions_by_orgid_cookbook, [OrgId, CookbookName])
             end,
    case Result of
        {ok, none} ->
            {ok, []};
        {ok, CookbookVersions} when is_list(CookbookVersions) ->
            Raw = lists:foldl(fun(Row, Acc) ->
                                      N = proplists:get_value(<<"name">>, Row),
                                      V = proplists:get_value(<<"version">>, Row),
                                      [{N,V}|Acc]
                              end,
                              [],
                              CookbookVersions),
            {ok, lists:reverse(Raw)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Fetch the name and "serialized_object" for each of the lastest
%% cookbook versions in the given organization.  We currently store
%% recipes within the opaque blob that is the serialized_object; this
%% is a GZipped JSON string, which will need to be decompressed and
%% decoded from JSON before the actual recipes can be extracted.
-spec fetch_latest_cookbook_recipes(OrgId::object_id()) -> {ok, [{CookbookName::binary(),
                                                                  SerializedObject::binary()}]} |
                                                           {error, term()}.
fetch_latest_cookbook_recipes(OrgId) ->
    case sqerl:select(fetch_latest_cookbook_recipes_by_orgid, [OrgId]) of
        {ok, none} ->
            {ok, []};
        {ok, Rows} when is_list(Rows) ->
            RecipeNames = lists:flatmap(
                            fun(Row) ->
                                    CookbookName = proplists:get_value(<<"name">>, Row),
                                    SerializedObject = proplists:get_value(<<"serialized_object">>, Row),
                                    chef_cookbook:qualified_recipe_names(CookbookName, SerializedObject)
                            end,
                            Rows),
            {ok, lists:sort(RecipeNames)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Retrieve cookbook version dependency information for all an
%% organization's cookbooks as a list of Depsolver dependency sets.
%%
%% We retrieve all cookbook information because
%%
%%    a). Depsolver implements the filtering algorithm we need to
%%    narrow things down to a single environment
%%
%%    and
%%
%%    b). Implementing this logic in the database would require some
%%    non-trivial stored procedures and probably a reworking of how
%%    dependencies are modeled in the database, which would then have
%%    non-trivial impact on CRUD operations for cookbook versions and
%%    environments.  Such work is a future optimization.
%%
%% Intended for internal use.
%%
%% Note that this neither preserves an alphabetical ordering of
%% cookbooks, nor a "most recent first" ordering of versions, as is
%% done in other places in Erchef, because Depsolver does not require
%% its inputs to be sorted in any particular way.  Given that this is
%% for internal usage, and that the direct results of this function
%% call won't be exposed directly to users, this should not be a
%% problem.
%%
%% The database query that supports this function, however, DOES need
%% to be ordered (at least by cookbook name), as the
%% process_dependency_resultset/1 function relies on results from the
%% same cookbook being grouped together.
-spec fetch_all_cookbook_version_dependencies(OrgId :: object_id()) -> {ok, [depsolver:dependency_set()]} |
                                                                       {error, term()}.
fetch_all_cookbook_version_dependencies(OrgId) ->
    case sqerl:select(fetch_all_cookbook_version_dependencies_by_orgid, [OrgId]) of
        {ok, none} ->
            {ok, []};
        {ok, Rows} ->
            DependencyInfo = process_dependency_resultset(Rows),
            {ok, DependencyInfo};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Returns a mapping of cookbook name to a list of version
%% identifiers (at most `NumVersions' long) for all cookbooks and
%% versions that satisfy the constraints imposed by the named
%% Environment.
%%
%% Specifying a CookbookName of `all' means that all cookbooks
%% in the org are retrieved, while specifying an actual name returns
%% information only for that cookbook.
%%
%% (There is no 3-arg version of this function (i.e., without the
%% `CookbookName' argument) because there is only one other place that
%% needs that logic, and it is internal to this module.)
%%
%% Results are sorted by cookbook name; version lists are sorted with
%% most recent version first.
-spec fetch_environment_filtered_cookbook_versions(OrgId :: object_id(),
                                                   EnvName :: binary(),
                                                   CookbookName :: binary() | all,
                                                   NumVersions :: num_versions()) ->
                                                          {ok, [{CookbookName :: binary(), [Version :: binary()]}]} |
                                                          {error, term()}.
fetch_environment_filtered_cookbook_versions(OrgId, EnvName, CookbookName, NumVersions) ->
    case fetch_environment(OrgId, EnvName) of
        {ok, #chef_environment{}=Environment} ->
            Constraints = chef_object:depsolver_constraints(Environment),

            %% Due to how environment constraint information is
            %% currently modeled, as well as the fact that the logic
            %% already exists in Depsolver, we do not do any filtering
            %% in the database; instead, we grab "the world" of
            %% cookbook version information for the org (as name /
            %% version pairs), and pass this information to Depsolver
            %% along with the environment constraints; Depsolver then
            %% filters out all cookbook versions that do not satisfy
            %% the given constraints.  We then take this filtered list
            %% and limit the number of versions returned according to
            %% `NumVersions'

            %% Remember, `CookbookName' can be the atom `all',
            %% in which case, all versions of all cookbooks are
            %% retrieved
            case fetch_latest_cookbook_versions(OrgId, CookbookName, all) of
                {ok, Packages} ->
                    {ok, Filtered} = depsolver:filter_packages(Packages, Constraints),
                    Result = case {Filtered, CookbookName} of
                                 %% In the odd case that an environment's
                                 %% constraints completely invalidate every
                                 %% cookbook version in an org, we still need
                                 %% to return some data to fulfill our
                                 %% published API; this case statement handles
                                 %% the various situations that may arise
                                 {[], all} ->
                                     %% Everything was filtered out,
                                     %% but we're looking for more
                                     %% than one cookbook; since
                                     %% `Packages' already has all the
                                     %% names of all the cookbooks, we
                                     %% just pull the data from there.
                                     [{Name, []}
                                      || Name <- lists:sort(proplists:get_keys(Packages))];
                                 {[], _} ->
                                     %% Everything was filtered out,
                                     %% but we're looking for exactly
                                     %% one cookbook
                                     [{CookbookName, []}];
                                 {_, _} ->
                                     %% Something made it past the
                                     %% filter; proceed.  This will be
                                     %% the most common code path.
                                     condense_depsolver_results(Filtered, NumVersions)
                             end,
                    {ok, Result};
                {error, Reason} ->
                    %% Problem fetching latest cookbook versions
                    {error, Reason}
            end;
        {error, Reason} ->
            %% Problem fetching environment information
            {error, Reason}
    end.



%% @doc Given an environment in an organization, return a sorted list
%% of qualified recipe names for the cookbook versions that best match
%% the environment's version constraints, if any.
-spec fetch_environment_filtered_recipes(OrgId :: object_id(),
                                         EnvName :: binary()) ->
                                                {ok, [QualifiedRecipeName :: binary()]} |
                                                {error, term()}.
fetch_environment_filtered_recipes(OrgId, EnvName) ->
    %% We're going to fetch the database ID for *all*
    %% cookbook versions in a single query and process the results
    %% into an dict data structure, keyed on the {CookbookName,
    %% Version} tuple.  The value will database ID.
    %%
    %% Then, we will determine which cookbook versions are the best
    %% match for the given environment's version constraints.
    %%
    %% These results will essentially be a list of {CookbookName,
    %% Version} pairs, which we can use with our dict to get the
    %% database IDs of just the cookbook versions we are interested
    %% in.
    %%
    %% Until we can generate SQL on-the-fly for Sqerl (to use IN
    %% clauses with an unbounded list of values), we will use the
    %% "bulk get" strategy that we have used in other places in Erchef
    %% (notably search) to retrieve the serialized objects for each of
    %% these cookbook versions in batches.  (The serialized object
    %% contains the list of recipes.)  Once all the data is retrieved
    %% from the database, we simply extract the recipe names and
    %% return them to the user, sorted.

    {ok, CookbookVersionIdDict} = create_cookbook_version_dict(OrgId),

    case fetch_environment_filtered_cookbook_versions(OrgId, EnvName, all, 1) of
        {ok, Versions} ->
            %% Determine the IDs we are interested in
            OnlyValidIds = extract_ids_using_filtered_results(CookbookVersionIdDict, Versions),

            %% Take the IDs and grab the cookbook name and serialized
            %% objects for them all (cookbook name is necessary to
            %% generate qualified recipe names, and this information
            %% is not present in the serialized object)
            {ok, CookbookSOPairs} = fetch_cookbook_version_serialized_objects(OnlyValidIds),

            %% Now just assemble the list of recipes
            Results = extract_recipe_names_from_serialized_objects(CookbookSOPairs),
            {ok, Results};
        {error, Reason} ->
            %% Couldn't get latest cookbook versions for the environment for some reason
            {error, Reason}
    end.

%% cookbook version ops
-spec fetch_cookbook_version(OrgId::object_id(),
                             versioned_cookbook()) -> #chef_cookbook_version{} |
                                                      {cookbook_exists, object_id()} |
                                                      not_found |
                                                      {error, term()}.
fetch_cookbook_version(OrgId, {Name, {Major, Minor, Patch}}) ->
    %% This is coded following fetch_object similar to fetch_data_bag_item/3
    RecordName = chef_cookbook_version,
    {QueryName, FirstRecordTxfm} = query_and_txfm_for_record(fetch, RecordName),
    case sqerl:select(QueryName, [OrgId, Name, Major, Minor, Patch], FirstRecordTxfm) of
        {ok, #chef_cookbook_version{} = CBVersion} ->
            case fetch_cookbook_version_checksums(OrgId, CBVersion#chef_cookbook_version.id) of
                Checksums when is_list(Checksums) ->
                    CBVersion#chef_cookbook_version{checksums = Checksums};
                {error, Error} ->
                    {error,Error}
            end;
        {ok, none} ->
            %% check if we have a cookbook entry
            case fetch_cookbook_authz(OrgId, Name) of
                not_found ->
                    not_found;
                AuthzId when is_binary(AuthzId) ->
                     {cookbook_exists, AuthzId};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.





%% TODO: Refactor this to use num_versions


-spec fetch_latest_cookbook_version(OrgId::object_id(),
                             CookbookName::binary()) ->
     #chef_cookbook_version{} | not_found | {error, term()}.
%% @doc Return the latest version of the requested cookbook
fetch_latest_cookbook_version(OrgId, CookbookName) ->
  RecordName = chef_cookbook_version,
  {QueryName, FirstRecordTxfm} = query_and_txfm_for_record(fetch_latest, RecordName),
  case sqerl:select(QueryName, [OrgId, CookbookName], FirstRecordTxfm) of
    {ok, none} ->
      not_found;
    {ok, #chef_cookbook_version{} = ResultRecord} ->
      case fetch_cookbook_version_checksums(OrgId, ResultRecord#chef_cookbook_version.id) of
        Checksums when is_list(Checksums) ->
          ResultRecord#chef_cookbook_version{checksums = Checksums};
        {error, Error} ->
          {error, Error}
      end;
    {error, Error} ->
      {error, Error}
  end.

-spec create_cookbook_version(#chef_cookbook_version{}) ->
    {ok, non_neg_integer()} | {error, term()}.
create_cookbook_version(CookbookVersion) ->
    case create_cookbook_if_needed(CookbookVersion) of
        ok ->
            create_object(CookbookVersion);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_cookbook_version(#chef_cookbook_version{}) ->
    {ok, non_neg_integer()} | {error, _}.
update_cookbook_version(#chef_cookbook_version{ id                = Id,
                                                frozen            = Frozen,
                                                meta_attributes   = MetaAttributes,
                                                meta_deps         = MetaDeps,
                                                meta_long_desc    = MetaLongDesc,
                                                metadata          = Metadata,
                                                serialized_object = SerializeObject,
                                                last_updated_by   = LastUpdatedBy,
                                                updated_at        = UpdatedAt }=UpdatedVersion) ->
    case update_cookbook_version_checksums(UpdatedVersion) of
        ok ->
            UpdatedFields = [Frozen, MetaAttributes, MetaDeps, MetaLongDesc, Metadata, SerializeObject, LastUpdatedBy, UpdatedAt, Id],
            do_update(update_cookbook_version, UpdatedFields);
        {error, Reason} ->
            {error, Reason}
    end.


update_cookbook_version_checksums(#chef_cookbook_version{ id        = Id,
                                                          org_id    = OrgId,
                                                          name      = Name,
                                                          major     = Major,
                                                          minor     = Minor,
                                                          patch     = Patch,
                                                          checksums = Checksums}) ->
    % Set up sets
    ExistingChecksums = sets:from_list(fetch_cookbook_version_checksums(OrgId, Id)),
    UpdatedChecksums  = sets:from_list(Checksums),

    Deletions = sets:subtract(ExistingChecksums, UpdatedChecksums),
    Additions = sets:subtract(UpdatedChecksums, ExistingChecksums),

    case delete_cookbook_checksums(sets:to_list(Deletions), OrgId, Id) of
        ok ->
            insert_cookbook_checksums(sets:to_list(Additions), OrgId, Name, Major, Minor, Patch);
        {error, Reason} ->
            {error, Reason}
    end.


-spec delete_cookbook_version(#chef_cookbook_version{}) -> {ok, 1 | 2} |
                                                           {error, term()}.
%% @doc Delete a cookbok version. This will delete from several
%% different tables in the DB. This function will return `{ok, 2}' if
%% the deleted cookbook version was the last version for the cookbook
%% name to indicate that both the cookbook version and the parent
%% cookbook (containing authz id) were deleted. Otherwise, the
%% function will return `{ok, 1}'.
%%
delete_cookbook_version(#chef_cookbook_version{id=CookbookVersionId,
                                               org_id=OrgId,
                                               name=Name}) ->
    case delete_checksums(OrgId, CookbookVersionId) of
        {ok, 1 } ->
            case delete_object(delete_cookbook_version_by_id, CookbookVersionId) of
                {ok, 1} ->
                    case delete_cookbook_if_last(OrgId, Name) of
                        {ok, N} ->
                            {ok, N + 1};
                        Error = {error, _} ->
                            Error
                    end;
                {error, Reason} ->
                    {error, Reason}
             end;
        {error, Reason} ->
            {error, Reason}
    end.

%% The API does not update data_bag objects at present. The only reason to have an upadte at
%% this time would be for audit/debug purposes where we could keep track of who and when a
%% change was made either to the ACL for the data_bag or to be able to record when a change
%% was made to one of its items. Unclear if this will ever be useful.
%%
%% -spec update_data_bag(#chef_data_bag{}) -> {ok, 1 | not_found} | {error, term()}.
%% update_data_bag(#chef_data_bag{last_updated_by = LastUpdatedBy,
%%                                updated_at = UpdatedAt,
%%                                id = Id}) ->
%%     UpdateFields = [LastUpdatedBy, UpdatedAt, Id],
%%     do_update(update_data_bag_by_id, UpdateFields).

%% @doc Return a proplist of the parameterized SQL queries needed for chef_sql
statements(DbType) ->
    File = atom_to_list(DbType) ++ "_statements.config",
    Path = filename:join([filename:dirname(code:which(?MODULE)), "..", "priv", File]),
    {ok, Statements} = file:consult(Path),
    Statements.

%% Sandbox Operations

fetch_sandbox(OrgId, SandboxID) ->
    case sqerl:select(find_sandbox_by_id, [OrgId, SandboxID]) of
        {ok, Rows} when is_list(Rows) ->
            {ok, sandbox_join_rows_to_record(Rows)};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

create_sandbox(#chef_sandbox{} = Sandbox) ->
    create_object(Sandbox).

-spec delete_sandbox(binary()) -> {ok, 1 | 'not_found'} |
                                  {error, term()}.
delete_sandbox(SandboxId) when is_binary(SandboxId) ->
    delete_object(delete_sandbox_by_id, SandboxId).


%% Checksum Operations

%% @doc Helper function for testing checksum existence.
-spec checksum_exists(Checksum :: binary()) ->
                             boolean() | {error, term()}.
checksum_exists(ChecksumId) ->
    case sqerl:select(find_checksum_by_id, [ChecksumId], first_as_scalar, [checksum]) of
        {ok, Checksum} when is_binary(Checksum) ->
            true;
        {ok, none} ->
            false;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Given an Org and list of checksums, mark all of them as having been uploaded.  In
%% practice, this means adding a new row to the checksums table. Returns 'ok' if all are
%% updated successfully, and an error tuple at the first failure; checksums beyond that are
%% not altered @end
%%
%% TODO: This wants to be in a transaction!
-spec mark_checksums_as_uploaded(binary(), [binary()]) ->
                                        'ok' |
                                        {'error', _} |
                                        {'foreign_key', _} |
                                        {'ok', 'none' | number()}.
mark_checksums_as_uploaded(_OrgId, []) ->
    ok;
mark_checksums_as_uploaded(OrgId, [Checksum|Rest]) ->
    case sqerl:statement(insert_checksum, [OrgId, Checksum], count) of
        {ok, 1} ->
            mark_checksums_as_uploaded(OrgId, Rest);
        {conflict, _} ->
            %% A conflict shouldn't stop us, because in this case, it just means
            %% that this org already is aware of this checksum being uploaded.
            mark_checksums_as_uploaded(OrgId, Rest);
        Error ->
            Error
    end.

-spec non_uploaded_checksums(binary(), binary()) -> [binary()] | {error, term()}.
non_uploaded_checksums(SandboxId, OrgId) when is_binary(SandboxId),
                                              is_binary(OrgId) ->
    case sqerl:select(non_uploaded_checksums, [OrgId, SandboxId], rows_as_scalars, [checksum]) of
        {ok, none} ->
            [];
        {ok, Checksums} when is_list(Checksums) ->
            Checksums;
        {error, Reason} ->
            {error, Reason}
    end.

%% private functions

-spec fetch_object(bin_or_string(), bin_or_string(), chef_object_name() | chef_cookbook) ->
    {ok, chef_object() | not_found} | {error, term()}.
%% @doc Fetch a single Chef object with the specified `Name'. The type of object to retrieve
%% is determined by `RecordName'.
fetch_object(OrgId, Name, RecordName) ->
    {QueryName, FirstRecordTxfm} = query_and_txfm_for_record(fetch, RecordName),
    case sqerl:select(QueryName, [OrgId, Name], FirstRecordTxfm) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when RecordName =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Given a query type and chef_object() record name, return the appropriate prepared
%% query name and sqerl record transform data to use in a call to sqerl.
query_and_txfm_for_record(fetch, chef_node) ->
    {find_node_by_orgid_name, ?FIRST(chef_node)};
query_and_txfm_for_record(fetch, chef_role) ->
    {find_role_by_orgid_name, ?FIRST(chef_role)};
query_and_txfm_for_record(fetch, chef_environment) ->
    {find_environment_by_orgid_name, ?FIRST(chef_environment)};
query_and_txfm_for_record(fetch, chef_client) ->
    {find_client_by_orgid_name, ?FIRST(chef_client)};
query_and_txfm_for_record(fetch, chef_data_bag) ->
    {find_data_bag_by_orgid_name, ?FIRST(chef_data_bag)};
query_and_txfm_for_record(fetch, chef_data_bag_item) ->
    {find_data_bag_item_by_orgid_name, ?FIRST(chef_data_bag_item)};
query_and_txfm_for_record(fetch, chef_cookbook_version) ->
    {find_cookbook_version_by_orgid_name_version, ?FIRST(chef_cookbook_version)};
query_and_txfm_for_record(fetch_latest, chef_cookbook_version) ->
    {find_latest_cookbook_version_by_orgid_name, ?FIRST(chef_cookbook_version)}.

-spec fetch_objects(bin_or_string(), chef_object_name()) ->
                           {ok, [binary()]} | {error, term()}.
%% @doc Return list of object names for a given organization and object type
fetch_objects(OrgId, RecordName) ->
    QueryName = list_query_for(RecordName),
    case sqerl:select(QueryName, [OrgId], rows_as_scalars, [name]) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

list_query_for(chef_node) ->
    list_nodes_for_org;
list_query_for(chef_role) ->
    list_roles_for_org;
list_query_for(chef_environment) ->
    list_environments_for_org;
list_query_for(chef_client) ->
    list_clients_for_org;
list_query_for(chef_data_bag) ->
    list_data_bags_for_org;
list_query_for(chef_data_bag_item) ->
    list_data_bag_items_for_data_bag.

-spec bulk_get_objects(chef_type(),
                       [binary()]) ->
                              {ok, [binary()] | not_found} |
                              {error, term()}.
%% @doc Return a list of objects for the given IDs as binary JSON blobs.
%% The standard code path is to return these blobs from the serialized_object
%% column in the table.
%%
bulk_get_objects(Type, Ids) ->
    Query = bulk_get_query_for_count(Type, length(Ids)),
    case sqerl:select(Query, Ids, rows_as_scalars, [serialized_object]) of
        {ok, none} ->
            {ok, not_found};
        {ok, L} when is_list(L) ->
            {ok, L};
        {error, Error} ->
            {error, Error}
    end.

-spec create_object(Object :: chef_object() |
                              #chef_sandbox{} |
                              #chef_cookbook_version{}) -> {ok, non_neg_integer()} |
                                                           sqerl_error().
%% @doc create an object given a chef object record
create_object(#chef_node{}=Node) ->
    create_object(insert_node, Node);
create_object(#chef_role{}=Role) ->
    create_object(insert_role, Role);
create_object(#chef_environment{}=Environment) ->
    create_object(insert_environment, Environment);
create_object(#chef_client{}=Client) ->
    create_object(insert_client, Client);
create_object(#chef_data_bag{}=DataBag) ->
    create_object(insert_data_bag, DataBag);
create_object(#chef_data_bag_item{}=DataBagItem) ->
    create_object(insert_data_bag_item, DataBagItem);
%% This does not exactly follow the same pattern as it needs to
%% insert a list of checksums into a separate table.
%%
%% We split the #chef_cookbook_version{} record into two lists
%% and insert into the DB as appropriate.  Note this depends
%% on the stability of the record order!
create_object(#chef_cookbook_version{checksums = Checksums}=CookbookVersion) ->
    Fields0 = flatten_record(CookbookVersion),
    Fields = cookbook_version_fields_for_insert(Fields0),
    case create_object(insert_cookbook_version, Fields) of
        {ok, 1} ->
            case insert_cookbook_checksums(Checksums,
                                           CookbookVersion#chef_cookbook_version.org_id,
                                           CookbookVersion#chef_cookbook_version.name,
                                           CookbookVersion#chef_cookbook_version.major,
                                           CookbookVersion#chef_cookbook_version.minor,
                                           CookbookVersion#chef_cookbook_version.patch) of
                ok ->
                    {ok, 1};
                {error, _Reason} ->
                    %% FIXME - what cleanup do I need to do here given I'm not in a transaction
                    {error, invalid_checksum}
            end;
        {conflict, Reason} ->
            {conflict, Reason};
        {error, Reason} ->
            {error, Reason}
    end;
create_object(#chef_sandbox{id=SandboxId,
                            org_id = OrgId,
                            created_at = CreatedAt,
                            checksums = Checksums}) ->
    %% We're not dispatching to the general create_object/2 because creating a sandbox
    %% involves adding multiple rows to a table.  Also, we currently embed a list of
    %% checksums in the sandbox record; this won't play nicely with the general
    %% 'create_object' logic, which passes all record fields (in order) as parameters for a
    %% prepared statement

    RawChecksums = [C || {C, _Uploaded} <- Checksums],

    case insert_sandboxed_checksums(RawChecksums, OrgId, SandboxId, CreatedAt) of
        ok ->
            %% (Remember, create_object/1 should return {ok, Number})
            {ok, 1};
        Error ->
            %% We could have potentially inserted some sandboxed checksum rows before the
            %% error was thrown.  If we had transactions, we could just bail out here, but
            %% instead we need to do a little cleanup.  Fortunately, this just means
            %% deleting all rows with the given sandbox id.

            delete_sandbox(SandboxId),

            %% Finally, we'll pass the root cause of
            %% the failure back up
            Error
    end.

-spec create_object(atom(), tuple() | list()) -> {ok, non_neg_integer()} | {error, term()} | {conflict, term()}.
create_object(QueryName, Args) when is_atom(QueryName), is_list(Args) ->
    sqerl:statement(QueryName, Args, count);
create_object(QueryName, Record) when is_atom(QueryName) ->
    List = flatten_record(Record),
    create_object(QueryName, List).

-spec cookbook_version_fields_for_insert(CbFields:: list()) -> list().
cookbook_version_fields_for_insert(CbFields) ->
   %% We drop the last record field - checksums
   lists:reverse(tl(lists:reverse(CbFields))).

%% @doc Inserts FK references to checksums into the database
%%
%% Returns 'ok' if all the records were inserted. Returns an error tuple
%% on the first error it detects.  Further processing of the list is
%% abandoned at that point.
-spec insert_cookbook_checksums(Checksums:: list(), bin_or_string(), bin_or_string(),
                                non_neg_integer(), non_neg_integer(),
                                non_neg_integer()) -> ok | {error, term()}.
insert_cookbook_checksums([], _OrgId, _Name, _Major, _Minor, _Patch) ->
    ok;
insert_cookbook_checksums([Checksum|Rest], OrgId, Name, Major, Minor, Patch) ->
    case sqerl:statement(insert_cookbook_version_checksum, [Checksum, OrgId, Name, OrgId, Major, Minor, Patch], count) of
        {ok, 1} ->
            insert_cookbook_checksums(Rest, OrgId, Name, Major, Minor, Patch);
        {foreign_key, _Reason} ->
            {error, {checksum_missing, Checksum} };
        Error ->
            Error
    end.

delete_cookbook_checksums([], _OrgId, _CookbookVersionId) ->
    ok;
delete_cookbook_checksums([Checksum|Rest], OrgId, CookbookVersionId) ->
    case sqerl:statement(delete_cookbook_version_checksum, [Checksum, OrgId, CookbookVersionId], count) of
        {ok, _Count} ->
            delete_cookbook_checksums(Rest, OrgId, CookbookVersionId);
        Error ->
            Error
    end.

-spec delete_object(delete_query(), binary()) -> {ok, 1 | 'not_found'} | sqerl_error().
delete_object(delete_sandbox_by_id = Query, Id) ->
    %% Special-casing sandbox deletion, since it involves deleting multiple rows from a
    %% table, as opposed to one for all other Chef objects.
    case sqerl:statement(Query, [Id]) of
        {ok, N} when is_integer(N) ->
            {ok, 1}; %% We "pretend" there was only one thing to delete
        {ok, none} ->
            {ok, not_found};
        Error ->
            Error
    end;
delete_object(Query, Id) ->
    case sqerl:statement(Query, [Id]) of
        {ok, 1} ->
            {ok, 1};
        {ok, none} ->
            {ok, not_found};
        Error ->
            Error
    end.

delete_object(delete_cookbook_by_orgid_name = Query, OrgId, Name) ->
    %% Special-casing cookbook deletion, since we don't have an exposed ID
    case sqerl:statement(Query, [OrgId, Name]) of
        {ok, 1} ->
            {ok, 1};
        {ok, none} ->
            {ok, not_found};
        Error ->
            Error
    end.

is_undefined(undefined) ->
    true;
is_undefined(_) ->
    false.

flatten_record(Rec) ->
    [_RecName|Tail] = tuple_to_list(Rec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, Rec});
        false -> ok
    end,
    Tail.

%% @doc Render the name of the bulk get query according to the bulk_get naming convention
%% (RFC 56667)
bulk_get_query_for_count(Type, X) ->
    TypeStr = atom_to_list(Type) ++ "s_",
    list_to_existing_atom("bulk_get_" ++ TypeStr ++ integer_to_list(X)).

do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        Error ->
            Error
    end.


%% @doc Safely retrieves a value from a proplist.  Throws an error if the specified key does
%% not exist in the list.
-spec safe_get(Key::binary(), Proplist::[{binary(), term()}]) -> term().
safe_get(Key, Proplist) ->
    {Key, Value} = lists:keyfind(Key, 1, Proplist),
    Value.

%% @doc Transforms a collection of proplists representing a sandbox / checksum join query
%% result and collapses them all into a single sandbox record.  There is a row for each
%% checksum.  A checksum tuple is extracted from each row; sandbox information is extracted
%% from the final row (since it's the same in every row).
%%
%% See the 'find_sandbox_by_id' prepared query for the row "shape".
sandbox_join_rows_to_record(Rows) ->
    sandbox_join_rows_to_record(Rows, []).
sandbox_join_rows_to_record([LastRow|[]], Checksums) ->
    C = proplist_to_checksum(LastRow),
    #chef_sandbox{id = safe_get(<<"sandbox_id">>, LastRow),
                  org_id = safe_get(<<"org_id">>, LastRow),
                  created_at = safe_get(<<"created_at">>, LastRow),
                  checksums = lists:reverse([C|Checksums])};
sandbox_join_rows_to_record([Row|Rest], Checksums ) ->
    C = proplist_to_checksum(Row),
    sandbox_join_rows_to_record(Rest, [C|Checksums]).

%% @doc Convenience function for assembling a checksum tuple from a proplist containing
%% 'checksum' and 'uploaded' keys.
proplist_to_checksum(Proplist) ->
    {safe_get(<<"checksum">>, Proplist),
     %% Normalize boolean representations
     %% TODO: It would be nice for this transformation to reside in sqerl
     case safe_get(<<"uploaded">>, Proplist) of
         0 -> false;
         1 -> true;
         true -> true;
         false -> false
     end}.


%% @doc Inserts sandboxed checksum records into the database.  All records are timestamped
%% with the same stamp, namely `CreatedAt`, which is a binary string in SQL date time
%% format.
%%
%% Returns 'ok' if all records are inserted without issue.  Returns an error tuple on the
%% first checksum that fails to insert into the database for whatever reason.  Further
%% processing of the list is abandoned at that point.
insert_sandboxed_checksums([], _OrgId, _SandboxId, _CreatedAt) ->
    ok;
insert_sandboxed_checksums([Checksum|Rest], OrgId, SandboxId, CreatedAt) ->
    case sqerl:statement(insert_sandboxed_checksum, [OrgId, SandboxId, Checksum, CreatedAt], count) of
        {ok, 1} ->
            insert_sandboxed_checksums(Rest, OrgId, SandboxId, CreatedAt);
        %% If we don't get ok assume it's an error and pass it back up
        Error ->
            Error
    end.

%% @doc Helper function for testing cookbook existence.
-spec cookbook_exists(OrgId :: object_id(),
                      CookbookName :: binary()) ->
                             boolean() | {error, term()}.
cookbook_exists(OrgId, CookbookName) ->
    case fetch_cookbook_authz(OrgId, CookbookName) of
        AuthzId when is_binary(AuthzId) ->
            true;
        not_found ->
            false;
        {error, Reason} ->
            {error, Reason}
    end.

-spec create_cookbook_if_needed(CookbookVersion::#chef_cookbook_version{}) ->
    ok | {error, term()}.
%% @doc Helper function which creates a row in the cookbook table if it
%% not already there
create_cookbook_if_needed(#chef_cookbook_version{org_id = OrgId, name = Name}=CookbookVersion) ->
    create_cookbook_if_needed(cookbook_exists(OrgId, Name), CookbookVersion).

-spec create_cookbook_if_needed(Exists::boolean(),
                                CookbookVersion::#chef_cookbook_version{}) ->
    ok | {error, term()}.
create_cookbook_if_needed(false, #chef_cookbook_version{authz_id = AuthzId,
                                                        org_id = OrgId,
                                                        name = Name}) ->
    case sqerl:statement(insert_cookbook, [AuthzId, OrgId, Name], count) of
        {ok, N} when is_integer(N) ->
            ok;
        Error ->
            Error
    end;
create_cookbook_if_needed(true, _CookbookVersion) ->
    ok;
create_cookbook_if_needed({error, Reason}, _CookbookVersion) ->
    {error, Reason}.

%% @doc Helper function used by fetch_cookbook_version/2 to return the list of
%% checksums for a cookbook.  The CookbookVersionId is the
%% #chef_cookbook_version{id} field.
%% @end
%%
%% TODO: We could extract out the case sqerl:select statement and logic for all the methods
%% that use rows_as_scalars transform
-spec fetch_cookbook_version_checksums(OrgId::object_id(),
                                       CookbookVersionId::object_id()) ->
                                           [binary()] | {error, term()}.
fetch_cookbook_version_checksums(OrgId, CookbookVersionId) when is_binary(OrgId),
                                                                is_binary(CookbookVersionId) ->
    QueryName = list_cookbook_checksums_by_orgid_cookbook_versionid,
    case sqerl:select(QueryName, [OrgId, CookbookVersionId], rows_as_scalars, [checksum]) of
        {ok, none} ->
            [];
        {ok, Checksums} when is_list(Checksums) ->
            Checksums;
        {error, Reason} ->
            {error, Reason}
    end.

-spec fetch_cookbook_authz(OrgId::object_id(), CookbookName::bin_or_string()) ->
                           object_id() | not_found | {error, term()}.
%% @doc helper function to return the AuthzId for a cookbook.
fetch_cookbook_authz(OrgId, CookbookName) ->
    case sqerl:select(find_cookbook_by_orgid_name,
                      [OrgId, CookbookName], rows_as_scalars, [authz_id]) of
        {ok, none} ->
            not_found;
        {ok, [AuthzId]} when is_binary(AuthzId) ->
            AuthzId;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Delete all checksums for a given cookbook version
-spec delete_checksums(OrgId::binary(),
                       CookbookVersionId::binary()) -> {ok, 1 } | {error, _}.
delete_checksums(OrgId, CookbookVersionId) ->
    case sqerl:statement(delete_cookbook_checksums_by_orgid_cookbook_versionid,
                         [OrgId, CookbookVersionId]) of
        {ok, N} when is_integer(N) -> %% pretend there is 1
            {ok, 1};
        {ok, none} ->
            {ok, 1}; %% this is ok, there might be no checksums to delete
        {error, Error} ->
            {error, Error}
    end.

%% @doc try and delete the row from cookbooks table.  It is protected by a
%% ON DELETE RESTRICT from cookbook_versions so we get a FK violation if there
%% are still cookbook versions left. This is ok, so we catch that case and return
%% `{ok, 0}'.
-spec delete_cookbook_if_last(object_id(), binary()) -> {ok, 0 | 1 } | {error, _}.
delete_cookbook_if_last(OrgId, Name) ->
    case delete_object(delete_cookbook_by_orgid_name, OrgId, Name) of
        {ok, 1} ->
            {ok, 1};
        {foreign_key, _} ->
            {ok, 0}; %% ignore since it means it's
                     %% not the last
        Error ->
            Error
    end.

%% @doc helper function to convert from the three fields stored in the DB
%% and convert it into a version() type as returned by the API
-spec triple_to_version_tuple(Major::non_neg_integer(),
                              Minor::non_neg_integer(),
                              Patch::non_neg_integer()) -> version().
triple_to_version_tuple(Major, Minor, Patch) ->
    {Major, Minor, Patch}.

%% @doc Converts the result of a
%% `fetch_all_cookbook_version_dependencies' query into a list of Depsolver
%% dependency sets.  While each individual row can be thought of as a
%% dependency set, they are each incomplete, and must be merged
%% together by cookbook to create the final dependency sets.
%%
%% `Rows' is a list of proplists containing `name', `version', and `dependencies' fields
-spec process_dependency_resultset(proplists:proplist()) -> [depsolver:dependency_set()].
process_dependency_resultset(Rows) ->
    process_dependency_resultset(Rows, []).

%% @doc Recursive implementation of process_dependency_resultset/1.
-spec process_dependency_resultset(Rows :: proplists:proplist(),
                                   WorkingDependencySet :: [depsolver:dependency_set()]) ->
                                          FinalDependencySet :: [depsolver:dependency_set()].
%% This clause starts things off
process_dependency_resultset([CurrentRow|Rows], []) ->
    DependencySet = row_to_dependency_set(CurrentRow),
    process_dependency_resultset(Rows, [DependencySet]);
%% This clause handles the "middle"
process_dependency_resultset([CurrentRow|Rows], [{LastCookbook, Versions} | DependencySet]) ->

    {CurrentCookbook, [{Version, Dependencies}]} = row_to_dependency_set(CurrentRow),

    %% This logic handles the merging together of versions under their
    %% cookbook, in order to produce a valid Depsolver dependency set
    case CurrentCookbook of
        LastCookbook ->
            process_dependency_resultset(Rows, [{CurrentCookbook, [{Version, Dependencies} | Versions]}
                                                | DependencySet]);
        _ ->
            process_dependency_resultset(Rows, [{CurrentCookbook, [{Version, Dependencies}]},
                                                {LastCookbook, Versions}
                                                | DependencySet ])
    end;
%% Finally, return the results
process_dependency_resultset([], DependencySets) ->
    DependencySets.


%% @doc Helper function to convert a row from a
%% `fetch_all_cookbook_version_dependencies' query into a Depsolver
%% dependency_set.
-spec row_to_dependency_set(Row :: proplists:proplist()) -> depsolver:dependency_set().
row_to_dependency_set(Row) ->
    CookbookName = proplists:get_value(<<"name">>, Row),
    Version = proplists:get_value(<<"version">>, Row),
    DependenciesJSON = proplists:get_value(<<"dependencies">>, Row),

    Dependencies = chef_object:depsolver_constraints(DependenciesJSON),

    {CookbookName, [{Version, Dependencies}]}.

%%------------------------------------------------------------------------------
%% Environment Cookbook Filtering Helper Functions
%%------------------------------------------------------------------------------

%% @doc Given a list of {Cookbook, Version} pairs that satisfy a given
%% set of environmental constraints, condense into a list of
%% {Cookbook, Versions} pairs, where Versions is a list of at most
%% NumVersions version identifiers (sorted most recent first).  If
%% NumVersions = 0, empty lists are returned, and if NumVersions =
%% 'all', then all versions are returned.
%%
%% It is assumed that all inputs are already properly grouped by
%% cookbook and sorted by version, most recent first.
%%
%% Examples:
%% condense_depsolver_results([{<<"cb_1">>, <<"1.0.0">>},
%%                             {<<"cb_1">>, <<"0.5.0">>},
%%                             {<<"cb_2">>, <<"1.2.3">>},
%%                             {<<"cb_2">>, <<"1.0.1">>},
%%                             {<<"cb_3">>, <<"5.0.0">>},
%%                             {<<"cb_3">>, <<"4.0.0">>},
%%                             {<<"cb_3">>, <<"3.0.0">>},
%%                             {<<"cb_3">>, <<"2.0.0">>},
%%                             {<<"cb_3">>, <<"1.0.0">>}], 2) ->
%% [{<<"cb_1">>, [<<"1.0.0">>, <<"0.5.0">>]},
%%  {<<"cb_2">>, [<<"1.2.3">>, <<"1.0.1">>]},
%%  {<<"cb_3">>, [<<"5.0.0">>, <<"4.0.0">>]}]
%%
%% condense_depsolver_results([{<<"cb_1">>, <<"1.0.0">>},
%%                             {<<"cb_1">>, <<"0.5.0">>},
%%                             {<<"cb_2">>, <<"1.2.3">>},
%%                             {<<"cb_2">>, <<"1.0.1">>},
%%                             {<<"cb_3">>, <<"5.0.0">>},
%%                             {<<"cb_3">>, <<"4.0.0">>},
%%                             {<<"cb_3">>, <<"3.0.0">>},
%%                             {<<"cb_3">>, <<"2.0.0">>},
%%                             {<<"cb_3">>, <<"1.0.0">>}], 0) ->
%% [{<<"cb_1">>, []},
%%  {<<"cb_2">>, []},
%%  {<<"cb_3">>, []}]
-spec condense_depsolver_results([{CookbookName :: binary(), Version :: binary()}],
                                 NumVersions :: num_versions()) ->
                                        [{CookbookBin :: binary(), [ VersionBin :: binary()]}].
condense_depsolver_results([], _NumVersions) ->
    [];
condense_depsolver_results([First|Rest], NumVersions) ->
    NumTaken = case NumVersions of
                   0 -> 0;
                   _ -> 1
               end,
    condense_depsolver_results(NumVersions, NumTaken, Rest, [initialize_version(First, NumVersions)]).

%% @doc Recursive implementation of condense_depsolver_results/2.
condense_depsolver_results(NumVersions, NumTaken,
                           [{CurrentCookbook, Version}=Current | Rest],
                           [{LastCookbook, Versions}=Last | RestProcessed]=Processed) ->
    case {LastCookbook, NumVersions} of
        {CurrentCookbook, NumTaken} ->
            %% We've already taken as many of this cookbook as we need; move to the next one
            condense_depsolver_results(NumVersions, NumTaken, Rest, Processed);
        {CurrentCookbook, _} ->
            %% We haven't yet taken as many versions of this cookbook as we need yet
            condense_depsolver_results(NumVersions, NumTaken+1, Rest, [{LastCookbook, [Version | Versions]}
                                                                       | RestProcessed]);
        {_, _} ->
            %% We've switched to a "new" cookbook, and we may or may not need to keep some versions
            Num = case NumVersions of
                      0 -> 0;
                      _ -> 1
                  end,
            condense_depsolver_results(NumVersions, Num, Rest, [initialize_version(Current, NumVersions),
                                                                finalize_versions(Last)
                                                                | RestProcessed])
    end;
condense_depsolver_results(_NumVersions, _NumTaken, [], [Last | Processed]) ->
   lists:reverse([ finalize_versions(Last) | Processed ]).

%% @doc Depending on how many versions of this cookbook we ultimately
%% want to present to the user, we will initialize differently; if we
%% keep nothing, then just use an empty list; otherwise we know we're
%% keeping at least one version, so we start a new list with it
-spec initialize_version({CookbookName :: binary(), Version :: binary()},
                         NumVersions :: num_versions()) ->
                                {CookbookBin :: binary(), [VersionBin :: binary()]}.
initialize_version({CookbookName, Version}, NumVersions) when is_binary(CookbookName),
                                                              is_binary(Version) ->
    {CookbookName, case NumVersions of
                       0 ->
                           [];
                       _ ->
                           [Version]
                   end}.

%% @doc When we have gathered all the versions for a given cookbook
%% that we need, we must reverse the list to maintain the
%% most-recent-version-first characteristic.
-spec finalize_versions({Cookbook :: binary(), Versions :: [binary()]}) ->
                               {Cookbook :: binary(), Versions :: [binary()]}.
finalize_versions({Cookbook, Versions}) ->
    {Cookbook, lists:reverse(Versions)}.

%%------------------------------------------------------------------------------
%% Utility Functions for Environment-Filtered Recipe Processing
%%
%% The following functions support fetch_enviornment_filtered_recipes/2
%%------------------------------------------------------------------------------

%% @doc Given an org ID, retrieve the name and version of all cookbook
%% versions in the organization, mapping them to the database ID of
%% the cookbook version using an Erlang dict.
%%
%% The key of the dict is a {CookbookName, Version} tuple, which
%% corresponds to the data that Depsolver gives.
-spec create_cookbook_version_dict(OrgId :: object_id()) -> {ok, dict()} |
                                                            {error, term()}.
create_cookbook_version_dict(OrgId) ->
    case sqerl:select(fetch_all_cookbook_version_ids_by_orgid, [OrgId]) of
        {ok, none} ->
            {ok, dict:new()};
        {ok, Results} when is_list(Results) ->
            Dict = lists:foldl(fun(Row, Dict) ->
                                       Name = proplists:get_value(<<"name">>, Row),
                                       Version = proplists:get_value(<<"version">>, Row),
                                       Id = proplists:get_value(<<"id">>, Row),
                                       dict:store({Name, Version}, Id, Dict)
                               end,
                               dict:new(),
                               Results),
            {ok, Dict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Returns just the IDs of cookbook versions that are in
%% environment-filtered set of cookbook versions.  Ordering is not
%% important (and so not guaranteed).
-spec extract_ids_using_filtered_results(MappingDict :: dict(),
                                         FilteredCookbookVersions :: [{CookbookName :: binary(),
                                                                       [BestVersion :: binary()]}]) ->
                                                [Id :: integer()].
extract_ids_using_filtered_results(MappingDict, FilteredCookbookVersions) ->
    %% Currently, we're using
    %% fetch_environment_filtered_cookbook_versions/3 to get the
    %% cookbook versions that are available in an environment, but for
    %% this use case, we only ever ask for 1 version that satisfies
    %% the constraints (i.e., the best match).  Since that function
    %% will return a one-version list of version for each cookbook, we
    %% need to "unwrap" that version to match the key structure of our
    %% dict.
    [ dict:fetch({CookbookName, Version}, MappingDict)
      || {CookbookName, [Version]} <- FilteredCookbookVersions ].

%% @doc Given a list of cookbook version database IDs, return a list
%% of {CookbookName, SerializedObject} to facilitate the extraction of
%% recipe names from the group.
-spec fetch_cookbook_version_serialized_objects([Ids :: integer()]) ->
                                                       {ok, [{CookbookName :: binary(),
                                                              SerializedObject :: binary()}]} |
                                                       {error, term()}.
fetch_cookbook_version_serialized_objects(Ids) ->
    {ok, BatchSize} = application:get_env(chef_db, bulk_fetch_batch_size),
    fetch_cookbook_version_serialized_objects(Ids, BatchSize, []).

%% @doc Recursive implementation of fetch_cookbook_version_serialized_objects/1.
fetch_cookbook_version_serialized_objects([], _BatchSize, AllResults) ->
    %% Not reversing the list since order doesn't matter here; we
    %% ultimately want all the recipe names, and *that* list will be
    %% sorted before returning to the user
    {ok, AllResults};
fetch_cookbook_version_serialized_objects(UnprocessedIds, BatchSize, AllResults) ->
    {Batch, RemainingIds} = safe_split(BatchSize, UnprocessedIds),
    case fetch_cookbook_version_serialized_objects_batch(Batch) of
        {ok, Results} ->
            fetch_cookbook_version_serialized_objects(RemainingIds, BatchSize, lists:append(Results, AllResults));
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Actual database interaction logic for
%% fetch_cookbook_version_serialized_objects/3, extracted here for
%% clarity.
%%
%% Note: There is no recursion in this function at all; each
%% invocation only fetches a single, small batch, limited by the
%% 'bulk_fetch_batch_size' configuration parameter for chef_common.
%% To attempt to fetch more than that limit is an error.
-spec fetch_cookbook_version_serialized_objects_batch([Ids :: integer()]) -> {ok, [{CookbookName :: binary(),
                                                                                    SerializedObject :: binary()}]} |
                                                                             {error, term()}.
fetch_cookbook_version_serialized_objects_batch(Ids) when is_list(Ids)->
    BatchSize = length(Ids),
    %% Not using bulk_get_query_for_count/2 since our semantics are
    %% slightly different (those queries are intended for pulling back
    %% just a serialized object, whereas we need to pull back a
    %% cookbook name as well; I didn't want to introduce any
    %% confusion)
    Query = list_to_existing_atom("bulk_get_cbv_serialized_object_" ++ integer_to_list(BatchSize)),
    case sqerl:select(Query, Ids) of
        {ok, none} ->
            {ok, []};
        {ok, Results} ->
            %% Convert Sqerl's proplists to simple tuples for ease of processing later on
            Processed = lists:map(fun(Row) ->
                                          Name = proplists:get_value(<<"name">>, Row),
                                          SO = proplists:get_value(<<"serialized_object">>, Row),
                                          {Name, SO}
                                  end,
                                  Results),
            {ok, Processed};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Extracts qualified recipe names for a collection of cookbook
%% versions, presented as {CookbookName, SerializedObject} pairs.
%%
%% Recipe names are returned sorted alphabetically.
-spec extract_recipe_names_from_serialized_objects([{CookbookName :: binary(),
                                                     SerializedObject :: binary()}]) ->
                                                          [ QualifiedRecipeName :: binary() ].
extract_recipe_names_from_serialized_objects(Pairs) ->
    Results = lists:flatmap(
                fun({CookbookName, SerializedObject}) ->
                        chef_cookbook:qualified_recipe_names(CookbookName, SerializedObject)
                end,
                Pairs),
    lists:sort(Results).

%% Copied from chef_rest_search_resource
%%
%% TODO: consider pulling out into a general utility module
safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
