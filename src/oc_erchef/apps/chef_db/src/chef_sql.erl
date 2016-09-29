%% ex: ts=4 sw=4 et
%% Copyright 2011-2015 Chef Software, Inc. All Rights Reserved.
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
%% @author Seth Falcon <seth@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author James Casey <james@chef.io>
%% @author Mark Mzyk <mmzyk@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
%% @author Ho-Sheng
%% @author Marc Paradise <marc@chef.io>

-module(chef_sql).

-include("chef_db.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-ifdef(namespaced_types).
-type dict() :: dict:dict().
-endif.

-export([
         create_name_id_dict/2,

         create_object/2,
         delete_object/1,
         delete_object/2,
         do_update/2,
         update/2,
         fetch_object/4,
         fetch_object_names/1,
         fetch/1,
         fetch_multi/4,

         bulk_get_authz_ids/2,

         %% checksum ops
         mark_checksums_as_uploaded/2,
         non_uploaded_checksums/2,

         %% data_bag_item ops
         fetch_data_bag_item_ids/2,

         %% search ops
         bulk_get_objects/2,
         bulk_get_clients/2,

         %% cookbook version ops
         cookbook_exists/2,
         bulk_fetch_minimal_cookbook_versions/2,
         fetch_cookbook_version/2,
         fetch_cookbook_versions/1,
         fetch_cookbook_versions/2,
         fetch_latest_cookbook_version/2,
         fetch_latest_cookbook_recipes/1,
         create_cookbook_version/1,
         update_cookbook_version/1,
         delete_cookbook_version/1,
         fetch_all_cookbook_version_dependencies/1,
         fetch_environment_filtered_cookbook_versions/4,

         fetch_environment_filtered_recipes/2,
         fetch_latest_cookbook_versions/3,


         %% Sandbox Ops
         create_sandbox/1,
         delete_sandbox/1,

         %% for license
         count_nodes/0,

         %% for orgs
         fetch_org_metadata/1,

         %% user-org ops
         list_common_orgs_for_users/2,
         is_user_in_org/2,

         %% key ops
         fetch_actors_by_name/2,

         %% policy_groups
         find_all_policy_revisions_by_group_and_name/1,
         find_all_policy_revisions_associated_to_group/2,

         %% policies
         list_all_policy_revisions_by_orgid/1,

         sql_now/0,
         ping/0,
         statements/0,

         select_rows/1

        ]).

-include_lib("sqerl/include/sqerl.hrl").
-include("chef_types.hrl").

-type delete_query() :: atom().

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

%%
%% for license
%%

-spec count_nodes() -> {ok, none} | {ok, integer()} | {error, _}.
%% Return a node count
count_nodes() ->
    sqerl:select(count_nodes, [], first_as_scalar, [count]).

%%
%% chef organization ops
%%

%% @doc Return the org GUID and authz_id for a given organization name (enterprise chef conly)
-spec fetch_org_metadata(binary()) -> {binary(), binary()} | not_found | {error, term()}.
fetch_org_metadata(OrgName) ->
    %% This would be neater if we had first_as_tuple or first_as_array
    case sqerl:select(find_organization_by_name, [OrgName], first, []) of
        {ok, none} ->
            not_found;
        {ok, L} when is_list(L) ->
            Guid = proplists:get_value(<<"id">>,L),
            AuthzId = proplists:get_value(<<"authz_id">>, L),
            {Guid, AuthzId};
        {error, Error} ->
            {error, Error}
    end.

%%
%% user-org membership ops
%% see also: oc_chef_authz/oc_chef_user_org_association
%%           oc_chef_authz/oc_chef_db
%%
-spec is_user_in_org(binary(), binary()) -> boolean() | {error, _}.
is_user_in_org(UserName, OrgName) ->
    case sqerl:select(user_in_org, [UserName, OrgName], first_as_scalar, [count]) of
        {ok, 0} ->
            false;
        {ok, N} when is_integer(N) ->
            true;
        {error, Error} ->
            {error, Error}
    end.

%%
%% list_common_orgs: List the common organizations between two users
%%
%%
-spec list_common_orgs_for_users(binary(), binary()) -> [list()].
list_common_orgs_for_users(User1Id, User2Id) ->
    case sqerl:select(list_common_orgs_for_users, [User1Id, User2Id], rows) of
        {ok, none} ->
            [];
        {ok, L} when is_list(L) ->
            L;
        {error, Error} ->
            {error, Error}
    end.

%%
%% key ops
%%
fetch_actors_by_name(undefined, Name) ->
    fetch_actors_by_name(global, Name);
fetch_actors_by_name(OrgId, Name) ->
    Transform = {rows_as_records, [chef_requestor, record_info(fields, chef_requestor)]},
    Result = sqerl:select(fetch_requestors_by_name, [OrgId, Name], Transform),
    match_result(Result).


%%
%% data_bag_item ops
%%

-spec fetch_data_bag_item_ids(bin_or_string(), bin_or_string()) ->
                                     {ok, [binary()]} | {error, term()}.
%% @doc Return list of data_bag_item names for a given organization
fetch_data_bag_item_ids(OrgId, DataBagName) ->
    QueryName = find_data_bag_item_id_by_orgid_name,
    case sqerl:select(QueryName, [OrgId, DataBagName], rows_as_scalars, [id]) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.


-spec bulk_get_clients(api_version(), [binary()]) -> {ok, [ [proplists:property()] ] | not_found} |
                                      {error, term()}.
%% The client table does not have a serialized_object field
%% so we need to construct a binary JSON representation of the table
%% for serving up in search results.
%%
%% Note this return a list of chef_client records, different from the other bulk_get_X
%% calls
bulk_get_clients(ApiVersion, Ids) ->
    Query = chef_object:bulk_get_query(#chef_client{server_api_version = ApiVersion}),
    case sqerl:select(Query, [Ids], ?ALL(chef_client)) of
        {ok, none} ->
            {ok, not_found};
        {ok, L} when is_list(L) ->
            {ok, [chef_object:set_api_version(R, ApiVersion) || R <- L]};
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_cookbook_versions(OrgId::object_id()) ->
    {ok, [versioned_cookbook()]} | {error, term()}.
%% @doc Return list of [cookbook name, version()] for a given organization.  The list is returned sort
%% by name, major, minor, patch fields.
fetch_cookbook_versions(OrgId) ->
    QueryName = list_cookbook_versions_by_orgid,
    cookbook_versions_from_db(QueryName, [OrgId]).

-spec fetch_cookbook_versions(OrgId::object_id(), CookbookName::binary()) ->
    {ok, [versioned_cookbook()]} | {error, term()}.
%% @doc Return list of [cookbook name, version()] for a given organization and cookbook.
%% The list is returned sorted by name, major, minor, patch fields.
fetch_cookbook_versions(OrgId, CookbookName) when is_binary(CookbookName) ->
    QueryName = list_cookbook_versions_by_orgid_cookbook_name,
    cookbook_versions_from_db(QueryName, [OrgId, CookbookName]).

%% @doc Fetch up to `NumberOfVersions' most recent versions of each
%% cookbook within an organization.  Just returns a proplist mapping
%% cookbook name to concatenated version (i.e., `<<"1.0.0">>' instead of
%% `{1,0,0}').
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
                                    chef_cookbook_version:qualified_recipe_names(CookbookName, SerializedObject)
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
                                                   Environment :: #chef_environment{},
                                                   CookbookName :: binary() | all,
                                                   NumVersions :: num_versions()) ->
                                                          {ok, [{CookbookName :: binary(), [Version :: binary()]}]} |
                                                          {error, term()}.
fetch_environment_filtered_cookbook_versions(OrgId, Environment, CookbookName, NumVersions) ->
    Constraints = chef_object_base:depsolver_constraints(Environment),

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
    end.

%% @doc Given an environment in an organization, return a sorted list
%% of qualified recipe names for the cookbook versions that best match
%% the environment's version constraints, if any.
-spec fetch_environment_filtered_recipes(OrgId :: object_id(),
                                         EnvName :: #chef_environment{}) ->
                                                {ok, [QualifiedRecipeName :: binary()]} |
                                                {error, term()}.
fetch_environment_filtered_recipes(OrgId, Environment) ->
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

    case fetch_environment_filtered_cookbook_versions(OrgId, Environment, all, 1) of
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
-spec bulk_fetch_minimal_cookbook_versions(OrgId::object_id(), [versioned_cookbook()]) ->
                                          [#chef_cookbook_version{}].
bulk_fetch_minimal_cookbook_versions(OrgId, CookbookVersions) ->
    QueryParam = cookbook_versions_array_to_binary(CookbookVersions),
    case sqerl:select(bulk_fetch_minimal_cookbook_versions, [OrgId, QueryParam], ?ALL(chef_cookbook_version)) of
        {ok, none} ->
            [];
        {ok, Results} ->
            Results;
        {error, Error} ->
            {error, Error}
    end.

-spec cookbook_versions_array_to_binary([versioned_cookbook()]) -> binary().
cookbook_versions_array_to_binary(CookbookVersions) ->
    cookbook_versions_array_to_binary(CookbookVersions, <<"{">>, <<"}">>, <<"">>).

-spec cookbook_versions_array_to_binary([versioned_cookbook()], binary(), binary(), binary()) -> binary().
cookbook_versions_array_to_binary([CkbVer|CookbookVersions], Acc, EndBin, Sep) ->
    CkbBin = cookbook_version_to_binary(CkbVer),
    cookbook_versions_array_to_binary(CookbookVersions,
                                      <<Acc/binary, Sep/binary, CkbBin/binary>>,
                                      EndBin, <<",">>);
cookbook_versions_array_to_binary([], Acc, EndBin, _Sep) ->
    <<Acc/binary, EndBin/binary>>.


%% @doc Tranform a versioned_cookbook() into a binary that can be used
%% in the bulk_fetch_minimal_cookbook_version sql query.  A
%% versioned_cookbook() looks like:
%%   {binary(), {integer(), integer(), integer()}}
%%
%% for example:
%%  {<<"yum">>, {0, 2, 43}}
%%
%% our desired output for this example would be:
%%   <<"\"(yum, 0, 2, 43)\"">>
%%
%%
-spec cookbook_version_to_binary(versioned_cookbook()) -> binary().
cookbook_version_to_binary({Name, {MajorInt, MinorInt, PatchInt}}) ->
    iolist_to_binary([<<"\"(">>, Name, <<",">>,
                      integer_to_binary(MajorInt), <<",">>,
                      integer_to_binary(MinorInt), <<",">>,
                      integer_to_binary(PatchInt), <<")\"">>]).

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
    #chef_db_cb_version_update{} | {error, _}.
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
        {ok, Additions, Deletions} ->
            UpdatedFields = [Frozen, MetaAttributes, MetaDeps, MetaLongDesc, Metadata, SerializeObject, LastUpdatedBy, UpdatedAt, Id],
            case do_update(update_cookbook_version, UpdatedFields) of
                {ok, _} -> #chef_db_cb_version_update{added_checksums=Additions,deleted_checksums=Deletions};
                Error -> Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.



%% @doc Updates the checksums associated with the given cookbook version.
%%
%% Since some checksums that were previously associated with this
%% cookbook version may no longer be associated (given this update),
%% we need to do a little housekeeping.  Specifically, the database
%% records linking those checksums to the CBV need to be deleted.
%% Then, if no other CBVs are referencing those checksums, they need
%% to be deleted from the database as a whole (this prevents "garbage"
%% checksums from building up).  The files that correspond to those
%% checksums which are completely removed from the database must then
%% also be removed from S3 to prevent "garbage" build-up there, as
%% well.  The S3 removal is not handled here, but the list of
%% checksummed files to remove from S3 is passed out.
-spec update_cookbook_version_checksums(#chef_cookbook_version{}) -> {'ok',
                                                                      NewChecksums :: [binary()],
                                                                      DeleteFromS3 :: [binary()]} |
                                                                     {'error', term()}.
update_cookbook_version_checksums(#chef_cookbook_version{id        = Id,
                                                         org_id    = OrgId,
                                                         name      = Name,
                                                         major     = Major,
                                                         minor     = Minor,
                                                         patch     = Patch,
                                                         checksums = Checksums}) ->
    %% Set up sets for difference operations
    OldChecksums = sets:from_list(fetch_cookbook_version_checksums(OrgId, Id)),
    NewChecksums  = sets:from_list(Checksums),

    %% Checksums that are in the database (the "old" cookbook version)
    %% that aren't in the new version's list are targeted for deletion
    UnlinkFromCBV = sets:to_list(sets:subtract(OldChecksums,
                                               NewChecksums)),

    %% Conversely, checksums that are in the new list but not the old
    %% need to be added to the system
    AddToCBV = sets:to_list(sets:subtract(NewChecksums,
                                          OldChecksums)),

    %% We must first dissociate the checksums-to-be-deleted from the
    %% cookbook version.  Then, we need to remove from the database
    %% entirely only those checksums that are no longer referenced by
    %% other cookbooks.  This will be a subset of our `UnlinkFromCBV'
    %% list.  This subset must then be removed from S3.
    %%
    %% The additions are new, so we need to link them to the CBV (the
    %% checksums will already be present in the database, and the
    %% corresponding files will have already been uploaded to S3).
    case unlink_checksums_from_cbv(UnlinkFromCBV, OrgId, Id) of
        ok ->
            DeleteFromS3 = delete_orphaned_checksums(OrgId, UnlinkFromCBV),
            case insert_cookbook_checksums(AddToCBV, OrgId, Name, Major, Minor, Patch) of
                ok -> {ok, AddToCBV, DeleteFromS3};
                Error -> Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_cookbook_version(#chef_cookbook_version{}) -> #chef_db_cb_version_delete{} |
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
    case unlink_all_checksums_from_cbv(OrgId, CookbookVersionId) of
        {ok, DeletedChecksums} ->
            CookbookDelete = case delete_object(delete_cookbook_version_by_id, CookbookVersionId) of
                                 {ok, 1} ->
                                     case delete_cookbook_if_last(OrgId, Name) of
                                         {ok, 0} -> false;
                                         {ok, 1} -> true;
                                         Error = {error, _} -> Error
                                     end;
                                 {error, Reason} -> {error, Reason}
                             end,
            dcv_result_or_error(CookbookDelete, DeletedChecksums);
        {error, Reason} -> {error, Reason}
    end.

dcv_result_or_error({error, Reason}, _) ->
    {error, Reason};
dcv_result_or_error(CookbookDelete, DeletedChecksums) ->
    #chef_db_cb_version_delete{deleted_checksums = DeletedChecksums,
                               cookbook_delete = CookbookDelete}.

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

%% @doc Return a proplist of the parameterized SQL queries needed for
%% chef_sql.
statements() ->
    Path = filename:join([code:priv_dir(chef_db), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.

-spec fetch(chef_object:object_rec()) -> chef_object:object_rec() | not_found | forbidden | {error, _Why}.
fetch(Record) ->
    chef_object:fetch(Record, fun select_rows/1).

-spec fetch_multi(api_version(), atom(), atom(), list()) -> [chef_object:object_rec()] | not_found | {error, _Why}.
fetch_multi(ApiVersion, RecModule, QueryName, QueryParams) ->
    case chef_object:fetch_multi(ApiVersion, RecModule, QueryName, QueryParams, fun select_rows/1) of
        {ok, Rows} when is_list(Rows) ->
            {ok, [chef_object:set_api_version(R, ApiVersion) || R <- Rows]};
        Result ->
            Result
    end.

-spec select_rows( {atom(), list()} |
                   {atom(), list(), tuple() | rows} |
                   {atom(), list(), [atom()]}) ->
                         chef_object:select_return()
                       | [epgsql:bind_param()].
select_rows({Query, BindParameters}) ->
    match_result(sqerl:select(Query, BindParameters));
select_rows({Query, BindParameters, Transform}) when is_tuple(Transform);
                                                     Transform == rows ->
    match_result(sqerl:select(Query, BindParameters, Transform));
select_rows({Query, BindParameters, Fields = [_|_]}) ->
    match_result(sqerl:select(Query, BindParameters, rows_as_scalars, Fields)).

-spec match_result(Input) -> NormalizedResult when
      Input :: {ok, list()} | {ok, none} | {ok, non_neg_integer()} | {ok, tuple()} | {error, term()},
      NormalizedResult ::  chef_object:select_return().
match_result({ok, none}) ->
    not_found;
match_result({ok, 0}) ->
    not_found;
match_result({conflict, _} = Conflict) ->
    Conflict;
match_result({ok, Result}) ->
    Result;
match_result({error, _} = Error) ->
    Error.



create_sandbox(#chef_sandbox{} = Sandbox) ->
    create_object(Sandbox).

-spec delete_sandbox(binary()) -> {ok, 1 | 'not_found'} |
                                  {error, term()}.
delete_sandbox(SandboxId) when is_binary(SandboxId) ->
    delete_object(delete_sandbox_by_id, SandboxId).

%% Checksum Operations

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

-spec fetch_object(list(), chef_object_name() | chef_cookbook, atom(), list()) ->
                          {ok, chef_object() | not_found} | {error, term()}.
fetch_object(Keys, RecordName, QueryName, RecordFields) ->
    FirstRecordTxfm = {first_as_record, [RecordName, RecordFields]},
    case sqerl:select(QueryName, Keys, FirstRecordTxfm) of
        %% Awkward sanity check that we got back the expected record type here.
        {ok, Object} when RecordName =:= element(1, Object) ->
            {ok, Object};
        {ok, none} ->
            {ok, not_found};
        {error, Error} ->
            {error, Error}
    end.

%% private functions

%% @doc Given a query type and chef_object() record name, return the appropriate prepared
%% query name and sqerl record transform data to use in a call to sqerl.
query_and_txfm_for_record(fetch, chef_cookbook_version) ->
    {find_cookbook_version_by_orgid_name_version, ?FIRST(chef_cookbook_version)};
query_and_txfm_for_record(fetch_latest, chef_cookbook_version) ->
    {find_latest_cookbook_version_by_orgid_name, ?FIRST(chef_cookbook_version)}.


-spec fetch_object_names(tuple()) ->
                           [epgsql:bind_param()] | {error, term()}.
%% @doc Return list of object names for a object record
fetch_object_names(StubRec) ->
    case chef_object:list(StubRec, fun select_rows/1) of
        L when is_list(L) ->
            L;
        not_found ->
            [];
        {error, _} = Error->
            Error
    end.

-spec bulk_get_objects(chef_type(),
                       [binary()]) ->
                              {ok, [binary()] | not_found} |
                              {error, term()}.
%% @doc Return a list of objects for the given IDs as binary JSON blobs.
%% The standard code path is to return these blobs from the serialized_object
%% column in the table.
%%
bulk_get_objects(Type, Ids) ->
    Query = list_to_existing_atom("bulk_get_" ++ atom_to_list(Type) ++ "s"),
    case sqerl:select(Query, [Ids], rows_as_scalars, [serialized_object]) of
        {ok, none} ->
            {ok, not_found};
        {ok, L} when is_list(L) ->
            {ok, L};
        {error, Error} ->
            {error, Error}
    end.


-spec bulk_get_authz_ids(chef_type(), [binary()]) ->
                              {ok, [[binary()]] | not_found} |
                              {error, term()}.
%% @doc return a list of Authz IDs for the given IDs
bulk_get_authz_ids(Type, Ids) ->
    Query = list_to_existing_atom("bulk_get_" ++ atom_to_list(Type) ++ "_authz_ids"),
    case sqerl:select(Query, [Ids], rows, []) of
        {ok, none} ->
            {ok, not_found};
        {ok, L} when is_list(L) ->
            {ok, L};
        {error, Error} ->
            {error, Error}
    end.


-spec create_object(Object :: chef_object() |
                              #chef_sandbox{} |
                              #chef_user{} |
                              #chef_cookbook_version{}) -> {ok, non_neg_integer()} |
                                                           sqerl_error().
%% @doc create an object given a chef object record
%% This does not exactly follow the same pattern as it needs to
%% insert a list of checksums into a separate table.
%%
%% We split the #chef_cookbook_version{} record into two lists
%% and insert into the DB as appropriate.  Note this depends
%% on the stability of the record order!
create_object(#chef_cookbook_version{checksums = Checksums}=CookbookVersion) ->
    % Note that we can't use chef_object :fields_for_insert - because of how
    % common tests are built, modules are not loaded before they are referenced,
    % and so fields_for_insert will use the default version instead of
    % the one provided by the module when we're running CT, causing failures.
    Fields = chef_cookbook_version:fields_for_insert(CookbookVersion),
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
%% okay, that's pretty ugly, but no more so than all the hacks in here and
%% chef_db for plain old cookbooks, their versions, and their checksums
%% and at least, it's in a transaction
create_object(insert_cookbook_artifact_version = QueryName, Args) when is_list(Args) ->
    case sqerl:select(QueryName, Args) of
        {ok, [_Row]} -> {ok, 1};
        {ok, Else} -> {error, {unexpected_response, Else}};
        %% special error code for missing checksums
        {error, {<<"CS001">>, _}} -> {error, invalid_checksum};
        {error, _Why} = Error -> Error;
        {conflict, _Why} = Conflict -> Conflict
    end;
create_object(QueryName, Args) when is_atom(QueryName), is_list(Args) ->
    sqerl:statement(QueryName, Args, count);
create_object(QueryName, Record) when is_atom(QueryName) ->
    List = chef_object:fields_for_insert(Record),
    create_object(QueryName, List).

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

%% @doc Delete only selected checksums from a given cookbook version,
%% as opposed to all checksums associated with a given cookbook
%% version (for that, see `unlink_all_checksums_from_cbv/2').
unlink_checksums_from_cbv([], _OrgId, _CookbookVersionId) ->
    ok;
unlink_checksums_from_cbv([Checksum|Rest], OrgId, CookbookVersionId) ->
    case sqerl:statement(delete_cookbook_version_checksum, [Checksum, OrgId, CookbookVersionId], count) of
        {ok, _Count} ->
            unlink_checksums_from_cbv(Rest, OrgId, CookbookVersionId);
        Error ->
            Error
    end.

delete_object(Rec) ->
    chef_object:delete(Rec, fun select_rows/1).

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

update(ObjectRec, ActorId) ->
    chef_object:update(ObjectRec, ActorId, fun select_rows/1).

do_update(QueryName, UpdateFields) ->
    case sqerl:statement(QueryName, UpdateFields) of
        {ok, 1} -> {ok, 1};
        {ok, none} -> {ok, not_found};
        Error ->
            Error
    end.



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

%% @doc Delete all checksums for a given cookbook version.
%%
%% Returns a list of all checksums that have been completely removed
%% from the database (because removing them from this cookbook version
%% made them orphans); the corresponding files can now be safely
%% removed from S3.
-spec unlink_all_checksums_from_cbv(OrgId::object_id(),
                                    CookbookVersionId::object_id()) ->
                                           {ok, [binary()]} | {error, term()}.
unlink_all_checksums_from_cbv(OrgId, CookbookVersionId) ->
    % retrieve a list of checksums before we delete the
    % cookbook_version_checksums record
    Checksums = fetch_cookbook_version_checksums(OrgId, CookbookVersionId),
    case sqerl:statement(delete_cookbook_checksums_by_orgid_cookbook_versionid,
                            [OrgId, CookbookVersionId]) of
        {ok, _} ->
            {ok, delete_orphaned_checksums(OrgId, Checksums)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Deletes checksums from the checksums table, but only if they
%% are not part of any existing cookbook version.
%%
%% Returns a list of deleted checksums (a subset of `Checksums')
%% for further upstream processing (i.e., deletion from S3).
-spec delete_orphaned_checksums(OrgId::binary(),
				Checksums::[binary()]) -> [binary()].
delete_orphaned_checksums(OrgId, Checksums) ->
    %% we don't want to delete checksums associated with
    %% cookbook artifact versions
    FilteredChecksums = oc_chef_cookbook_artifact:filter_checksums_to_delete(OrgId, Checksums),
    lists:foldl(fun(Checksum, Acc) ->
            case sqerl:statement(delete_checksum_by_id, [OrgId, Checksum]) of
                {ok, N} when is_integer(N) -> %% pretend there is 1
                    [Checksum|Acc];
                {foreign_key, _} ->
                    %% The checksum may still be associated with
                    %% another cookbook version record which is OK!
                    Acc;
                {error, Reason} ->
                    error_logger:error_msg("Checksum deletion error: ~p~n"
                                           "{~p,delete_orphaned_checksums,2,[{file,~p},{line,~p}]}~n",
                                           [Reason, ?MODULE, ?FILE, ?LINE]),
                    Acc
            end
      end,
      [],
      FilteredChecksums).

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

-spec cookbook_versions_from_db(QueryName :: atom(), Args :: [binary() | object_id()]) ->
    {ok, [versioned_cookbook()]} | {error, term()}.
%% @doc helper function to do the actual logic for returning cookbook
%% versions, either for all of them in an org or for all specific to
%% a given cookbook name.
%%
%% extracted here for clarity and DRYness
cookbook_versions_from_db(QueryName, Args) ->
    case sqerl:select(QueryName, Args, rows, []) of
        {ok, L} when is_list(L) ->
            {ok,
             [ [Name, triple_to_version_tuple(Major, Minor, Patch) ] || [Name, Major, Minor, Patch]  <- L]
            };
        {ok, none} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
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

    Dependencies = chef_object_base:depsolver_constraints(DependenciesJSON),

    {CookbookName, [{Version, Dependencies}]}.

%%------------------------------------------------------------------------------
%% Environment Cookbook Filtering Helper Functions
%%------------------------------------------------------------------------------

%% @doc Given a list of `{Cookbook, Version}' pairs that satisfy a given
%% set of environmental constraints, condense into a list of
%% `{Cookbook, Versions}' pairs, where Versions is a list of at most
%% NumVersions version identifiers (sorted most recent first).  If
%% `NumVersions = 0', empty lists are returned, and if NumVersions =
%% 'all', then all versions are returned.
%%
%% Note that this helper function is never called with an empty list.
%%
%% It is assumed that all inputs are already properly grouped by
%% cookbook and sorted by version, most recent first.
%%
%% Examples:
%% ```
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
%% '''
%%
-spec condense_depsolver_results([{CookbookName :: binary(), Version :: binary()}],
                                 NumVersions :: num_versions()) ->
                                        [{CookbookBin :: binary(), [ VersionBin :: binary()]}].
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
%% The key of the dict is a `{CookbookName, Version}' tuple, which
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
%% of `{CookbookName, SerializedObject}' to facilitate the extraction of
%% recipe names from the group.
-spec fetch_cookbook_version_serialized_objects([Ids :: integer()]) ->
                                                       {ok, [{CookbookName :: binary(),
                                                              SerializedObject :: binary()}]} |
                                                       {error, term()}.
fetch_cookbook_version_serialized_objects(Ids) ->
    BatchSize = envy:get(chef_db, bulk_fetch_batch_size, pos_integer),
    fetch_cookbook_version_serialized_objects(Ids, BatchSize, []).

%% @doc Recursive implementation of {@link fetch_cookbook_version_serialized_objects/1}.
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
%% {@link fetch_cookbook_version_serialized_objects/3}, extracted here for
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
    case sqerl:select(bulk_get_cbv_serialized_object, [Ids]) of
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

list_all_policy_revisions_by_orgid(OrgId) ->
    case sqerl:select(list_all_policy_revisions_by_orgid, [OrgId]) of
        {ok, none} -> {ok, []};
        {ok, AllRevisionRows} ->
            Processed = [tuplize_policy_rev(Row) || Row <- AllRevisionRows],
            {ok, Processed};
        {error, Reason} ->
            {error, Reason}
    end.

tuplize_policy_rev(Row) ->
    PolicyName = proplists:get_value(<<"name">>, Row),
    RevisionID = proplists:get_value(<<"revision_id">>, Row),
    {PolicyName, RevisionID}.


find_all_policy_revisions_by_group_and_name(OrgId) ->
    case sqerl:select(find_all_policy_revisions_by_group_and_name, [OrgId]) of
        {ok, none} ->
            {ok, []};
        {ok, Rows } ->
            Processed = policy_rev_by_group_rows_to_tuple(Rows, []),
            {ok, Processed};
        {error, Reason} ->
            {error, Reason}
    end.

find_all_policy_revisions_associated_to_group(OrgId, GroupName) ->
    case sqerl:select(find_all_policy_revisions_associated_to_group, [OrgId, GroupName]) of
        {ok, none} ->
            {ok, []};
        {ok, Rows } ->
            Processed = policy_rev_by_group_rows_to_tuple(Rows, []),
            {ok, Processed};
        {error, Reason} ->
            {error, Reason}
    end.

policy_rev_by_group_rows_to_tuple([Row|Rest], Processed) ->
    PolicyGroupName = proplists:get_value(<<"policy_group_name">>, Row),
    PolicyName = proplists:get_value(<<"policy_revision_name">>, Row),
    RevisionID = proplists:get_value(<<"policy_revision_revision_id">>, Row),
    ProcessedRow = {PolicyGroupName, PolicyName, RevisionID},
    policy_rev_by_group_rows_to_tuple(Rest, [ProcessedRow|Processed]);
policy_rev_by_group_rows_to_tuple([], Processed) ->
    Processed.

%% @doc Extracts qualified recipe names for a collection of cookbook
%% versions, presented as `{CookbookName, SerializedObject}' pairs.
%%
%% Recipe names are returned sorted alphabetically.
-spec extract_recipe_names_from_serialized_objects([{CookbookName :: binary(),
                                                     SerializedObject :: binary()}]) ->
                                                          [ QualifiedRecipeName :: binary() ].
extract_recipe_names_from_serialized_objects(Pairs) ->
    Results = lists:flatmap(
                fun({CookbookName, SerializedObject}) ->
                        chef_cookbook_version:qualified_recipe_names(CookbookName, SerializedObject)
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

%% @doc Make a dict mapping an object's unique name to its database ID for all objects
%% within a given "index". (This is currently only used for reindexing, so it only works on
%% items that are indexed.)  An index that is a binary is taken to be a data bag name, in
%% which case, the dict will map data bag item ID to database ID for all items within that
%% data bag.
-spec create_name_id_dict(OrgId :: object_id(),
                          Index :: node | role | client | environment | binary()) ->
                                 {ok, dict()} | {error, term()}.
create_name_id_dict(OrgId, Index) ->
    Query = dict_query_for_index(Index),
    Args = dict_query_args_for_index(OrgId, Index),
    KV = dict_key_value_for_index(Index),
    create_dict(Query, Args, KV).

%% @doc Determine the appropriate prepared statement to call to
%% generate a name->id dict for the given kind of object.
dict_query_for_index(DataBagName) when is_binary(DataBagName) -> list_data_bag_item_ids_names_for_org;
dict_query_for_index(node)                                    -> list_node_ids_names_for_org;
dict_query_for_index(role)                                    -> list_role_ids_names_for_org;
dict_query_for_index(client)                                  -> list_client_ids_names_for_org;
dict_query_for_index(environment)                             -> list_environment_ids_names_for_org.

%% @doc Queries for generating name->id dicts only require an OrgId
%% for most things, but require a data bag name if we're querying
%% data bag items.
%%
%% See also dict_query_for_index/1.
dict_query_args_for_index(OrgId, DataBagName) when is_binary(DataBagName) ->
    [OrgId, DataBagName];
dict_query_args_for_index(OrgId, Index) when Index =:= node;
                                             Index =:= role;
                                             Index =:= client;
                                             Index =:= environment ->
    [OrgId].

%% @doc Most objects' "unique name" is stored under a "name" key,
%% except for data bag items, which use "item_name".
%%
%% NOTE: These binaries refer to the names of database table columns.
%%
%% See also create_dict/3, create_name_id_dict/2 and
%% proplists_to_dict/3.
dict_key_value_for_index(DataBagName) when is_binary(DataBagName) ->
    {<<"item_name">>, <<"id">>};
dict_key_value_for_index(Index) when Index =:= node;
                                     Index =:= role;
                                     Index =:= client;
                                     Index =:= environment ->
    {<<"name">>, <<"id">>}.

%% This type is only needed to spec create_dict/3, and then only
%% because we run Dialyzer with -Wunderspecs.
-type dict_queries() :: list_client_ids_names_for_org |
                        list_data_bag_items_ids_names_for_org |
                        list_environment_ids_names_for_org |
                        list_node_ids_names_for_org |
                        list_role_ids_names_for_org.

%% @doc Create a dict mapping `Key' to `Value' across the resultset of
%% executing a database query.
%% @end
%%
%% This spec brought to you by -Wunderspecs
-spec create_dict(Query :: dict_queries(),
                  Args :: list(),
                  {Key :: <<_:32,_:_*40>>, %% <<"name">> | <<"item_name">>
                   Value :: <<_:16>>}) %% <<"id">>
                 -> {ok, dict()} | {error, term()}.
create_dict(Query, Args, {Key, Value}) ->
    case proplist_results(Query, Args) of
        Results when is_list(Results) ->
            {ok, proplists_to_dict(Results, Key, Value)};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Execute a query with the given arguments and return the "raw"
%% resultset as a list of proplists.
-spec proplist_results(Query :: atom(), Args :: list()) -> [[tuple()]] | {error, term()}.
proplist_results(Query, Args) ->
    case sqerl:select(Query, Args) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, Error} ->
            {error, Error}
    end.

%% @doc Given a list of proplists (e.g., a "raw" query resultset from
%% sqerl), create a dict that maps `Key' to `Value', where those are
%% both keys present in each proplist.
%%
%% Thus, using `Key = <<"foo">>' and `Value = <<"bar">>', a proplist
%% of `[{<<"foo">>, 123}, {<<"bar">>, 456}]' would become a dict entry
%% mapping 123 to 456.
%% @end
%%
%% This spec brought to you by -Wunderspecs
-spec proplists_to_dict(ResultSetProplist :: [[tuple()]],
                        Key :: <<_:32,_:_*40>>, %% <<"name">> | <<"item_name">>
                        Value :: <<_:16>>) -> dict(). %% <<"id">>
proplists_to_dict(ResultSetProplist, Key, Value) ->
    lists:foldl(fun(Row, Dict) ->
                        K = proplists:get_value(Key, Row),
                        if
                          K == unknown -> lager:debug("Unable to find ~p", [K]);
                          true -> ok
                        end,
                        case dict:is_key(K, Dict) of
                          true ->
                            lager:debug("Duplicate key found ~p", [K]);
                          false -> ok
                        end,
                        V = proplists:get_value(Value, Row),
                        if
                          V == unknown -> lager:debug("Unable to find ~p", [V]);
                          true -> ok
                        end,
                        dict:store(K, V, Dict)
                end,
                dict:new(),
                ResultSetProplist).
