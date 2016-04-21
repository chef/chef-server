%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Daniel Deleo <dan@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author Mark Mzyk <mmzyk@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
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

-module(chef_db).

-export([
         %% Context record manipulation
         make_context/1,
         make_context/2,
         make_context/3,
         darklaunch_from_context/1,

         create_name_id_dict/3,

         user_record_to_authz_id/2,
         fetch_org_id/2,
         fetch_org_metadata/2,

         %% temporary hack for reindex script until
         %% orgs are migrated
         cdb_fetch_org_metadata/2,
         sql_fetch_org_metadata/2,

         client_record_to_authz_id/2,

         fetch_requestor/3,

         %% Checksum ops
         mark_checksums_as_uploaded/3,

         %% user ops
         count_user_admins/1,
         node_record_to_authz_id/2,

         %% role ops
         fetch_roles/2,
         %% role_record_to_authz_id/2,

         %% environment ops
         fetch_data_bag_item_ids/3,

         %% cookbook_version ops
         cookbook_exists/3,

         fetch_cookbook_version/3,
         fetch_cookbook_versions/2,
         fetch_cookbook_versions/3,
         fetch_latest_cookbook_version/3,
         fetch_latest_cookbook_versions/2,
         fetch_latest_cookbook_versions/3,
         fetch_latest_cookbook_recipes/2,
         fetch_all_cookbook_version_dependencies/2,
         fetch_environment_filtered_cookbook_versions/5,
         fetch_environment_filtered_recipes/3,

         %% Sandbox ops
         make_sandbox/4,
         commit_sandbox/2,

         %% for license
         count_nodes/1,

         is_user_in_org/3,
         connect/0,
         create/3,
         delete/2,
         list/2,
         update/3,
         fetch/2,
         bulk_get/4,
         data_bag_exists/3,
         environment_exists/3]).

-include("chef_db.hrl").
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

-define(gv(Key, PList), proplists:get_value(Key, PList)).

-type db_context() :: #context{}.

%% All the names of functions to create various Chef items, found in this module
-type create_fun() :: 'create_data_bag' |
                      'create_data_bag_item' |
                      'create_environment' |
                      'create_client' |
                      'create_node' |
                      'create_user' |
                      'create_role' |
                      'create_sandbox' |
                      'create_cookbook_version'.

-type update_fun() :: 'update_cookbook_version' |
                      'update_data_bag_item' |
                      'update_environment' |
                      'update_client' |
                      'update_node' |
                      'update_role' |
                      'update_user'.

-type delete_fun() :: 'delete_cookbook_version' |
                      'delete_data_bag' |
                      'delete_data_bag_item' |
                      'delete_environment' |
                      'delete_client' |
                      'delete_node' |
                      'delete_user' |
                      'delete_role' |
                      'delete_sandbox'.

-export_type([
              create_fun/0,
              db_context/0,
              %% delete_fun/0 %% currently not needed anywhere else
              update_fun/0
             ]).

-type object_rec() :: tuple().

%% -type chef_object_name() :: 'chef_node' |
%%                             'chef_role'.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

make_context(ReqId) ->
    #context{reqid = ReqId, darklaunch = undefined, otto_connection = chef_otto:connect()}.

make_context(ReqId, Darklaunch) ->
    #context{reqid = ReqId, darklaunch = Darklaunch, otto_connection = chef_otto:connect()}.

make_context(ReqId, Darklaunch, OttoServer) ->
    #context{reqid = ReqId, darklaunch = Darklaunch, otto_connection = OttoServer}.

-spec create(object_rec(), #context{}, object_id()) -> ok | {conflict, term()} | {error, term()}.
create(#chef_cookbook_version{} = Record, DbContext, ActorId) ->
    create_object(DbContext, create_cookbook_version, Record, ActorId);
create(ObjectRec0, #context{reqid = ReqId}, ActorId) ->
    ObjectRec = chef_object:set_created(ObjectRec0, ActorId),
    QueryName = chef_object:create_query(ObjectRec),
    FlattenedRecord = chef_object:flatten(ObjectRec),
    case stats_hero:ctime(ReqId, {chef_sql, create_object},
                          fun() -> chef_sql:create_object(QueryName, FlattenedRecord) end) of
        {ok, 1} -> ok;
        {conflict, Msg}-> {conflict, Msg};
        {error, Why} -> {error, Why}
    end.

-spec delete(object_rec(), #context{}) -> {ok, 1 | 2} | 1 | 2 | not_found | {error, _}.
delete(#chef_cookbook_version{org_id = OrgId} = CookbookVersion,
       #context{reqid = ReqId} = Ctx) ->
    case delete_object(Ctx, delete_cookbook_version, CookbookVersion) of
        #chef_db_cb_version_delete{cookbook_delete=CookbookDeleted, deleted_checksums=DeletedChecksums} ->
            ?SH_TIME(ReqId, chef_s3, delete_checksums, (OrgId, DeletedChecksums)),
            %% TODO: return the actual chef_db_cb_version_delete record to the caller
            case CookbookDeleted of
                false -> {ok, 1};
                true -> {ok, 2}
            end;
        Result -> Result %% not_found or {error, _}
    end;
delete(ObjectRec, #context{reqid = ReqId}) ->
    stats_hero:ctime(ReqId, {chef_sql, delete_object},
                     fun() -> chef_sql:delete_object(ObjectRec) end).

-spec fetch(object_rec(),
            DbContext :: #context{}) ->
                   object_rec() |
                   not_found |
                   {error, term()}.
fetch(ObjectRec, #context{reqid = ReqId}) ->
    stats_hero:ctime(ReqId, {chef_sql, fetch},
                          fun() ->
                                  chef_sql:fetch(ObjectRec)
                          end).

-spec list(object_rec(), #context{}) -> [binary()] | {error, _}.
list(StubRec, #context{reqid = ReqId} = _Ctx) ->
    stats_hero:ctime(ReqId, {chef_sql, fetch_object_names},
                      fun() ->
                              chef_sql:fetch_object_names(StubRec)
                      end).

-spec update(object_rec(), #context{}, object_id()) ->
             ok | not_found | {conflict, term()} | {error, term()}.
%% TODO: get rid of cookbook_version special case
update(#chef_cookbook_version{org_id =OrgId} = Record, #context{reqid = ReqId} = DbContext, ActorId) ->
    case update_object(DbContext, ActorId, chef_object:update_query(Record), Record) of
        #chef_db_cb_version_update{deleted_checksums=DeletedChecksums} ->
            ?SH_TIME(ReqId, chef_s3, delete_checksums, (OrgId, DeletedChecksums)),
            ok;
        Result -> Result %% {conflict, _} or {error, _}
    end;
update(ObjectRec, #context{reqid = ReqId}, ActorId) ->
    case stats_hero:ctime(ReqId, {chef_sql, do_update},
                          fun() ->
                                  chef_sql:update(ObjectRec, ActorId) end) of
        N when is_integer(N), N > 0 -> ok;
        not_found -> not_found;
        {conflict, Message} -> {conflict, Message};
        {error, Error} -> {error, Error}
    end.

darklaunch_from_context(#context{darklaunch = Darklaunch}) ->
    Darklaunch.

-spec count_user_admins(#context{}) -> integer() | {error, term()}.
count_user_admins(#context{reqid = ReqId}) ->
  case stats_hero:ctime(ReqId, {chef_sql, count_user_admins},
                        fun() -> chef_sql:count_user_admins() end) of
       {ok, Count} -> Count;
       Other -> Other
  end.

-spec user_record_to_authz_id(#context{}, #chef_user{} | not_found) -> id().
user_record_to_authz_id(#context{}, #chef_user{} = UserRecord) ->
    UserRecord#chef_user.authz_id;
user_record_to_authz_id(#context{}, not_found) ->
    %% FIXME: is this what we want here?
    erlang:error({error, not_found}).

-spec fetch_org_id(#context{}, binary()) -> not_found | binary().
fetch_org_id(Context, OrgName) ->
    case fetch_org_metadata(Context, OrgName) of
        not_found -> not_found;
        {OrgId, _OrgAuthzId} -> OrgId
    end.

%% Fetch the id and authz_id for a given organization name
-spec fetch_org_metadata(#context{}, binary() | ?OSC_ORG_NAME) -> not_found | {binary(), binary()}.
fetch_org_metadata(_, ?OSC_ORG_NAME) ->
    {?OSC_ORG_ID, ?OSC_REQUESTOR_ID};
fetch_org_metadata(#context{darklaunch = Darklaunch} = Context, OrgName) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_organizations">>, Darklaunch) of
        true ->
            cdb_fetch_org_metadata(Context, OrgName);
        false ->
            sql_fetch_org_metadata(Context, OrgName)
    end.

%% For couchdb-based orgs, fetch information from the orgname -> metadata cache
cdb_fetch_org_metadata(#context{reqid = ReqId,
                                otto_connection = Server}, OrgName) ->
    case chef_cache:get(org_metadata, OrgName) of
        Error when Error =:= not_found orelse Error =:= no_cache ->
            case ?SH_TIME(ReqId, chef_otto, fetch_org_metadata, (Server, OrgName)) of
                not_found ->
                    not_found;
                {OrgGuid, OrgAuthzId} ->
                    chef_cache:put(org_metadata, OrgName, {OrgGuid, OrgAuthzId}),
                    {OrgGuid, OrgAuthzId}
            end;
        {ok, {OrgGuid, OrgAuthzId}} ->
            {OrgGuid, OrgAuthzId}
    end.

sql_fetch_org_metadata(#context{reqid = ReqId}, OrgName) ->
    ?SH_TIME(ReqId, chef_sql, fetch_org_metadata, (OrgName)).

client_record_to_authz_id(_Context, ClientRecord) ->
    ClientRecord#chef_client.authz_id.

%% @doc Given a name and an org, find either a user or a client and return a
%% #chef_user{} or #chef_client{} record.
%%
%% Looks for a client first, then a user. We search for the client first for two reasons: 1)
%% more API requests come from clients; 2) prevents a DoS attack cleverly chosen user names
%% in OHC breaking clients with matching names.
%%
%% We examine the user/client record and return the certificate containing the actor's
%% public key.  Some legacy records are present in the database that do not have
%% certificates and have only a public key.  For these cases, we return the key tagged with
%% 'key' instead of 'cert'.
-spec fetch_requestor(#context{},
                      binary() | undefined,
                      binary()) -> #chef_client{} | #chef_user{} |
                                   %% TODO: fix chef_wm so we can just return 'not_found'
                                   {'not_found', 'client'}.
fetch_requestor(Context, undefined, UserName) ->
            case fetch(#chef_user{username = UserName}, Context) of
                not_found ->
                    %% back compat for now until we update chef_wm
                    {not_found, client};
                #chef_user{} = User ->
                    User
            end;
fetch_requestor(Context, OrgId, ClientName) ->
    case fetch(#chef_client{org_id = OrgId, name = ClientName}, Context) of
        not_found ->
            fetch_requestor(Context, undefined, ClientName);
        #chef_client{} = Client ->
            Client
    end.

%% @doc Saves sandbox information for a new sandbox in the database, and returns a
%% chef_sandbox record representing the new sandbox.  This is a different pattern from other
%% Chef objects, because the creation of a record for the object requires data that the
%% caller does not have access to (the 'upload' state of the checksums must be retrieved
%% from the database).

-spec make_sandbox(DbContext :: #context{},
                   OrgName :: binary(),
                   ActorId :: object_id(),
                   Checksums :: [binary()]) -> #chef_sandbox{} |
                                               not_found |
                                               {conflict, term()} |
                                               {error, term()}.
make_sandbox(#context{}=Ctx, OrgName, ActorId, Checksums) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            Id = chef_object_base:make_org_prefix_id(OrgId),
            %% TempSandbox doesn't know if the checksums have been uploaded yet or not
            TempSandbox = #chef_sandbox{id=Id,
                                        org_id=OrgId,
                                        checksums = [{C, false} || C <- Checksums]},
            %% TODO: ActorId isn't actually needed in the code, but it is if we want to use common code paths
            case create_object(Ctx, create_sandbox, TempSandbox, ActorId) of
                ok ->
                    %% this sandbox will know if the checksums have been uploaded
                    fetch(TempSandbox, Ctx);
                {conflict, Msg} ->
                    {conflict, Msg};
                {error, Why} ->
                    {error, Why}
            end
    end.

-spec cookbook_exists(DbContext :: #context{},
                      OrgName :: binary(),
                      CookbookName :: binary()) ->
                             boolean() | {error, term()}.
cookbook_exists(#context{reqid=ReqId} = DbContext, OrgName, CookbookName) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            ?SH_TIME(ReqId, chef_sql, cookbook_exists, (OrgId, CookbookName))
    end.

-spec fetch_cookbook_versions(#context{}, binary()) -> {not_found, org} |
                                                       [versioned_cookbook()] |
                                                       {error, any()}.
%% @doc Return a list of all cookbook names and versions in an org
fetch_cookbook_versions(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_cookbook_versions, OrgName).

-spec fetch_cookbook_versions(#context{}, binary(), binary()) -> {not_found, org} |
                                                                 [versioned_cookbook()] |
                                                                 {error, any()}.
%% @doc Return a list of all cookbook names and versions in an org
fetch_cookbook_versions(#context{} = Ctx, OrgName, CookbookName) ->
    fetch_objects(Ctx, fetch_cookbook_versions, OrgName, CookbookName).

-spec fetch_cookbook_version(DbContext :: #context{},
                             OrgName :: binary(),
                             VersionedCookbook :: versioned_cookbook()) -> #chef_cookbook_version{} |
                                                                           {cookbook_exists, object_id()} |
                                                                           not_found |
                                                                           {error, term()}.
fetch_cookbook_version(#context{reqid = ReqId} = Ctx, OrgName, VersionedCookbook) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            stats_hero:ctime(ReqId, {chef_sql, fetch_cookbook_version},
                             fun() -> chef_sql:fetch_cookbook_version(OrgId,
                                                                      VersionedCookbook)
                             end)
    end.

-spec fetch_latest_cookbook_version(Ctx::#context{},
                                    OrgName::binary(),
                                    CookbookName::binary()) ->
                                    #chef_cookbook_version{} |
                                    not_found |
                                    {error, term()}.
%% @doc Return the latest version of the requested cookbook
fetch_latest_cookbook_version(#context{reqid=ReqId} = Ctx, OrgName, CookbookName) ->
    case fetch_org_id(Ctx, OrgName) of
      not_found ->
        not_found;
      OrgId ->
         stats_hero:ctime(ReqId, {chef_sql, fetch_latest_cookbook_version},
                          fun() -> chef_sql:fetch_latest_cookbook_version(OrgId, CookbookName)
                          end)
    end.

%% @doc Retrieve a list of `{Name, Verison}' tuples for the latest version of each cookbook in
%% an organization.  Version information is returned as a binary string (e.g., `<<"1.0.0">>')
%% instead of the tuple form (e.g. `{1,0,0}') that is used elsewhere.
-spec fetch_latest_cookbook_versions(DbContext :: #context{},
                                     OrgName :: binary()) ->
                                            [{CookbookName :: binary(),
                                              VersionString :: binary()}] |
                                            {not_found, org} |
                                            {error, Reason :: term()}.
fetch_latest_cookbook_versions(#context{}=DbContext, OrgName) ->
    fetch_latest_cookbook_versions(DbContext, OrgName, 1).

%% @doc Same as {@link fetch_latest_cookbook_versions/2}, but allows you to specify how many
%% versions for which to fetch information.  For instance, setting `NumberOfVersions' to `3'
%% will retrieve the `{Name, Version}' pairs for the three most recent versions of each
%% cookbook.
%%
%% If fewer than `NumberOfVersions' versions exist for a given cookbook, all
%% versions are represented in the output.
-spec fetch_latest_cookbook_versions(DbContext :: #context{},
                                     OrgName :: binary(),
                                     NumberOfVersions :: non_neg_integer()) ->
                                            [{CookbookName :: binary(),
                                              VersionString :: binary()}] |
                                            {not_found, org} |
                                            {error, Reason :: term()}.
fetch_latest_cookbook_versions(#context{reqid=ReqId}=DbContext, OrgName, NumberOfVersions) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            {not_found, org};
        OrgId ->
            case ?SH_TIME(ReqId, chef_sql, fetch_latest_cookbook_versions, (OrgId, all, NumberOfVersions)) of
                {ok, VersionInfo} ->
                    VersionInfo;
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Retrieves the list of all recipes from the latest version of all an organization's
%% cookbooks and returns their cookbook-qualified names.
-spec fetch_latest_cookbook_recipes(DbContext :: #context{},
                                    OrgName :: binary()) ->
                                           [CookbookQualifiedRecipeName :: binary()] |
                                           {not_found, org} |
                                           {error, Reason :: term()}.
fetch_latest_cookbook_recipes(#context{reqid=ReqId}=DbContext, OrgName) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            {not_found, org};
        OrgId ->
            case ?SH_TIME(ReqId, chef_sql, fetch_latest_cookbook_recipes, (OrgId)) of
                {ok, Results} ->
                    Results;
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Retrieve all cookbook dependency information for an organization as a list of
%% Depsolver dependency sets.
%%
%% See the corresponding function in the chef_sql module for more information.
-spec fetch_all_cookbook_version_dependencies(DbContext :: #context{},
                                              OrgName :: binary()) -> [depsolver:dependency_set()] |
                                                                      {error, term()}.
fetch_all_cookbook_version_dependencies(#context{reqid=ReqId}=DbContext, OrgName) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            case ?SH_TIME(ReqId, chef_sql, fetch_all_cookbook_version_dependencies, (OrgId)) of
                {ok, Results} ->
                    Results;
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Retrieve cookbook versions, subject to any constraints imposed by the specified
%% environment.  `NumVersions' is either an integer or the atom `all', and indicates the
%% maximum number of versions that should be returned that satisfy the constraints for each
%% cookbook
%%
%% If the given CookbookName is the atom 'all', information for all cookbooks is retrieved;
%% if it is a cookbook name (i.e., a binary), then information on just that cookbook will be
%% returned.
-spec fetch_environment_filtered_cookbook_versions(DbContext :: #context{},
                                                   OrgName :: binary(),
                                                   EnvName :: binary(),
                                                   CookbookName :: binary() | all,
                                                   NumVersions :: all | non_neg_integer()) ->
                                                          [{CookbookName :: binary(), [Version :: binary()]}] |
                                                          {error, term()}.
fetch_environment_filtered_cookbook_versions(#context{reqid=ReqId}=DbContext, OrgName, EnvName, CookbookName, NumVersions) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            case fetch(#chef_environment{org_id = OrgId, name = EnvName}, DbContext) of
                #chef_environment{} = Environment ->
                    case ?SH_TIME(ReqId, chef_sql, fetch_environment_filtered_cookbook_versions, (OrgId, Environment, CookbookName, NumVersions)) of
                        {ok, Results} ->
                            Results;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} -> {error, Error}
            end
    end.

fetch_environment_filtered_recipes(#context{reqid=ReqId}=DbContext, OrgName, EnvName) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            case fetch(#chef_environment{org_id = OrgId, name = EnvName}, DbContext) of
                #chef_environment{} = Environment ->
                    case ?SH_TIME(ReqId, chef_sql, fetch_environment_filtered_recipes, (OrgId, Environment)) of
                        {ok, Results} ->
                            Results;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} -> {error, Error}
            end
    end.


-spec fetch_roles(#context{}, binary()) -> {not_found, org} |
                                           [binary()] |
                                           {error, any()}.
%% @doc Return a list of all role names in an org
fetch_roles(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_roles, OrgName).


-spec fetch_data_bag_item_ids(#context{}, binary() | {id, object_id()},
                              binary()) -> [binary()] |
                                           {not_found, org} |
                                           {error, _}.
%% @doc Returns list of data_bag_item names in `DataBagName' for `OrgName'.
fetch_data_bag_item_ids(#context{} = Ctx, OrgName, DataBagName) ->
    fetch_objects(Ctx, fetch_data_bag_item_ids, OrgName, DataBagName).

%% @doc Verifies that all checksums in the given sandbox are marked as uploaded, and if so,
%% deletes the sandbox from the database.
%%
%% This most closely corresponds to the "delete_X" functions for other Chef objects.
-spec commit_sandbox(#context{}, #chef_sandbox{}) -> {ok, 1 | 2} |
                                                     not_found |
                                                     {error, {need_upload, [binary()]}} |
                                                     {error, term()}.
commit_sandbox(#context{}=Ctx, #chef_sandbox{}=Sandbox) ->
    case non_uploaded_checksums(Ctx, Sandbox) of
        [] ->
            %% Good to go!
            remove_sandbox(Ctx, Sandbox);
        Checksums when is_list(Checksums) ->
            %% Oops, you've still got some work to do
            {error, {need_upload, Checksums}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Deletes a sandbox from the database.
%%
%% This is not an exported function, because it is a considered a "private" step in other
%% processes.  Currently, this is only used for committing a sandbox, but in the future, it
%% will be used for purging stale sandboxes as well.
-spec remove_sandbox(#context{}, #chef_sandbox{}) -> {ok, 1 | 2} |
                                                     not_found |
                                                     {error, any()}.
remove_sandbox(#context{}=Ctx, #chef_sandbox{}=Sandbox) ->
    delete_object(Ctx, delete_sandbox, Sandbox).

%% @doc Returns a list of all the checksums in the given sandbox that have not yet been
%% marked as uploaded.
-spec non_uploaded_checksums(#context{}, #chef_sandbox{}) -> [binary()] | {error, term()}.
non_uploaded_checksums(#context{reqid=ReqId}, #chef_sandbox{id=SandboxId,
                                                            org_id=OrgId}) ->
    ?SH_TIME(ReqId, chef_sql, non_uploaded_checksums, (SandboxId, OrgId)).

%% @doc Given an OrgId and a list of checksums, marks them all as having been uploaded to the system
-spec mark_checksums_as_uploaded(#context{}, binary(), [binary()]) -> ok | {error, term()}.
mark_checksums_as_uploaded(#context{reqid=ReqId}, OrgId, Checksums) ->
    ?SH_TIME(ReqId, chef_sql, mark_checksums_as_uploaded, (OrgId, Checksums)).

-spec node_record_to_authz_id(any(), any()) -> id().
node_record_to_authz_id(_Context, NodeRecord) ->
    NodeRecord#chef_node.authz_id.

-spec count_nodes(#context{}) -> integer() | {error, _}.
count_nodes(#context{reqid = ReqId} = _Ctx) ->
    case stats_hero:ctime(ReqId, {chef_sql, count_nodes},
                          fun() ->
                                  chef_sql:count_nodes()
                          end) of
        {ok, none} ->
            0;
        {ok, Number} ->
            Number;
        {error, Error} ->
            {error, Error}
    end.

-spec is_user_in_org(#context{}, binary(), binary()) -> boolean() | {error, _}.
is_user_in_org(#context{darklaunch = Darklaunch} = Ctx, User, OrgName) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_associations">>, Darklaunch) of
        true ->
            cdb_is_user_in_org(Ctx, User, OrgName);
        false ->
            sql_is_user_in_org(Ctx, User, OrgName)
    end.

cdb_is_user_in_org(#context{reqid = ReqId, otto_connection = S}=Ctx, User, OrgName) ->
    case fetch(#chef_user{username = User}, Ctx) of
        #chef_user{id = UserId} ->
            ?SH_TIME(ReqId, chef_otto, is_user_in_org, (S, UserId, OrgName));
        not_found ->
            false;
        {error, Why} ->
            {error, Why}
    end.

sql_is_user_in_org(#context{reqid = ReqId}, UserName, OrgName) ->
    ?SH_TIME(ReqId, chef_sql, is_user_in_org, (UserName, OrgName)).

connect() ->
    chef_otto:connect().

-spec bulk_get(#context{}, binary(), chef_type(), [binary()]) ->
                      [binary()|ej:json_object()] | {error, _}.
%% @doc Return a list of JSON/gzip'd JSON as binary corresponding to the specified list of
%% IDs.
bulk_get(#context{reqid = ReqId}, _OrgName, node, Ids) ->
    bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_nodes, (Ids)));
bulk_get(#context{reqid = ReqId}, _OrgName, role, Ids) ->
    bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_roles, (Ids)));
bulk_get(#context{reqid = ReqId}, _OrgName, data_bag_item, Ids) ->
    bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_data_bag_items, (Ids)));
bulk_get(#context{reqid = ReqId}, _OrgName, environment, Ids) ->
    bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_environments, (Ids)));
bulk_get(#context{reqid = ReqId}, OrgName, client, Ids) ->
    ClientRecords = bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_clients, (Ids))),
    [chef_client:assemble_client_ejson(C, OrgName) || #chef_client{}=C <- ClientRecords];

% TODO: Can this come out now?
bulk_get(Ctx, OrgName, Type, Ids) ->
    bulk_get_couchdb(Ctx, OrgName, Type, Ids).

bulk_get_result({ok, not_found}) ->
    [];
bulk_get_result({ok, L}) when is_list(L) ->
    L;
bulk_get_result({error, _Why}=Error) ->
    Error.

bulk_get_couchdb(#context{reqid = ReqId, otto_connection = S}=Ctx, OrgName, _Type, Ids) ->
    %% presently searching for non-nodes goes to couchdb
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            {error, {not_found, org}};
        OrgId ->
            DbName = chef_otto:dbname(OrgId),
            ?SH_TIME(ReqId, chef_otto, bulk_get, (S, DbName, Ids))
    end.

-spec data_bag_exists(#context{}, binary(), binary()) -> boolean().
%% @doc Return true if data bag `DataBag' exists in org `OrgName' and false otherwise.
data_bag_exists(#context{}=Ctx, OrgId, DataBag) ->
    case fetch(#chef_data_bag{org_id = OrgId, name = DataBag}, Ctx) of
        #chef_data_bag{} -> true;
        not_found -> false
    end.

-spec environment_exists(#context{}, <<_:256>>, binary()) -> boolean().
%% @doc Return true if environment `EnvName' exists in org `OrgId' and false otherwise.
environment_exists(#context{}=Ctx, OrgId, EnvName) ->
    %% FIXME: we should implement a specialized environment exists function
    case fetch(#chef_environment{org_id = OrgId, name = EnvName}, Ctx) of
        #chef_environment{} -> true;
        _ -> false
    end.

%% -------------------------------------
%% private functions
%% -------------------------------------

-spec create_object(DbContext :: #context{},
                    Fun :: create_fun(),
                    Object :: chef_object() |
                              #chef_user{} |
                              #chef_sandbox{} |
                              #chef_cookbook_version{},
                    ActorId :: object_id()) -> ok |
                                               {conflict, _} |
                                               {error, any()}.
%% @doc Generic object creation with metrics.  `Fun' is the function in the `chef_sql'
%% module to use for the creation and determines the return type (will return the
%% appropriate chef object record type).
create_object(#context{reqid = ReqId}, Fun, Object, ActorId) ->
    Object1 = chef_object:set_created(Object, ActorId),
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(Object1) end) of
        {ok, 1} -> ok;
        {conflict, Msg}-> {conflict, Msg};
        {error, Why} -> {error, Why}
    end.

-spec fetch_objects(#context{}, atom(),
                    binary() | {id, object_id()}) -> {not_found, org} |
                                                     [binary() | versioned_cookbook() ] |
                                                     {error, any()}.
%% @doc Generic listing of a Chef object type. `Fun' is a function in the `chef_sql'
%% module. Returns a list of object names.
fetch_objects(#context{reqid = ReqId}, Fun, {id, OrgId}) ->
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(OrgId) end) of
        {ok, L} when is_list(L) ->
            L;
        {error, Error} ->
            {error, Error}
    end;
fetch_objects(#context{} = Ctx, Fun, OrgName) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found -> {not_found, org};
        OrgId -> fetch_objects(Ctx, Fun, {id, OrgId})
    end.

-spec fetch_objects(#context{}, atom(), binary() | {id, object_id()},
                    binary()) -> {not_found, org} |
                                 [binary() | versioned_cookbook()] |
                                 {error, any()}.
%% @doc Generic listing of a Chef object type with filtering. `Fun' is the appropriate
%% function in the `chef_sql' module. This version is used to fetch nodes within a specified
%% `environment' as well as `data_bag_items' within a specified `data_bag'.
fetch_objects(#context{reqid = ReqId}, Fun, {id, OrgId}, Arg) ->
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(OrgId, Arg) end) of
        {ok, L} when is_list(L) ->
            L;
        {error, Error} ->
            {error, Error}
    end;
fetch_objects(#context{}=Ctx, Fun, OrgName, Arg) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found -> {not_found, org};
        OrgId -> fetch_objects(Ctx, Fun, {id, OrgId}, Arg)
    end.

%% FIXME: seems like delete_object should take either object ID or orgid+object_name only.
%% Also might want to take ActorId here and at least log who deleted the object.
-spec delete_object(DbContext :: #context{},
                    Fun :: delete_fun(),
                    Object :: chef_object() | object_id() | #chef_user{} | #chef_client{} | #chef_sandbox{} |
                              #chef_cookbook_version{} ) -> {ok, 1 | 2} |
                                                            not_found |
                                                            #chef_db_cb_version_delete{} |
                                                            {error, _}.
%% @doc Delete a object. You can provide either a `#chef_object{}' record or just the ID of
%% the object.

%% SPECIAL CASE - We need an OrgId and Name in addition to the Id when deleting
%% cookbook versions.
delete_object(#context{reqid = ReqId}, Fun, #chef_cookbook_version{} = CookbookVersion) ->
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(CookbookVersion) end) of
        {ok, not_found} -> not_found;
        Result -> Result
    end;
delete_object(#context{}=Ctx, Fun, Object) when is_tuple(Object) ->
    delete_object(Ctx, Fun, chef_object:id(Object));
delete_object(#context{reqid = ReqId}, Fun, Id) ->
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(Id) end) of
        {ok, not_found} -> not_found;
        Result -> Result
    end.

-spec update_object(#context{}, object_id(), update_fun(),
                    chef_updatable_object() |
                    #chef_user{}) ->  ok |
                                      not_found |
                                      {conflict, any()} |
                                      {error, any()} |
                                      #chef_db_cb_version_update{}.
%% @doc Generic update for Chef object types. `Fun' is the appropriate function in the
%% `chef_sql' module. `Object' is a Chef object (record) with updated data.
update_object(#context{reqid = ReqId}, ActorId, Fun, Object) ->
    Object1 = chef_object:set_updated(Object, ActorId),
    case stats_hero:ctime(ReqId, {chef_sql, Fun},
                          fun() -> chef_sql:Fun(Object1) end) of
        #chef_db_cb_version_update{}=CookbookVersionUpdate -> CookbookVersionUpdate;
        {ok, 1} -> ok;
        {ok, not_found} -> not_found;
        {conflict, Message} -> {conflict, Message};
        {error, Error} -> {error, Error}
    end.

%% @doc Make a dict mapping an object's unique name to its database ID for all objects
%% within a given "index". (This is currently only used for reindexing, so it only works on
%% items that are indexed.)  An index that is a binary is taken to be a data bag name, in
%% which case, the dict will map data bag item ID to database ID for all items within that
%% data bag.
-spec create_name_id_dict(#context{},
                          Index :: node | role | client | environment | binary(),
                          OrgId :: object_id()) ->
                                 dict() | {error, term()}.
create_name_id_dict(#context{reqid=ReqId}, Index, OrgId) ->
    case ?SH_TIME(ReqId, chef_sql, create_name_id_dict, (OrgId, Index)) of
        {ok, D} ->
            D;
        {error, Error} ->
            {error, Error}
    end.
