%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Daniel Deleo <dan@opscode.com>
%% @author Mark Anderson <mark@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
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


-module(chef_db).

-export([
         user_record_to_authz_id/2,
         %% fetch_org/2,
         fetch_org_id/2,
         client_record_to_authz_id/2,

         fetch_requestor/3,

         %% Checksum ops
         mark_checksums_as_uploaded/3,

         %% user ops
         fetch_user/2,
         fetch_users/1,
         create_user/3,
         delete_user/2,
         count_user_admins/1,

         %% node ops
         fetch_node/3,
         fetch_nodes/2,
         fetch_nodes/3,
         create_node/3,
         delete_node/2,
         node_record_to_authz_id/2,

         %% role ops
         fetch_role/3,
         fetch_roles/2,
         create_role/3,
         delete_role/2,
         %% role_record_to_authz_id/2,

         %% environment ops
         fetch_environment/3,
         fetch_environments/2,
         create_environment/3,
         delete_environment/2,

         %% client ops
         fetch_client/3,
         fetch_clients/2,
         create_client/3,
         delete_client/2,

         %% data_bag ops
         fetch_data_bag/3,
         fetch_data_bags/2,
         create_data_bag/3,
         delete_data_bag/2,

         %% data_bag_item ops
         fetch_data_bag_item/4,
         fetch_data_bag_items/3,
         fetch_data_bag_item_ids/3,
         create_data_bag_item/3,
         delete_data_bag_item/2,

         %% cookbook_version ops
         cookbook_exists/3,

         fetch_cookbook_version/3,
         fetch_cookbook_versions/2,
         fetch_cookbook_versions/3,
         fetch_latest_cookbook_version/3,
         create_cookbook_version/3,
         delete_cookbook_version/2,
         fetch_latest_cookbook_versions/2,
         fetch_latest_cookbook_versions/3,
         fetch_latest_cookbook_recipes/2,
         fetch_all_cookbook_version_dependencies/2,
         fetch_environment_filtered_cookbook_versions/5,
         fetch_environment_filtered_recipes/3,

         %% Sandbox ops
         make_sandbox/4,
         fetch_sandbox/3,
         commit_sandbox/2,

         %% fetch_orgs_for_user/2,

         is_user_in_org/3,
         connect/0,
         create/3,
         update/3,
         bulk_get/4,
         data_bag_exists/3,
         data_bag_names/2,
         environment_exists/3,
         make_context/1,
         make_context/2]).

-include_lib("chef_db/include/chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-record(context, {reqid :: binary(),
                  otto_connection}).

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

%% -type chef_object_name() :: 'chef_node' |
%%                             'chef_role'.

%% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%% -endif.

make_context(ReqId) ->
    #context{reqid = ReqId, otto_connection = chef_otto:connect()}.

make_context(ReqId, OttoServer) ->
    #context{reqid = ReqId, otto_connection = OttoServer}.

%%%
%%% User access
%%%
-spec fetch_user(#context{}, binary() | string()) -> #chef_user{} |
                                                     not_found |
                                                     {error, term()}.
fetch_user(#context{reqid = ReqId, otto_connection = _Server} = _Context, UserName) ->
    case ?SH_TIME(ReqId, chef_sql, fetch_user, (UserName)) of
        {ok, not_found} ->
            not_found;
        {ok, #chef_user{}=User} ->
            User;
        {error, Error} ->
            {error, Error}
    end.

-spec fetch_users(#context{}) -> [binary()] | {error, _}.
fetch_users(#context{reqid = ReqId}) ->
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, fetch_users),
                          fun() -> chef_sql:fetch_users() end) of
        {ok, L} -> L;
        Other -> Other
    end.

-spec count_user_admins(#context{}) -> integer() | {error, term()}.
count_user_admins(#context{reqid = ReqId}) ->
  case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, count_user_admins),
                        fun() -> chef_sql:count_user_admins() end) of
       {ok, Count} -> Count;
       Other -> Other
  end.

%%%
-spec user_record_to_authz_id(#context{}, #chef_user{} | not_found) -> id().
user_record_to_authz_id(#context{}, #chef_user{} = UserRecord) ->
    UserRecord#chef_user.authz_id;
user_record_to_authz_id(#context{}, not_found) ->
    %% FIXME: is this what we want here?
    erlang:error({error, not_found}).

%% fetch_org(S, OrgName) ->
%%     chef_otto:fetch_org(S, OrgName).

-spec fetch_org_id(#context{}, binary() | ?OSC_ORG_NAME) -> not_found | binary().
fetch_org_id(_, ?OSC_ORG_NAME) ->
    ?OSC_ORG_ID;
fetch_org_id(#context{reqid = ReqId,
                      otto_connection = Server}, OrgName) when is_binary(OrgName) ->
    case chef_cache:get(org_guid, OrgName) of
        Error when Error =:= not_found orelse Error =:= no_cache ->
            case ?SH_TIME(ReqId, chef_otto, fetch_org_id, (Server, OrgName)) of
                not_found ->
                    not_found;
                Guid ->
                    chef_cache:put(org_guid, OrgName, Guid),
                    Guid
            end;
        {ok, Guid} ->
            Guid
    end.

%%% Client access
%%%
fetch_couchdb_client(#context{} = _Context, not_found, _ClientName) ->
    not_found;
fetch_couchdb_client(#context{otto_connection = Server} = Context, OrgName, ClientName) ->
    case fetch_org_id(Context, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            case chef_otto:fetch_client(Server, OrgId, ClientName) of
                {not_found, _} -> not_found;
                Other -> Other
            end
    end.

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
                      binary(),
                      binary()) -> #chef_client{} | #chef_user{} |
                                   %% TODO: fix chef_wm so we can just return 'not_found'
                                   {'not_found', 'client'}.
fetch_requestor(Context, OrgName, ClientName) ->
    case fetch_client(Context, OrgName, ClientName) of
        not_found ->
            case fetch_user(Context, ClientName) of
                not_found ->
                    %% back compat for now until we update chef_wm
                    {not_found, client};
                #chef_user{} = User ->
                    User
            end;
        #chef_client{} = Client ->
            Client
    end.

-spec create_node(#context{}, #chef_node{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new node in the datastore.
create_node(#context{}=Ctx, Node, ActorId) ->
    create_object(Ctx, create_node, Node, ActorId).

-spec create_role(#context{}, #chef_role{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new role in the datastore.
create_role(#context{}=Ctx, Role, ActorId) ->
    create_object(Ctx, create_role, Role, ActorId).

-spec create_user(#context{}, #chef_user{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new user in the datastore.
create_user(#context{}=Ctx, User, ActorId) ->
    create_object(Ctx, create_user, User, ActorId).

-spec create_environment(#context{}, #chef_environment{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new environment in the datastore.
create_environment(#context{}=Ctx, Environment, ActorId) ->
    create_object(Ctx, create_environment, Environment, ActorId).

-spec create_client(#context{}, #chef_client{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new client in the datastore.
create_client(#context{}=Ctx, Client, ActorId) ->
    create_object(Ctx, create_client, Client, ActorId).

-spec create_data_bag(#context{}, #chef_data_bag{}, object_id()) -> 'ok' |
                                                                    {'conflict', _} |
                                                                    {'error', _}.
%% @doc Store a new data_bag in the datastore.
create_data_bag(#context{}=Ctx, #chef_data_bag{}=DataBag, ActorId) ->
    create_object(Ctx, create_data_bag, DataBag, ActorId).

-spec create_data_bag_item(#context{}, #chef_data_bag_item{}, object_id()) -> ok | {conflict, term()} | term().
%% @doc Store a new data_bag_item in the datastore.
create_data_bag_item(#context{}=Ctx, DataBagItem, ActorId) ->
    create_object(Ctx, create_data_bag_item, DataBagItem, ActorId).

-spec create_cookbook_version(DbContext :: #context{},
                              CBVersion :: #chef_cookbook_version{},
                              ActorId :: object_id()) -> ok | {conflict, _} | {error, _}.
%% @doc Store a new cookbook_version in the datastore.
create_cookbook_version(#context{}=Ctx, CookbookVersion, ActorId) ->
    create_object(Ctx, create_cookbook_version, CookbookVersion, ActorId).

%% Currently, this is only used internally, but will be useful for future enhancements to
%% the sandboxes protocol that allow for partial progress monitoring (e.g., upload a few
%% files, retrieve the current state of the sandbox, upload a few more, etc.)
-spec fetch_sandbox(DbContext::#context{},
                    OrgName::binary(),
                    SandboxId::object_id()) -> #chef_sandbox{} |
                                               not_found |
                                               {error, any()}.
fetch_sandbox(#context{} = Ctx, OrgName, SandboxId) ->
    fetch_object(Ctx, chef_sandbox, OrgName, SandboxId).

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
            Id = chef_object:make_org_prefix_id(OrgId),
            %% TempSandbox doesn't know if the checksums have been uploaded yet or not
            TempSandbox = #chef_sandbox{id=Id,
                                        org_id=OrgId,
                                        checksums = [{C, false} || C <- Checksums]},
            %% TODO: ActorId isn't actually needed in the code, but it is if we want to use common code paths
            case create_object(Ctx, create_sandbox, TempSandbox, ActorId) of
                ok ->
                    %% this sandbox will know if the checksums have been uploaded
                    fetch_sandbox(Ctx, OrgName, Id);
                {conflict, Msg} ->
                    {conflict, Msg};
                {error, Why} ->
                    {error, Why}
            end
    end.

-spec fetch_environment(#context{},
                        OrgName :: binary() | {id, object_id()},
                        EnvironmentName :: binary()) -> #chef_environment{} |
                                                        not_found |
                                                        {error, _}.
%% @doc Return the environment in `OrgName' with name `EnvironmentName'. Returns a
%% `#chef_environment{}' record.
fetch_environment(#context{} = Ctx, OrgName, EnvironmentName) ->
    fetch_object(Ctx, chef_environment, OrgName, EnvironmentName).

-spec fetch_client(#context{}, binary(), binary()) -> #chef_client{} |
                                                      not_found | {error, _}.
%% @doc Return the client in `OrgName' with name `ClientName'. Returns a
%% `#chef_client{}' record.
fetch_client(#context{} = Ctx, OrgName, ClientName) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_clients">>) of
        true ->
            fetch_couchdb_client(Ctx, OrgName, ClientName);
        false ->
            fetch_object(Ctx, chef_client, OrgName, ClientName)
    end.

-spec fetch_node(#context{}, binary(), binary()) -> #chef_node{} |
                                                    not_found |
                                                    {error, _}.
%% @doc Return the node in `OrgName' with name `NodeName'. Returns a `#chef_node{}' record.
fetch_node(#context{} = Ctx, OrgName, NodeName) ->
    fetch_object(Ctx, chef_node, OrgName, NodeName).

-spec fetch_role(#context{}, binary(), binary()) -> #chef_role{} |
                                                    not_found |
                                                    {error, _}.
%% @doc Return the role in `OrgName' with name `RoleName'. Returns a `#chef_role{}' record.
fetch_role(#context{} = Ctx, OrgName, RoleName) ->
    fetch_object(Ctx, chef_role, OrgName, RoleName).

-spec fetch_data_bag(#context{}, binary(), binary()) -> #chef_data_bag{} |
                                                        not_found |
                                                        {error, _}.
%% @doc Return the data_bag in `OrgName' with name `DataBagName'. Returns a
%% `#chef_data_bag{}' record.
fetch_data_bag(#context{} = Ctx, OrgName, DataBagName) ->
    fetch_object(Ctx, chef_data_bag, OrgName, DataBagName).

-spec fetch_data_bag_item(#context{}, binary(), binary(), binary()) ->
                                 #chef_data_bag_item{} |
                                 not_found |
                                 {error, _}.
%% @doc Return the data_bag_item in the `DataBagName' data_bag in the `OrgName'
%% org.. Returns a `#chef_data_bag_item{}' record.
fetch_data_bag_item(#context{reqid = ReqId} = Ctx, OrgName, DataBagName, ItemName) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            %% FIXME: do we want to indicate that the issue is the org not found?
            %%    {not_found, org};
            not_found;
        OrgId ->
            case ?SH_TIME(ReqId, chef_sql, fetch_data_bag_item, (OrgId, DataBagName,
                                                                 ItemName)) of
                {ok, not_found} ->
                    not_found;
                {ok, #chef_data_bag_item{}=DataBagItem} ->
                    DataBagItem;
                {error, _Why} = Error ->
                    Error
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
            stats_hero:ctime(ReqId, stats_hero:label(chef_sql, fetch_cookbook_version),
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
         stats_hero:ctime(ReqId, stats_hero:label(chef_sql, fetch_latest_cookbook_version),
                          fun() -> chef_sql:fetch_latest_cookbook_version(OrgId, CookbookName)
                          end)
    end.

%% @doc Retrieve a list of {Name, Verison} tuples for the latest version of each cookbook in
%% an organization.  Version information is returned as a binary string (e.g., <<"1.0.0">>)
%% instead of the tuple form (e.g. {1,0,0}) that is used elsewhere.
-spec fetch_latest_cookbook_versions(DbContext :: #context{},
                                     OrgName :: binary()) ->
                                            [{CookbookName :: binary(),
                                              VersionString :: binary()}] |
                                            {not_found, org} |
                                            {error, Reason :: term()}.
fetch_latest_cookbook_versions(#context{}=DbContext, OrgName) ->
    fetch_latest_cookbook_versions(DbContext, OrgName, 1).

%% @doc Same as fetch_latest_cookbook_versions/2, but allows you to specify how many
%% versions for which to fetch information.  For instance, setting `NumberOfVersions' to `3'
%% will retrieve the {Name, Version} pairs for the three most recent versions of each
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
            case ?SH_TIME(ReqId, chef_sql, fetch_environment_filtered_cookbook_versions, (OrgId, EnvName, CookbookName, NumVersions)) of
                {ok, Results} ->
                    Results;
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Given an environment in an organization, return a sorted list
%% of qualified recipe names for the cookbook versions that best match
%% the environment's version constraints, if any.
-spec fetch_environment_filtered_recipes(DbContext :: #context{},
                                         OrgName :: binary(),
                                         EnvName :: binary()) ->
                                                [QualifiedRecipeName :: binary()].
fetch_environment_filtered_recipes(#context{reqid=ReqId}=DbContext, OrgName, EnvName) ->
    case fetch_org_id(DbContext, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            case ?SH_TIME(ReqId, chef_sql, fetch_environment_filtered_recipes, (OrgId, EnvName)) of
                {ok, Results} ->
                    Results;
                {error, Error} ->
                    {error, Error}
            end
    end.

-spec fetch_environments(#context{}, binary()) -> {not_found, org} |
                                                  [binary()] |
                                                  {error, any()}.
%% @doc Return a list of all environment names in an org
fetch_environments(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_environments, OrgName).

-spec fetch_clients(#context{}, binary()) -> {not_found, org} |
                                             [binary()] | {error, any()}.
%% @doc Return a list of all client names in an org
%%
fetch_clients(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_clients, OrgName).

-spec fetch_data_bags(#context{}, binary() | {id, object_id()}) -> [binary()] |
                                                                   {not_found, org} |
                                                                   {error, any()}.
%% @doc Return a list of all data_bag names in an org
fetch_data_bags(#context{} = Ctx, OrgName) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_data">>) of
        true ->
            fetch_couchdb_data_bags(Ctx, OrgName);
        false ->
            fetch_objects(Ctx, fetch_data_bags, OrgName)
    end.

-spec fetch_nodes(#context{}, binary()) -> {not_found, org} |
                                           [binary()] |
                                           {error, any()}.
%% @doc Return a list of all node names in an org
fetch_nodes(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_nodes, OrgName).

-spec fetch_roles(#context{}, binary()) -> {not_found, org} |
                                           [binary()] |
                                           {error, any()}.
%% @doc Return a list of all role names in an org
fetch_roles(#context{} = Ctx, OrgName) ->
    fetch_objects(Ctx, fetch_roles, OrgName).

-spec fetch_nodes(#context{}, binary(), binary()) -> [binary()] |
                                                     {not_found, org} |
                                                     {error, _}.
%% @doc Returns list of node names for `OrgName' that are in the `EnvName' environment.
fetch_nodes(#context{} = Ctx, OrgName, EnvName) ->
    fetch_objects(Ctx, fetch_nodes, OrgName, EnvName).

-spec fetch_data_bag_items(#context{}, binary(), binary()) -> [binary()] |
                                                              {not_found, org} |
                                                              {error, _}.
%% @doc Returns list of data_bag_item names in `DataBagName' for `OrgName'.
fetch_data_bag_items(#context{} = Ctx, OrgName, DataBagName) ->
    fetch_objects(Ctx, fetch_data_bag_items, OrgName, DataBagName).

-spec fetch_data_bag_item_ids(#context{}, binary() | {id, object_id()},
                              binary()) -> [binary()] |
                                           {not_found, org} |
                                           {error, _}.
%% @doc Returns list of data_bag_item names in `DataBagName' for `OrgName'.
fetch_data_bag_item_ids(#context{} = Ctx, OrgName, DataBagName) ->
    fetch_objects(Ctx, fetch_data_bag_item_ids, OrgName, DataBagName).

%% @doc Delete a cookbook version
-spec delete_cookbook_version(Ctx :: #context{},
                              CookbookVersion :: #chef_cookbook_version{}) ->
                                {ok, 1 | 2} | not_found | {error, term()}.
delete_cookbook_version(#context{}=Ctx, #chef_cookbook_version{org_id=OrgId}=CookbookVersion) ->
    case delete_object(Ctx, delete_cookbook_version, CookbookVersion) of
        #chef_db_cb_version_delete{cookbook_delete=CookbookDeleted, deleted_checksums=DeletedChecksums} ->
            chef_s3:delete_checksums(OrgId, DeletedChecksums),
            %% TODO: return the actual chef_db_cb_version_delete record to the caller
            case CookbookDeleted of
                false -> {ok, 1};
                true -> {ok, 2}
            end;
        Result -> Result %% not_found or {error, _}
    end.

-spec delete_node(#context{}, #chef_node{}) -> {ok, 1 | 2} | not_found | {error, _}.
%% @doc Delete a node. You can provide either a `#chef_node{}' record or just the ID of the
%% node.
delete_node(#context{}=Ctx, #chef_node{}=Node) -> delete_object(Ctx, delete_node, Node).

-spec delete_user(#context{}, #chef_user{}) -> {ok, 1 | 2 } | not_found | {error, _}.
delete_user(#context{}=Ctx, #chef_user{}=User) ->
  delete_object(Ctx, delete_user, User).

-spec delete_role(#context{}, #chef_role{}) -> {ok, 1 | 2} | not_found | {error, _}.
%% @doc Delete a role. You can provide either a `#chef_role{}' record or just the ID of the
%% role.
delete_role(#context{}=Ctx, #chef_role{}=Role) -> delete_object(Ctx, delete_role, Role).

-spec delete_environment(#context{}, #chef_environment{}) -> {ok, 1 | 2} |
                                                             not_found |
                                                             {error, _}.
%% @doc Delete a environment. You can provide either a `#chef_environment{}' record or just
%% the ID of the environment.
delete_environment(#context{}=Ctx, #chef_environment{}=Environment) ->
    delete_object(Ctx, delete_environment, Environment).

-spec delete_client(#context{}, #chef_client{}) -> {ok, 1 | 2} |
                                                   not_found |
                                                   {error, _}.
%% @doc Delete a client. You can provide either a `#chef_client{}' record or just
%% the ID of the client.
delete_client(#context{}=Ctx, #chef_client{}=Client) ->
    delete_object(Ctx, delete_client, Client).

-spec delete_data_bag(#context{}, #chef_data_bag{}) -> {ok, 1 | 2} |
                                                       not_found |
                                                       {error, _}.
%% @doc Delete a data_bag. You can provide either a `#chef_data_bag{}' record or just
%% the ID of the data_bag.
delete_data_bag(#context{}=Ctx, #chef_data_bag{}=DataBag) ->
    delete_object(Ctx, delete_data_bag, DataBag).

-spec delete_data_bag_item(#context{}, #chef_data_bag_item{}) -> {ok, 1 | 2} |
                                                                 not_found |
                                                                 {error, _}.
%% @doc Delete a data_bag_item. You can provide either a `#chef_data_bag_item{}' record or just
%% the ID of the data_bag_item.
delete_data_bag_item(#context{}=Ctx, #chef_data_bag_item{}=DataBagItem) ->
    delete_object(Ctx, delete_data_bag_item, DataBagItem).


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

-spec is_user_in_org(#context{}, binary(), binary()) -> boolean() | {error, _}.
is_user_in_org(#context{reqid = ReqId, otto_connection = S}=Ctx, User, OrgName) ->
    case fetch_user(Ctx, User) of
        #chef_user{id = UserId} ->
            ?SH_TIME(ReqId, chef_otto, is_user_in_org, (S, UserId, OrgName));
        not_found ->
            false;
        {error, Why} ->
            {error, Why}
    end.

connect() ->
    chef_otto:connect().

-spec bulk_get(#context{}, binary(), chef_type(), [binary()]) ->
                      [binary()|ej:json_object()] | {error, _}.
%% @doc Return a list of JSON/gzip'd JSON as binary corresponding to the specified list of
%% IDs.
bulk_get(#context{reqid = ReqId}, _OrgName, node, Ids) ->
    bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_nodes, (Ids)));
bulk_get(#context{reqid = ReqId}=Ctx, OrgName, role, Ids) ->
    %% TODO: remove after roles migration to Erchef/SQL
    case chef_db_darklaunch:is_enabled(<<"couchdb_roles">>, OrgName) of
        true ->
            bulk_get_couchdb(Ctx, OrgName, role, Ids);
        false ->
            bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_roles, (Ids)))
    end;
bulk_get(#context{reqid = ReqId}=Ctx, OrgName, data_bag_item, Ids) ->
    %% TODO: remove after data_bag data_bag_item migration to Erchef/SQL
    case chef_db_darklaunch:is_enabled(<<"couchdb_data">>, OrgName) of
        true ->
            bulk_get_couchdb(Ctx, OrgName, data_bag_item, Ids);
        false ->
            bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_data_bag_items, (Ids)))
    end;
bulk_get(#context{reqid = ReqId}=Ctx, OrgName, environment, Ids) ->
    %% TODO: remove after environments migration to Erchef/SQL
    case chef_db_darklaunch:is_enabled(<<"couchdb_environments">>, OrgName) of
        true ->
            bulk_get_couchdb(Ctx, OrgName, environment, Ids);
        false ->
            bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_environments, (Ids)))
    end;
bulk_get(#context{reqid = ReqId}=Ctx, OrgName, client, Ids) ->
    %% TODO: remove after environments migration to Erchef/SQL
    case chef_db_darklaunch:is_enabled(<<"couchdb_clients">>, OrgName) of
        true ->
            bulk_get_couchdb(Ctx, OrgName, client, Ids);
        false ->
            ClientRecords = bulk_get_result(?SH_TIME(ReqId, chef_sql, bulk_get_clients, (Ids))),
            [chef_client:assemble_client_ejson(C, OrgName) || #chef_client{}=C <- ClientRecords]
    end;
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

-spec data_bag_exists(#context{}, binary(), binary() | string()) -> boolean().
%% @doc Return true if data bag `DataBag' exists in org `OrgName' and false otherwise.
data_bag_exists(#context{reqid = ReqId, otto_connection = S}=Ctx, OrgName, DataBag) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_data">>, OrgName) of
        true ->
            case fetch_org_id(Ctx, OrgName) of
                not_found ->
                    false;
                OrgId ->
                    ?SH_TIME(ReqId, chef_otto, data_bag_exists, (S, OrgId, DataBag))
            end;
        false ->
            case fetch_data_bag(Ctx, OrgName, DataBag) of
                #chef_data_bag{} -> true;
                not_found -> false
            end
    end.

%% @doc Return a lit of the names of all an organization's data bags.
-spec data_bag_names(#context{}, OrgId::object_id()) ->  [binary()] |
                                                         {not_found, org} |
                                                         {error, any()}.
data_bag_names(#context{}=Ctx, OrgId) ->
    fetch_data_bags(Ctx, {id, OrgId}).

-spec environment_exists(#context{}, binary(), binary() | string()) -> boolean().
%% @doc Return true if environment `EnvName' exists in org `OrgId' and false otherwise.
environment_exists(#context{reqid = ReqId, otto_connection = S}=Ctx, OrgId, EnvName) ->
    case chef_db_darklaunch:is_enabled(<<"couchdb_environments">>) of
        true ->
            ?SH_TIME(ReqId, chef_otto, environment_exists, (S, OrgId, EnvName));
        false ->
            %% FIXME: we should implement a specialized environment exists function
            case fetch_environment(Ctx, {id, OrgId}, EnvName) of
                #chef_environment{} -> true;
                _ -> false
            end
    end.

-spec create(chef_object() | #chef_user{} | #chef_sandbox{}, #context{}, object_id()) -> ok | {conflict, term()} | {error, term()}.
%% @doc Call the appropriate create function based on the given chef_object record
create(#chef_data_bag{} = Record, DbContext, ActorId) ->
    create_data_bag(DbContext, Record, ActorId);
create(#chef_data_bag_item{} = Record, DbContext, ActorId) ->
    create_data_bag_item(DbContext, Record, ActorId);
create(#chef_environment{} = Record, DbContext, ActorId) ->
    create_environment(DbContext, Record, ActorId);
create(#chef_client{} = Record, DbContext, ActorId) ->
  create_client(DbContext, Record, ActorId);
create(#chef_node{} = Record, DbContext, ActorId) ->
    create_node(DbContext, Record, ActorId);
create(#chef_user{} = Record, DbContext, ActorId) ->
    create_user(DbContext, Record, ActorId);
create(#chef_role{} = Record, DbContext, ActorId) ->
    create_role(DbContext, Record, ActorId);
create(#chef_cookbook_version{} = Record, DbContext, ActorId) ->
    create_cookbook_version(DbContext, Record, ActorId).

-spec update(#context{}, chef_updatable_object() | #chef_user{}, object_id()) ->
             ok | not_found | {conflict, term()} | {error, term()}.
update(DbContext, #chef_data_bag_item{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_data_bag_item, Record);
update(DbContext, #chef_environment{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_environment, Record);
update(DbContext, #chef_client{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_client, Record);
update(DbContext, #chef_node{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_node, Record);
update(DbContext, #chef_role{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_role, Record);
update(DbContext, #chef_cookbook_version{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_cookbook_version, Record);
update(DbContext, #chef_user{} = Record, ActorId) ->
    update_object(DbContext, ActorId, update_user, Record).

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
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
                          fun() -> chef_sql:Fun(Object1) end) of
        {ok, 1} -> ok;
        {conflict, Msg}-> {conflict, Msg};
        {error, Why} -> {error, Why}
    end.

%% @doc Generic fetching of a single Chef object. `Fun' is the name of a function in the
%% `chef_sql' module to use to fetch for the specified `OrgIdentifier' is either the binary
%% name of the org, or it is an `{id, OrgId}' tuple.  In most cases, `ObjectIdentifier',
%% will just be the name of the object.  In the case of a Cookbook Version, however, it is a
%% `{Name, Version}' tuple.
%%
%% TODO: Fold data_bag_item back into this
-spec fetch_object(DbContext :: #context{},
                   ObjectType :: atom(),
                   OrgIdentifier :: binary() | {id, object_id()},
                   ObjectIdentifier :: binary() |
                                       versioned_cookbook()) -> not_found |
                                                                #chef_client{} |
                                                                #chef_environment{} |
                                                                #chef_data_bag{} |
                                                                %% #chef_data_bag_item{} handled separately
                                                                #chef_node{} |
                                                                #chef_role{} |
                                                                #chef_sandbox{} |
                                                                {error, any()}.
fetch_object(#context{reqid = ReqId}, ObjectType, {id, OrgId}, ObjectIdentifier) ->
    Fun = fetch_query_for_type(ObjectType),
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
                          fun() -> chef_sql:Fun(OrgId, ObjectIdentifier) end) of
        {ok, not_found} -> not_found;
        {ok, Object} -> assert_chef_object(Object, ObjectType);
        {error, _Why} = Error -> Error
    end;
fetch_object(#context{}=Ctx, ObjectType, OrgName, ObjectIdentifier) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            not_found;
        OrgId ->
            fetch_object(Ctx, ObjectType, {id, OrgId}, ObjectIdentifier)
    end.

%% note that chef_data_bag_item is handled separately because it requires an extra argument
%% to fetch.
fetch_query_for_type(chef_client) ->
    fetch_client;
fetch_query_for_type(chef_data_bag) ->
    fetch_data_bag;
fetch_query_for_type(chef_environment) ->
    fetch_environment;
fetch_query_for_type(chef_node) ->
    fetch_node;
fetch_query_for_type(chef_role) ->
    fetch_role;
fetch_query_for_type(chef_sandbox) ->
    fetch_sandbox.

-define(ASSERT_RECORD(Object, RecName),
        case Object of
            #RecName{} -> Object;
            _ -> error({unexpected_type, {expected, #RecName{}}, {found, Object}})
        end).

assert_chef_object(Object, chef_client) ->
    ?ASSERT_RECORD(Object, chef_client);

assert_chef_object(Object, chef_data_bag) ->
    ?ASSERT_RECORD(Object, chef_data_bag);

%% not used, so left out for now to avoid dialyzer warning
%% assert_chef_object(Object, chef_data_bag_item) ->
%%     ?ASSERT_RECORD(Object, chef_data_bag_item);

assert_chef_object(Object, chef_environment) ->
    ?ASSERT_RECORD(Object, chef_environment);

assert_chef_object(Object, chef_node) ->
    ?ASSERT_RECORD(Object, chef_node);

assert_chef_object(Object, chef_role) ->
    ?ASSERT_RECORD(Object, chef_role);

assert_chef_object(Object, chef_sandbox) ->
    ?ASSERT_RECORD(Object, chef_sandbox).


%% assert_chef_object(Object, chef_data_bag) ->
%%     case Object of
%%         #chef_data_bag{} -> true;
%%         _ -> error({unexpected_type, {expected, #chef_data_bag{}}, {found, Object}})
%%     end;
%% assert_chef_object(Object, chef_data_bag_item) ->
%%     case Object of
%%         #chef_data_bag_item{} -> true;
%%         _ -> error({unexpected_type, {expected, #chef_data_bag_item{}}, {found, Object}})
%%     end;
%% assert_chef_object(Object, chef_environment) ->
%%     case Object of
%%         #chef_environment{} -> true;
%%         _ -> error({unexpected_type, {expected, #chef_environment{}}, {found, Object}})
%%     end;
%% assert_chef_object(Object, chef_node) ->
%%     case Object of
%%         #chef_node{} -> true;
%%         _ -> error({unexpected_type, {expected, #chef_node{}}, {found, Object}})
%%     end;
%% assert_chef_object(Object, chef_role) ->
%%     case Object of
%%         #chef_role{} -> true;
%%         _ -> error({unexpected_type, {expected, #chef_role{}}, {found, Object}})
%%     end.


-spec fetch_objects(#context{}, atom(),
                    binary() | {id, object_id()}) -> {not_found, org} |
                                                     [binary() | versioned_cookbook() ] |
                                                     {error, any()}.
%% @doc Generic listing of a Chef object type. `Fun' is a function in the `chef_sql'
%% module. Returns a list of object names.
fetch_objects(#context{reqid = ReqId}, Fun, {id, OrgId}) ->
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
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
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
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

%% For back-compat with data bags in couchdb, this is used to fetch the data bag names which
%% are valid as part of search queries.
fetch_couchdb_data_bags(#context{} = Ctx, OrgName) when is_binary(OrgName) ->
    case fetch_org_id(Ctx, OrgName) of
        not_found ->
            {not_found, org};
        OrgId ->
            fetch_couchdb_data_bags(Ctx, {id, OrgId})
    end;
fetch_couchdb_data_bags(#context{reqid = ReqId, otto_connection = S}, {id, OrgId}) ->
    ?SH_TIME(ReqId, chef_otto, data_bag_names, (S, OrgId)).

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
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
                          fun() -> chef_sql:Fun(CookbookVersion) end) of
        {ok, not_found} -> not_found;
        Result -> Result
    end;
delete_object(#context{}=Ctx, Fun, Object) when is_tuple(Object) ->
    delete_object(Ctx, Fun, get_id(Object));
delete_object(#context{reqid = ReqId}, Fun, Id) ->
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
                          fun() -> chef_sql:Fun(Id) end) of
        {ok, not_found} -> not_found;
        Result -> Result
    end.

-spec update_object(#context{}, object_id(), update_fun(),
                    chef_updatable_object() |
                    #chef_user{}) ->  ok |
                                      not_found |
                                      {conflict, any()} |
                                      {error, any()}.
%% @doc Generic update for Chef object types. `Fun' is the appropriate function in the
%% `chef_sql' module. `Object' is a Chef object (record) with updated data.
update_object(#context{reqid = ReqId}, ActorId, Fun, Object) ->
    Object1 = chef_object:set_updated(Object, ActorId),
    case stats_hero:ctime(ReqId, stats_hero:label(chef_sql, Fun),
                          fun() -> chef_sql:Fun(Object1) end) of
        {ok, 1} -> ok;
        {ok, not_found} -> not_found;
        {conflict, Message} -> {conflict, Message};
        {error, Error} -> {error, Error}
    end.

  -spec get_id(chef_object() | #chef_user{} | #chef_client{} | #chef_sandbox{} | #chef_cookbook_version{}) -> object_id().
%% @doc Return the `id' field from a `chef_object()' record type.
get_id(#chef_client{id = Id}) ->
    Id;
get_id(#chef_node{id = Id}) ->
    Id;
get_id(#chef_role{id = Id}) ->
    Id;
get_id(#chef_environment{id = Id}) ->
    Id;
get_id(#chef_data_bag{id = Id}) ->
    Id;
get_id(#chef_data_bag_item{id = Id}) ->
    Id;
get_id(#chef_sandbox{id = Id}) ->
    Id;
get_id(#chef_user{username = Username}) ->
    Username.
