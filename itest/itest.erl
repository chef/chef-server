%% Copyright 2012-2013 Opscode, Inc. All Rights Reserved.
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

-module(itest).

-compile([export_all]).

-exports([setup_env/0, basic_test_/0,
          statements/1]).

-define(GET_ARG(Name, Args), proplists:get_value(Name, Args)).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_db/include/chef_db.hrl").
-include_lib("chef_objects/include/chef_types.hrl").


read_db_config() ->
    Path = filename:join([filename:dirname(code:which(?MODULE)), "pgsql.config"]),
    {ok, Config} = file:consult(Path),
    Config.

app_list() ->
    [crypto, public_key, ssl, epgsql, pooler].

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            Error
    end.

setup_env() ->
    Info = read_db_config(),
    itest_util:set_env(sqerl,
                       [{db_host, ?GET_ARG(host, Info)},
                        {db_port, ?GET_ARG(port, Info)},
                        {db_user, "itest"},
                        {db_pass, "itest"},
                        {db_name, ?GET_ARG(db, Info)},
                        {idle_check, 10000},
                        {prepared_statements, {?MODULE, statements, [pgsql]}},
                        {column_transforms,
                         [{<<"created_at">>, fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1},
                        {<<"updated_at">>, fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1}]}
                       ]),
    %% In production we use 5, but I'm using 2 here for the time being
    %% to exercise the joining together of multiple database calls.  See the TODO
    %% in the "Environment-filtered Recipes Tests" section for more.
    ok = application:set_env(chef_db, bulk_fetch_batch_size, 2),

    PoolConfig = [{name, sqerl},
                  {max_count, 3},
                  {init_count, 1},
                  {start_mfa, {sqerl_client, start_link, []}}],
    ok = application:set_env(pooler, pools, [PoolConfig]),
    [ ensure_started(App) || App <- app_list() ],
    %% Temporarily disable logging for sqerl startup
    %% TODO: refactor so we only setup and destroy once
    %% error_logger:tty(false),
    ok = application:start(sqerl),
    ok.
    %% error_logger:tty(true).

%% @doc Shutdown all the infrastructure that was started up by
%% setup_env/0.  Use as a final cleanup function for test suites.
destroy_env() ->
    %% Suppress output of logging for shutdowns... it muddies up the output
    error_logger:tty(false),
    application:stop(sqerl),
    [ application:stop(App) || App <- lists:reverse(app_list()) ],
    error_logger:tty(true).

%% @doc Until Sqerl gets the ability to fire off generated SQL, use
%% transactions, or we get a more sane integration test setup that
%% allows for cleaning up the database between tests, I'm adding some
%% prepared statements to Sqerl that will do the kind of cleanup I
%% need.  Feel free to add more as the need arises.
cleanup_statements() ->
    [{delete_cookbook_versions,
      <<"DELETE FROM cookbook_versions">>},
     {delete_cookbook_version_checksums,
      <<"DELETE FROM cookbook_version_checksums">>},
     {delete_cookbooks,
      <<"DELETE FROM cookbooks">>}].

statements(pgsql) ->
    {ok, Statements} = file:consult("priv/pgsql_statements.config"),
    {ok, HelperStatements} = file:consult("itest/helper_pgsql_statements.config"),
    Statements ++ HelperStatements ++ cleanup_statements().

basic_test_() ->
    {foreach,
     %% Setup
     fun() ->
             setup_env()
     end,
     %% Cleanup
     fun(_) ->
             destroy_env()
     end,
     %% Tests
     [
      {<<"Node Operations">>,
       [
        {<<"Insert operations">>, fun chef_sql_nodes:insert_node_data/0}
       ]
      },
      {<<"User Operations">>,
       [
         %% Always run fetch user list first, so no users
         %% yet exist in DB, so results are predictable,
         %% since we don't clean up after every test
         {<<"Fetch user list">>, fun chef_sql_users:fetch_user_list/0},
         {<<"Insert user">>, fun chef_sql_users:insert_user_data/0},
         {<<"Fetch single user">>, fun chef_sql_users:fetch_user_data/0},
         {<<"Update user">>, fun chef_sql_users:update_user_data/0},
         {<<"Delete user">>, fun chef_sql_users:delete_user_data/0},
         {<<"Count admin users">>, fun chef_sql_users:count_admin_users/0}
       ]
      },
      {<<"Client Operations">>,
       [
        {<<"Insert operations">>, fun chef_sql_clients:insert_client_data/0},
        {<<"Fetch operations">>,  fun chef_sql_clients:fetch_client_data/0},
        {<<"Bulk Fetch operations">>,  fun chef_sql_clients:bulk_fetch_client_data/0},
        {<<"Delete operations">>, fun chef_sql_clients:delete_client_data/0}
       ]
      },
      {<<"Data Bag Operations">>,
       [ {<<"Insert operations">>, fun chef_sql_data_bag:insert_data_bag_data/0},
         {<<"Fetch multiple">>, fun chef_sql_data_bag:fetch_data_bags/0},
         {<<"Fetch single">>, fun chef_sql_data_bag:fetch_data_bag/0},
         {<<"Delete">>, fun chef_sql_data_bag:delete_data_bag/0} ] }
      ,
      {<<"Data Bag Item Operations">>,
       [ {<<"Insert operations">>, fun chef_sql_data_bag_item:insert_data_bag_item_data/0},
         {<<"Fetch multiple">>, fun chef_sql_data_bag_item:fetch_data_bag_items/0},
         {<<"Fetch single">>, fun chef_sql_data_bag_item:fetch_data_bag_item/0},
         {<<"Id get">>, fun chef_sql_data_bag_item:fetch_data_bag_item_ids/0},
         {<<"Bulk Get">>, fun chef_sql_data_bag_item:bulk_get_data_bag_items/0},
         {<<"Update">>, fun chef_sql_data_bag_item:update_data_bag_item/0},
         {<<"Delete">>, fun chef_sql_data_bag_item:delete_data_bag_item/0}
       ]},
      {<<"Sandbox Operations">>,
       [
        {<<"Insert operations">>, fun chef_sql_sandboxes:insert_sandbox/0},
        {<<"Mark most checksums as uploaded">>, fun chef_sql_sandboxes:mark_some_checksums_as_uploaded/0},
        {<<"Check which checksums are not uploaded">>, fun chef_sql_sandboxes:check_non_uploaded_checksums/0},
        {<<"Upload and verify last checksum">>, fun chef_sql_sandboxes:upload_last_checksum/0},
        {<<"Fetch sandbox">>, fun chef_sql_sandboxes:fetch_sandbox/0},
        {<<"Delete sandbox">>, fun chef_sql_sandboxes:delete_sandbox/0}

       ]},
      {<<"Cookbook Operations">>,
       [ {<<"Insert operations">>, fun chef_sql_cookbook_versions:insert_cookbook_data/0},
         {<<"Fetch AuthzId">>, fun chef_sql_cookbook_versions:fetch_cookbook_authz/0}
       ]},
      {<<"Cookbook Version Operations">>,
       [ {<<"Insert operations">>, fun chef_sql_cookbook_versions:insert_cookbook_version_data/0},
         {<<"Insert cookbook with null id">>, fun chef_sql_cookbook_versions:insert_cbv_null_id/0},
         {<<"Insert cookbook with no id">>, fun chef_sql_cookbook_versions:insert_cbv_no_id/0},
         {<<"Insert cookbook with unknown checksums">>, fun chef_sql_cookbook_versions:insert_cbv_with_unknown_checksums/0},
         {<<"Insert cookbook with frozen">>, fun chef_sql_cookbook_versions:insert_cbv_with_frozen/0},
         {<<"Fetch single, does not exist">>, fun chef_sql_cookbook_versions:fetch_cookbook_version_not_exist/0},
         {<<"Fetch single, no checksums">>, fun chef_sql_cookbook_versions:fetch_cookbook_version_no_checksums/0},
         {<<"Fetch single with checksums">>, fun chef_sql_cookbook_versions:fetch_cookbook_version_checksums/0},
         {<<"Fetch different version of cookbook returns authzid">>,
          fun chef_sql_cookbook_versions:fetch_cookbook_version_different_version/0},
         {<<"Fetch versions">>, fun chef_sql_cookbook_versions:fetch_cookbook_versions/0},
         {<<"Fetch version for single cookbook, no versions">>, fun chef_sql_cookbook_versions:fetch_cookbook_versions_single_cookbook_no_versions/0},
         {<<"Fetch version for single cookbook, with versions">>, fun chef_sql_cookbook_versions:fetch_cookbook_versions_single_cookbook_with_versions/0},

         {<<"Fetch latest version of cookbook">>, fun chef_sql_cookbook_versions:fetch_latest_cookbook_version/0},
         {<<"Fetch latest version of cookbook, different version on server">>, fun chef_sql_cookbook_versions:fetch_latest_cookbook_version_different_versions/0},
	 {<<"Update existing cookbook">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums/0},
	 {<<"Update existing cookbook with deletions">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums_with_deletions/0},
	 {<<"Update existing cookbook with additions">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums_with_additions/0},
	 {<<"Update existing cookbook with deletions and additions">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums_with_deletions_and_additions/0},
	 {<<"Update existing cookbook with checkums that have not been uploaded">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums_with_missing_checksums/0},
	 {<<"Update existing cookbook when deleting a shared checksum">>, fun chef_sql_cookbook_versions:update_cookbook_version_checksums_with_shared_checksums/0},
         {<<"Delete with checksums">>, fun chef_sql_cookbook_versions:delete_cookbook_version_checksums/0},
         {<<"Delete multiple versions">>, fun chef_sql_cookbook_versions:delete_cookbook_multiple_versions/0}
       ]},
      {<<"Full Cookbook flow integration Tests">>,
       [ {<<"Create for first time">>, fun chef_sql_cookbook_versions:cookbook_create_from_scratch/0},
         {<<"Create new version of existing cookbook">>, fun chef_sql_cookbook_versions:cookbook_create_new_version/0}
       ]},
      {"Latest Cookbook Tests", generator, fun chef_sql_latest_cookbooks:latest_cookbooks/0},
      {"Cookbook Recipes Tests", generator, fun chef_sql_cookbook_recipes:cookbook_recipes/0},

      {"Cookbook Version Dependency Retrieval Tests", generator, fun chef_sql_cookbook_deps:deps_retrieval/0},
      {"Environment-filtered Cookbooks and Recipe Tests", generator, fun chef_sql_environment_cookbooks:environment_cookbooks/0}
     ]}.
