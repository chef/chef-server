%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2011-2015 Chef, Inc. All Rights Reserved.
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

-module(chef_db_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

all() -> [node_ops, user_ops, client_ops, data_bag_ops, data_bag_item_ops,
          sandbox_ops, cookbook_ops,
          cookbook_version_ops,
          cookbook_upload_flow_ops,
          latest_cookbooks,
          cookbook_recipes,
          cookbook_version_deps,
          environment_filtered_cookbooks_and_recipes].

needed_apps() ->
    [crypto, public_key, ssl, epgsql, pooler, stats_hero, asn1, sqerl].

%% append to the chef_sql statements with some test statements we'll need
statements() ->
    ChefDbStatements = oc_chef_sql:statements(pgsql),
    [{delete_cookbook_versions,
      <<"DELETE FROM cookbook_versions">>},
     {delete_cookbook_version_checksums,
      <<"DELETE FROM cookbook_version_checksums">>},
     {delete_cookbooks,
      <<"DELETE FROM cookbooks">>},
     {find_checksum_by_id,
      <<"SELECT checksum FROM checksums WHERE org_id = $1 AND checksum = $2">>}]
        ++ ChefDbStatements.

init_per_suite(LastConfig) ->
    %% Define statements for sqerl
    NewConfig = [{app, chef_db}, {statements, {chef_db_SUITE, statements, []}} | LastConfig],
    Config = chef_test_db_helper:start_db(NewConfig, "oc_chef_authz_itests"),
    start_server(Config).

start_server(Config) ->
    chef_test_suite_helper:set_app_env(stats_hero),
    chef_test_suite_helper:set_app_env(pooler),

    %% In production we use 5, but I'm using 2 here for the time being
    %% to exercise the joining together of multiple database calls.  See the TODO
    %% in the "Environment-filtered Recipes Tests" section for more.
    application:set_env(chef_db, bulk_fetch_batch_size, 2),
    application:set_env(chef_db, couchdb_host, "localhost"),
    application:set_env(chef_db, couchdb_port, chef_test_suite_helper:random_bogus_port()),
    [ {ok, _} = application:ensure_all_started(App) || App <- needed_apps() ],
    Config.

end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, needed_apps()).

node_ops(_Config) ->
    chef_sql_nodes:insert_node_data().

user_ops(_Config) ->
    %% Always run fetch user list first, so no users
    %% yet exist in DB, so results are predictable,
    %% since we don't clean up after every test
    chef_sql_users:fetch_user_list(),
    chef_sql_users:insert_user_data(),
    chef_sql_users:fetch_user_data(),
    chef_sql_users:update_user_data(),
    chef_sql_users:delete_user_data(),
    chef_sql_users:count_admin_users().

client_ops(_Config) ->
    chef_sql_clients:insert_client_data(),
    chef_sql_clients:fetch_client_data(),
    chef_sql_clients:bulk_fetch_client_data(),
    chef_sql_clients:delete_client_data().

data_bag_ops(_Config) ->
    chef_sql_data_bag:insert_data_bag_data(),
    chef_sql_data_bag:fetch_data_bags(),
    chef_sql_data_bag:fetch_data_bag(),
    chef_sql_data_bag:delete_data_bag().

data_bag_item_ops(_Config) ->
    chef_sql_data_bag_item:insert_data_bag_item_data(),
    chef_sql_data_bag_item:fetch_data_bag_items(),
    chef_sql_data_bag_item:fetch_data_bag_item(),
    chef_sql_data_bag_item:fetch_data_bag_item_ids(),
    chef_sql_data_bag_item:bulk_get_data_bag_items(),
    chef_sql_data_bag_item:update_data_bag_item(),
    chef_sql_data_bag_item:delete_data_bag_item().

sandbox_ops(_Config) ->
    chef_sql_sandboxes:insert_sandbox(),
    chef_sql_sandboxes:mark_some_checksums_as_uploaded(),
    chef_sql_sandboxes:check_non_uploaded_checksums(),
    chef_sql_sandboxes:upload_last_checksum(),
    chef_sql_sandboxes:fetch_sandbox(),
    chef_sql_sandboxes:delete_sandbox().

cookbook_ops(_Config) ->
    chef_sql_cookbook_versions:insert_cookbook_data(),
    chef_sql_cookbook_versions:fetch_cookbook_authz().

cookbook_version_ops(_Config) ->
    chef_sql_cookbook_versions:insert_cookbook_version_data(),
    chef_sql_cookbook_versions:insert_cbv_null_id(),
    chef_sql_cookbook_versions:insert_cbv_with_unknown_checksums(),
    chef_sql_cookbook_versions:insert_cbv_with_frozen(),
    chef_sql_cookbook_versions:fetch_cookbook_version_not_exist(),
    chef_sql_cookbook_versions:fetch_cookbook_version_no_checksums(),
    chef_sql_cookbook_versions:fetch_cookbook_version_checksums(),
    chef_sql_cookbook_versions:fetch_cookbook_version_different_version(),
    chef_sql_cookbook_versions:fetch_cookbook_versions(),
    chef_sql_cookbook_versions:fetch_cookbook_versions_single_cookbook_no_versions(),
    chef_sql_cookbook_versions:fetch_cookbook_versions_single_cookbook_with_versions(),
    chef_sql_cookbook_versions:fetch_latest_cookbook_version(),
    chef_sql_cookbook_versions:fetch_latest_cookbook_version_different_versions(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums_with_deletions(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums_with_additions(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums_with_deletions_and_additions(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums_with_missing_checksums(),
    chef_sql_cookbook_versions:update_cookbook_version_checksums_with_shared_checksums(),
    chef_sql_cookbook_versions:delete_cookbook_version_checksums(),
    chef_sql_cookbook_versions:delete_cookbook_multiple_versions().

cookbook_upload_flow_ops(_Config) ->
    chef_sql_cookbook_versions:cookbook_create_from_scratch(),
    chef_sql_cookbook_versions:cookbook_create_new_version().

latest_cookbooks(_Config) ->
    chef_sql_latest_cookbooks:test_all().

cookbook_recipes(_Config) ->
    chef_sql_cookbook_recipes:test_all().

cookbook_version_deps(_Config) ->
    chef_sql_cookbook_deps:test_all().

environment_filtered_cookbooks_and_recipes(_Config) ->
    chef_sql_environment_cookbooks:test_all().


