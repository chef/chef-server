-module(chef_db_SUITE).

-include_lib("common_test/include/ct.hrl").

all() -> [node_ops, user_ops, client_ops, data_bag_ops, data_bag_item_ops,
          sandbox_ops, cookbook_ops,
          cookbook_version_ops,
          cookbook_upload_flow_ops,
          latest_cookbooks,
          cookbook_recipes,
          cookbook_version_deps,
          environment_filtered_cookbooks_and_recipes].

init_per_suite(Config) ->
    ct:pal("hi from init~n"),
    Dir = ?config(priv_dir, Config),
    Port = pg_test_util:random_bogus_port(),
    SchemaDir = code:priv_dir(chef_db),
    Schema = filename:join([SchemaDir, "pgsql_schema.sql"]),
    DbConfig = pg_test_util:init_pg_db(Dir, "chef_db_test_db", Schema, Port),
    Config1 = DbConfig ++ Config,
    setup_chef_db(Config1),
    Config1.

end_per_suite(Config) ->
    ct:pal("bye from init~n"),
    PgDir = ?config(pg_data, Config),
    cleanup_chef_db(),
    pg_test_util:stop_pg_db(PgDir),
    ok.

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
    chef_sql_cookbook_versions:insert_cbv_no_id(),
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

setup_chef_db(Config) ->
    itest_util:set_env(sqerl,
                       [{db_host, "localhost"},
                        {db_port, ?config(pg_port, Config)},
                        {db_user, os:getenv("USER")},
                        %% ignored since we are connecting locally as owning user
                        {db_pass, "sesame-ignored"},
                        {db_name, ?config(pg_name, Config)},
                        {idle_check, 10000},
                        {prepared_statements, {?MODULE, statements, [pgsql]}},
                        {column_transforms,
                         [{<<"created_at">>, fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1},
                        {<<"updated_at">>, fun sqerl_transformers:convert_YMDHMS_tuple_to_datetime/1}]}
                       ]),

    itest_util:set_env(stats_hero,
                       [{udp_socket_pool_size, 1},
                        {estatsd_host, "localhost"},
                        {estatsd_port, 3001}]),

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
    %% error_logger:tty(false),
    ok = application:start(sqerl),
    ok.
    %% error_logger:tty(true).

%% @doc Shutdown all the infrastructure that was started up by
%% setup_env/0.  Use as a final cleanup function for test suites.
cleanup_chef_db() ->
    %% Suppress output of logging for shutdowns... it muddies up the output
    error_logger:tty(false),
    application:stop(sqerl),
    [ application:stop(App) || App <- lists:reverse(app_list()) ],
    error_logger:tty(true).

app_list() ->
    [crypto, public_key, ssl, epgsql, pooler, stats_hero].

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            Error
    end.

statements(pgsql) ->
    ChefDbFile = filename:join([code:priv_dir(chef_db), "pgsql_statements.config"]),
    {ok, ChefDbStatements} = file:consult(ChefDbFile),
    [{delete_cookbook_versions,
      <<"DELETE FROM cookbook_versions">>},
     {delete_cookbook_version_checksums,
      <<"DELETE FROM cookbook_version_checksums">>},
     {delete_cookbooks,
      <<"DELETE FROM cookbooks">>},
     {find_checksum_by_id,
      <<"SELECT checksum FROM checksums WHERE org_id = $1 AND checksum = $2">>}]
        ++ ChefDbStatements.

