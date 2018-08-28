%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(setup_helper).

-include_lib("common_test/include/ct.hrl").

-export([
         start_server/1,
         stop_server/1
        ]).

-define(REQUIRED_APPS, [sasl,
                        asn1,
                        crypto,
                        stats_hero,
                        pooler,
                        public_key,
                        ssl,
                        epgsql,
                        sqerl,
                        ibrowse,
                        ejson,
                        inets,
                        mochiweb,
                        webmachine,
                        darklaunch,
                        gen_server2,
                        eldap,
                        bcrypt,
                        folsom,
                        chef_objects,
                        rabbit_common,
                        amqp_client,
                        gen_bunny,
                        compiler,
                        syntax_tools,
                        goldrush,
                        lager,
                        couchbeam,
                        chef_index,
                        oc_chef_authz,
                        oc_chef_wm]).

start_server(Config) ->
    DbConfig = db_helper:start_db(Config),
    DbName = ?config(db_name, DbConfig),
    DbPort = ?config(db_port, DbConfig),
    DbDataDir = ?config(db_port, DbConfig),
    DbUser = ?config(db_user, DbConfig),
    DbPass = ?config(db_pass, DbConfig),

    application:set_env(oc_chef_wm, default_orgname, <<"org">>),
    application:set_env(oc_chef_authz, couchdb_host, "localhost"),
    application:set_env(oc_chef_authz, couchdb_port, 6984),
    application:set_env(chef_db, couchdb_host, "localhost"),
    application:set_env(chef_db, couchdb_port, 6984),
    
    application:set_env(lager, error_logger_redirect, false),

    application:set_env(stats_hero, udp_socket_pool_size, 200),
    application:set_env(stats_hero, estatsd_host, "127.0.0.1"),
    application:set_env(stats_hero, estatsd_port, 9466),

    application:set_env(oc_chef_wm, api_version, "11.0.0"),
    application:set_env(oc_chef_wm, server_flavor, "ec"),
    application:set_env(oc_chef_wm, ip, "127.0.0.1"),
    application:set_env(oc_chef_wm, port, 8000),
    application:set_env(oc_chef_wm, reqid_header_name, "X-Request-Id"),
    application:set_env(oc_chef_wm, auth_skew, 900),
    application:set_env(oc_chef_wm, bulk_fetch_batch_size, 5),
    application:set_env(oc_chef_wm, superusers, [<<"pivotal">>]),
    application:set_env(oc_chef_wm, root_metric_key, "chefAPI"),
    application:set_env(oc_chef_wm, authz_timeout, 1000),
    application:set_env(oc_chef_wm, authz_fanout, 20),

    application:set_env(chef_wm, local_key_gen, {true, 1024}),

    application:set_env(sqerl, db_type, pgsql),
    application:set_env(sqerl, db_host, "127.0.0.1"),
    application:set_env(sqerl, db_port, DbPort),
    application:set_env(sqerl, db_user, DbUser),
    application:set_env(sqerl, db_pass, DbPass),
    application:set_env(sqerl, db_name, DbName),
    application:set_env(sqerl, idle_check, 10000),
    application:set_env(sqerl, prepared_statements, {oc_chef_sql, statements, [pgsql]}),
    application:set_env(sqerl, column_transforms,
                        [{<<"created_at">>,
                          {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}},
                         {<<"updated_at">>,
                          {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}}]),

    application:set_env(pooler, pools,
                        [[{name, sqerl},
                          {max_count, 1},
                          {init_count, 1},
                          {start_mfa, {sqerl_client, start_link, []}}],
                         [{name, chef_depsolver},
                          {max_count, 5},
                          {init_count, 5},
                          {start_mfa, {chef_depsolver_worker, start_link, []}}]]),

    PrivDir = ?config(priv_dir, Config),
    application:set_env(webmachine, log_handlers,
                        [{oc_wm_request_logger,
                          [
                           {file, filename:join([PrivDir, "requests.log"])},
                           {file_size, 100},  %% Size in MB
                           {files, 1},
                           {annotations, [req_id, org_name, msg, darklaunch, perf_stats]}
                          ]
                         }]),
    application:set_env(chef_index, disable_rabbitmq, true),

    [begin
         ct:pal("Starting ~p~n", [App]),
         ok = application:start(App)
     end || App <- ?REQUIRED_APPS ],

    %% return the ct config merged with the database config
    lists:append(Config, DbConfig).

stop_server(Config) ->
    [begin
         ct:pal("Stopping ~p~n", [App]),
         ok = application:stop(App)
     end || App <- lists:reverse(?REQUIRED_APPS)],
    Config2 = db_helper:stop_db(Config),
    Config2.
