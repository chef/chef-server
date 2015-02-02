%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2013-2015 Chef, Inc. All Rights Reserved.
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

-module(setup_helper).

-include_lib("common_test/include/ct.hrl").

-export([
         start_server/1,
         needed_apps/0
        ]).

start_server(Config) ->
    chef_test_suite_helper:set_app_env(stats_hero),
    chef_test_suite_helper:set_app_env(pooler),

    application:set_env(oc_chef_authz, couchdb_host, "localhost"),
    application:set_env(oc_chef_authz, couchdb_port, 6984),

    application:set_env(chef_db, couchdb_host, "localhost"),
    application:set_env(chef_db, couchdb_port, 6984),

    application:set_env(lager, error_logger_redirect, false),

    % TODO: we should automate setting these, if it matters at all
    application:set_env(oc_chef_wm, default_orgname, <<"org">>),
    application:set_env(oc_chef_wm, api_version, "12.0.0"),
    application:set_env(oc_chef_wm, server_flavor, "cs"),
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

    [ ok = chef_test_suite_helper:ensure_started(A) || A <- needed_apps() ],
    Config.

needed_apps() ->
    [sasl,
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
     oc_chef_wm].

