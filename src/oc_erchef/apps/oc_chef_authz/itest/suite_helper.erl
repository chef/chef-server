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

-module(suite_helper).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

start_server(Config) ->
    chef_test_suite_helper:set_app_env(stats_hero),
    chef_test_suite_helper:set_app_env(pooler),

    chef_test_suite_helper:set_env(oc_chef_authz,
            [{authz_service, [{root_url, "http://test-authz-service:2323"},
                              {timeout, 2000},
                              {init_count, 5},
                              {max_count, 50},
                              {ibrowse_options, [{connect_timeout, 5000}]}]},
             {couchdb_host, "localhost"},
             {couchdb_port, 6984},
             {cleanup_interval, 60000},
             {cleanup_batch_size, 100}]),

    [ {ok, _} = application:ensure_all_started(A) || A <- needed_apps() ],
    Config.

needed_apps() ->
    [ibrowse,
     pooler,
     stats_hero,
     asn1,
     crypto,
     public_key,
     ssl,
     epgsql,
     sqerl,
     oc_chef_authz].

statements(pgsql) ->
    ChefDbStatements = chef_sql:statements(),
    AuthzDbStatements = oc_chef_authz_db:statements(pgsql),
    ChefDbStatements ++ AuthzDbStatements.

cleanup_test_case(Config) ->
    Tables = ?config(tables, Config),
    [chef_test_db_helper:delete_all_from_table(TableName) || TableName <- Tables],
    Config.
