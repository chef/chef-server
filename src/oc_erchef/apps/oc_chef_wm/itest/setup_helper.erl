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
-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").

-export([
         start_server/1,
         needed_apps/0,
         base_init_per_suite/1,
         base_end_per_suite/1,
         get_config/2,
         make_user/3
        ]).

-define(TEST_DB_NAME, "oc_chef_wm_itests").

start_server(Config) ->
    chef_test_suite_helper:set_app_env(stats_hero),
    chef_test_suite_helper:set_app_env(pooler),

    application:set_env(oc_chef_authz, couchdb_host, "localhost"),
    application:set_env(oc_chef_authz, couchdb_port, 6984),

    application:set_env(chef_db, couchdb_host, "localhost"),
    application:set_env(chef_db, couchdb_port, 6984),

    application:set_env(lager, error_logger_redirect, false),

    % Use canned openssl response instead: always returns the same key, but does
    % so very fast.
    application:set_env(chef_authn, openssl_path,
                        filename:join(code:priv_dir(oc_chef_wm),
                                      "../test/mock_openssl.sh")),

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
    application:set_env(oc_chef_wm, node_license, 25),
    application:set_env(oc_chef_wm, upgrade_url, <<"https://nowhere">>),

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
    application:set_env(chef_index, search_queue_mode, inline),
    application:set_env(chef_index, solr_service,
                        [{root_url, "http://localhost:9999"},
                         {max_count, 10},
                         {init_count, 10},
                         {cull_interval, {1, min}},
                         {max_age, {70, sec}},
                         {max_connection_duration, {70, sec}},
                         {ibrowse_options, [{connect_timeout, 10000}]},
                         {timeout, 300}
                        ]),
    [ {ok, _} = application:ensure_all_started(A) || A <- needed_apps() ],
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
     chef_index,
     oc_chef_authz,
     oc_chef_wm].

%% @doc Does the setup common to all CT suites in this app:
%% starts the server, create an org, and a client for it
%% Expects the Config to define the following keys:
%% org_name, org_authz_id, authz_id, client_name
base_init_per_suite(Config0) ->
    OrgName = get_config(org_name, Config0),
    OrgAuthzId = get_config(org_authz_id, Config0),
    AuthzId = get_config(authz_id, Config0),
    ClientName = get_config(client_name, Config0),

    Config1 = chef_test_db_helper:start_db([{app, oc_chef_wm}|Config0], ?TEST_DB_NAME),
    Config2 = start_server(Config1),

    FakeContext = chef_db:make_context(?API_MIN_VER, <<"fake-req-id">>),
    OrganizationRecord = chef_object:new_record(oc_chef_organization,
                                                ?API_MIN_VER,
                                                nil,
                                                OrgAuthzId,
                                                {[{<<"name">>, OrgName},
                                                  {<<"full_name">>, OrgName}]}),
    ok = chef_db:create(OrganizationRecord,
                        FakeContext,
                        AuthzId),

    %% create the test client
    OrgId = oc_chef_organization:id(OrganizationRecord),
    ClientRecord = chef_object:new_record(chef_client,
                                          ?API_MIN_VER,
                                          OrgId,
                                          AuthzId,
                                          {[{<<"name">>, ClientName},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),
    io:format("ClientRecord ~p~n", [ClientRecord]),
    ok = chef_db:create(ClientRecord,
                        FakeContext,
                        AuthzId),
    ct:pal("pwd: ~p", [os:cmd("pwd")]),
    {ok, PubKey} = file:read_file(filename:join([?config(data_dir, Config2), "..","spki_public.pem"])),
    [{context, FakeContext},
     {org, OrganizationRecord},
     {org_id, OrgId},
     {client, ClientRecord},
     {pubkey, PubKey}
     | Config2].

%% @doc Returns a value from the config, making sure it's defined
-spec get_config(atom(), CtConfig :: [{atom(), any()}]) -> any().
get_config(Key, Config) ->
    Value = ?config(Key, Config),
    case Value =:= undefined of
        true ->
            ct:pal("You need to define the ~p in your CT config", [Key]),
            ?assertNotEqual(undefined, Value);
        false ->
            Value
    end.

base_end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, needed_apps()).

make_user(Config, Name, AuthzId) ->
    User = chef_object:new_record(chef_user, ?API_MIN_VER, ?OSC_ORG_ID, AuthzId,
                                   {[{<<"username">>, Name},
                                     {<<"password">>, <<"zuperzecret">>},
                                     {<<"email">>, iolist_to_binary([Name, <<"@somewhere.com">>])},
                                     {<<"public_key">>, ?config(pubkey, Config)},
                                     {<<"display_name">>, Name}]}),
    Response = chef_db:create(User, ?config(context, Config), AuthzId),
    ?assertEqual(Response, ok),
    User.
