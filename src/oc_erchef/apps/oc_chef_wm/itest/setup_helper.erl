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
         make_user/3,
         mock_authz/1,
         unmock_authz/0
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

    %% Set bcrypt rounds to the minimum, speeding up password hashing during
    %% user record creation
    application:set_env(bcrypt, default_log_rounds, 4, [{persistent, true}]),

    %% Use canned openssl response instead: always returns the same key, but does
    %% so very fast.
    application:set_env(chef_authn, openssl_path,
                        filename:join(code:priv_dir(oc_chef_wm),
                                      "../test/mock_openssl.sh")),
    %% Set chef_secrets

   %% TODO(ssd) 2017-03-15: Tests in one of the other applications aren't cleaning up after
   %% themselves.  When all the ct tests are run together, this means that chef_secret might
   %% be running already with different test configuration.
    application:stop(chef_secrets),
    application:set_env(chef_secrets, provider, chef_secrets_mock_provider),
    application:set_env(chef_secrets, provider_config, []),
    {ok, FileContent} = file:read_file(filename:join(code:priv_dir(oc_chef_wm), "../test/secrets.json")),
    FakeSecretsData = jiffy:decode(FileContent),
    meck:new(chef_secrets_mock_provider, [non_strict]),
    meck:expect(chef_secrets_mock_provider, read, fun(_Config) -> {ok, FakeSecretsData} end),

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
                         {retry_on_conn_closed, true},
                         {ibrowse_options, [{connect_timeout, 10000}]},
                         {timeout, 300}
                        ]),
    meck:new(darklaunch_app),
    meck:expect(darklaunch_app, start, fun(_, _) -> {ok, self()} end),
    [ {ok, _} = application:ensure_all_started(A) || A <- needed_apps() ],
    Config.

needed_apps() ->
    [sasl,
     asn1,
     crypto,
     stats_hero,
     chef_secrets,
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
     eldap,
     bcrypt,
     folsom,
     chef_objects,
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
    PubKeyFile = filename:absname_join(filename:dirname(?FILE), "spki_public.pem"),
    ct:pal("Opening pubkey file: ~p", [PubKeyFile]),
    {ok, PubKey} = file:read_file(PubKeyFile),
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
    V = chef_test_suite_helper:stop_server(Config, needed_apps()),
    ct:pal("App: ~p~n", [application:which_applications()]),
    V.

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

mock_authz(ClientAuthzId) ->
    meck:new(oc_chef_authz, [passthrough]),
    meck:expect(oc_chef_authz, create_entity_if_authorized,
                fun(_, _, _, _) ->
                        ct:pal("create_entity_if_authorized()~n", []),
                        <<RandomInt:128>> = crypto:strong_rand_bytes(16),
                        RandomId = iolist_to_binary(io_lib:format("~32.16.0b", [RandomInt])),
                        {ok, RandomId}
                end),
    meck:expect(oc_chef_authz, delete_resource,
                fun(_, _, _) ->
                        ct:pal("delete_resource()~n", []),
                        ok
                end),
    meck:expect(oc_chef_authz, get_container_aid_for_object,
                fun(_, _, _) ->
                        ct:pal("get_container_aid_for_object()~n", []),
                        <<"00000000000000000000000000000000">>
                end),
    meck:expect(oc_chef_authz, is_authorized_on_resource,
                fun(_, _, _, _, _, _) ->
                        ct:pal("is_authorized_on_resource()~n", []),
                        true
                end),
    meck:expect(oc_chef_authz, is_actor_transitive_member_of_group,
                fun(_, _, _) ->
                        ct:pal(" is_actor_transitive_member_of_group()~n", []),
                        true
                end),

    meck:new(chef_authn, [passthrough]),
    meck:expect(chef_authn, authenticate_user_request,
                fun(_, _, _, _, _, _) ->
                        ct:pal("authenticate_user_request()~n", []),
                        {name, <<"mock_user">>, #chef_requestor{name = <<"default">>, authz_id = ClientAuthzId, type = <<"client">> }}
                end),
    meck:expect(chef_authn, validate_headers,
                fun(_, _) ->
                        ct:pal("validate_headers()~n", []),
                        [{'algorithm', <<"SHA1">>}, {'version', <<"1.0">>}]
                end),
    ok.

unmock_authz() ->
    meck:unload(chef_authn),
    meck:unload(oc_chef_authz).
