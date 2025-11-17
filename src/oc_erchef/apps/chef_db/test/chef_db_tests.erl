%% B
%% Copyright Chef Software, Inc. All Rights Reserved.
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

-module(chef_db_tests).

-include_lib("eunit/include/eunit.hrl").

-include("chef_types.hrl").

-define(REQ_ID, <<"req-id-123">>).

fetch_requestor_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_sql),
             meck:new(chef_db_darklaunch),
             set_app_env()
     end,
     fun(_) ->
             meck:unload([chef_sql, chef_db_darklaunch])
     end,
     [
      {"a user is found SQL",
       fun() ->
               meck:expect(chef_db_darklaunch, is_enabled,
                           fun(<<"couchdb_clients">>, _) -> false;
                              (<<"couchdb_organizations">>, _) -> true
                           end),


               User = #chef_user{server_api_version = ?API_MIN_VER,
                                 id = <<"a1">>,
                                 authz_id = <<"b2">>,
                                 username = <<"alice">>,
                                 public_key = <<"key data">>,
                                 hashed_password = <<"abc123xyz">>,
                                 salt = <<"pepper">>,
                                 hash_type = <<"bcrypt">>,
                                 external_authentication_uid = <<"">>,
                                 recovery_authentication_enabled = <<"0">>
                               },
               meck:expect(chef_sql, fetch,
                           fun(#chef_client{org_id = <<"mock-org-id-123">>, name =  <<"alice">>}) -> not_found; % client_fetch
                              (#chef_user{username = <<"alice">>}) -> User                                % user fetch
                           end),
               Context = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
               Got = chef_db:fetch_requestor(Context, <<"mock-org-id-123">>, <<"alice">>),
               ?assertEqual(Got, User),
               Stats = stats_hero:snapshot(<<"req-id-123">>, all),
               ExpectKeys = [<<"req_time">>,
                             <<"rdbms.chef_sql.fetch_time">>,
                             <<"rdbms.chef_sql.fetch_count">>,
                             <<"rdbms_time">>,
                             <<"rdbms_count">>],
               GotKeys = [ Key || {Key, _} <- Stats ],
               ?assertEqual(lists:sort(ExpectKeys), lists:sort(GotKeys))
       end
      },
      {"a client is found SQL cert",
       fun() ->
               Client = #chef_client{server_api_version = ?API_MIN_VER,
                                     id = <<"mock-client-id">>,
                                     authz_id = <<"mock-client-authz-id">>,
                                     org_id = <<"org-id-123">>,
                                     name = <<"alice">>,
                                     pubkey_version = 1,
                                     public_key = <<"key data">>},
               meck:expect(chef_sql, fetch,
                           fun(#chef_user{username = <<"alice">>}) -> not_found;
                              (#chef_client{org_id = <<"mock-org-id-123">>, name = <<"alice">>}) -> Client
                           end),
               Context = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
               Got = chef_db:fetch_requestor(Context, <<"mock-org-id-123">>, <<"alice">>),
               ?assertEqual(Got, Client)
       end
      },
      {"a client is found SQL key",
       fun() ->
               meck:expect(chef_db_darklaunch, is_enabled,
                           fun(<<"sql_users">>, _) -> true end),
               Client = #chef_client{server_api_version = ?API_MIN_VER,
                                     id = <<"mock-client-id">>,
                                     authz_id = <<"mock-client-authz-id">>,
                                     org_id = <<"org-id-123">>,
                                     name = <<"alice">>,
                                     pubkey_version = 0,
                                     public_key = <<"key data">>},
               meck:expect(chef_sql, fetch,
                           fun(#chef_user{username = <<"alice">>}) -> not_found;
                              (#chef_client{org_id = <<"mock-org-id-123">>, name = <<"alice">>}) -> Client
                           end),
               Context = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
               Got = chef_db:fetch_requestor(Context, <<"mock-org-id-123">>, <<"alice">>),
               ?assertEqual(Got, Client),
               Stats = stats_hero:snapshot(<<"req-id-123">>, all),
               ExpectKeys = [<<"req_time">>,
                             <<"rdbms.chef_sql.fetch_time">>,
                             <<"rdbms.chef_sql.fetch_count">>,
                             <<"rdbms_time">>,
                             <<"rdbms_count">>],
               GotKeys = [ Key || {Key, _} <- Stats ],
               ?assertEqual(lists:sort(ExpectKeys), lists:sort(GotKeys))
       end
      }
    ]}.

fetch_cookbook_versions_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_sql),
             meck:new(chef_db_darklaunch),
             meck:expect(chef_db_darklaunch, is_enabled,
                         fun(<<"couchdb_organizations">>, _) -> false end),
             set_app_env()
     end,
     fun(_) ->
             ?assert(meck:validate(chef_sql)),
             meck:unload()
     end,
     [
       {"fetch_cookbook_versions returns list containing empty list on no results",
         fun() ->
             SqlOutput = [[ ]],
             meck:expect(chef_sql, fetch_cookbook_versions,
                         fun(_) -> {ok, SqlOutput} end),
             Ctx = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
             ?assertEqual(SqlOutput, chef_db:fetch_cookbook_versions(Ctx, <<"mock-org">>))
         end},
      {"fetch_cookbook_versions collects stats_hero metrics",
         fun() ->
             SqlOutput = [[ ]],
             meck:expect(chef_sql, fetch_cookbook_versions,
                         fun(_) -> {ok, SqlOutput} end),
             Ctx = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
             SqlOutput = chef_db:fetch_cookbook_versions(Ctx, <<"mock-org-id">>),
             Stats = stats_hero:snapshot(<<"req-id-123">>, all),
             ExpectKeys = [<<"req_time">>,
                           <<"rdbms.chef_sql.fetch_cookbook_versions_time">>,
                           <<"rdbms.chef_sql.fetch_cookbook_versions_count">>,
                           <<"rdbms_time">>,
                           <<"rdbms_count">>],
             GotKeys = [ Key || {Key, _} <- Stats ],
             ?assertEqual(lists:sort(ExpectKeys), lists:sort(GotKeys))
         end},
       {"fetch_cookbook_versions passes structured list",
         fun() ->
             SqlOutput = [[ <<"foo">>, {1, 2, 3} ]],
             meck:expect(chef_sql, fetch_cookbook_versions,
                         fun(_) -> {ok, SqlOutput} end),
             Ctx = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
             ?assertEqual(SqlOutput, chef_db:fetch_cookbook_versions(Ctx, <<"mock-org">>))
         end},
       {"fetch_cookbook_versions handles errors",
         fun() ->
             meck:expect(chef_sql, fetch_cookbook_versions,
                         fun(_) -> {error, internal_error} end),
             Ctx = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
             ?assertEqual({error, internal_error}, chef_db:fetch_cookbook_versions(Ctx, <<"mock-org">>))
         end},
       {"fetch_cookbook_versions handles errors",
         fun() ->
             meck:expect(chef_sql, fetch_cookbook_versions,
                         fun(_) -> {error, internal_error} end),
             Ctx = chef_db:make_context(?API_MIN_VER, <<"req-id-123">>),
             ?assertEqual({error, internal_error}, chef_db:fetch_cookbook_versions(Ctx, <<"mock-org">>))
         end}
     ]
    }.

set_app_env() ->
    chef_db_test_utils:start_stats_hero(),
    application:set_env(chef_db, couchdb_host, "localhost"),
    application:set_env(chef_db, couchdb_port, 5984),
    spawn_stats_hero_worker().

spawn_stats_hero_worker() ->
    Config = stats_hero_config(),
    {ok, _} = stats_hero_worker_sup:new_worker(Config),
    ok.

stats_hero_config() ->
    [{my_app, "chef_db_test"},
     {request_label, "test-req"},
     {request_action, "ACTION"},
     {org_name, "myorg"},
     {request_id, ?REQ_ID},
     {label_fun, {chef_db_test_utils, stats_hero_label}},
     {upstream_prefixes, [<<"rdbms">>, <<"couchdb">>, <<"solr">>]}].

%% CHEF-27821: Tests for fetch_requestors/4 with OR query support
fetch_requestors_with_or_query_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_sql),
             meck:new(stats_hero),
             meck:expect(stats_hero, ctime, fun(_, _) -> ok end),
             meck:expect(stats_hero, etime, fun(_, _) -> ok end),
             set_app_env()
     end,
     fun(_) ->
             meck:unload([chef_sql, stats_hero])
     end,
     [
      {"fetch_requestors with same name (backward compatibility)",
       fun() ->
               %% Test: When Name and OriginalName are the same
               %% Should find the user/client with that name
               User = #chef_user{
                   server_api_version = ?API_MIN_VER,
                   id = <<"user-id-123">>,
                   authz_id = <<"authz-id-456">>,
                   username = <<"testuser">>,
                   public_key = <<"key data">>,
                   hashed_password = <<"abc123xyz">>,
                   salt = <<"pepper">>,
                   hash_type = <<"bcrypt">>,
                   external_authentication_uid = <<"">>,
                   recovery_authentication_enabled = <<"0">>
               },
               
               meck:expect(chef_sql, fetch_actors_by_name, 
                           fun(_, <<"mock-org-id">>, <<"testuser">>, <<"testuser">>) -> 
                               [User] 
                           end),
               
               Context = chef_db:make_context(?API_MIN_VER, ?REQ_ID),
               Result = chef_db:fetch_requestors(Context, <<"mock-org-id">>, <<"testuser">>, <<"testuser">>),
               
               ?assertEqual(User, Result)
       end},
       
      {"fetch_requestors finds user by original name in OR query",
       fun() ->
               %% Test: Gateway scenario - should find user by OriginalName
               User = #chef_user{
                   server_api_version = ?API_MIN_VER,
                   id = <<"user-id-123">>,
                   authz_id = <<"authz-id-456">>,
                   username = <<"testuser">>,  % Original username
                   public_key = <<"key data">>,
                   hashed_password = <<"abc123xyz">>,
                   salt = <<"pepper">>,
                   hash_type = <<"bcrypt">>,
                   external_authentication_uid = <<"">>,
                   recovery_authentication_enabled = <<"0">>
               },
               
               %% SQL: WHERE (name = 'tenant1_testuser' OR name = 'testuser')
               %% Matches on OriginalName = 'testuser'
               meck:expect(chef_sql, fetch_actors_by_name,
                           fun(_, <<"mock-org-id">>, <<"tenant1_testuser">>, <<"testuser">>) ->
                               [User]
                           end),
               
               Context = chef_db:make_context(?API_MIN_VER, ?REQ_ID),
               Result = chef_db:fetch_requestors(Context, <<"mock-org-id">>, 
                                                 <<"tenant1_testuser">>, <<"testuser">>),
               
               ?assertEqual(User, Result)
       end},
       
      {"fetch_requestors finds client by gateway-modified name",
       fun() ->
               %% Test: User found by gateway-modified name
               Client = #chef_client{
                   server_api_version = ?API_MIN_VER,
                   id = <<"client-id-123">>,
                   authz_id = <<"authz-id-789">>,
                   org_id = <<"mock-org-id">>,
                   name = <<"tenant1_apiclient">>,
                   pubkey_version = 1,
                   public_key = <<"key data">>
               },
               
               meck:expect(chef_sql, fetch_actors_by_name,
                           fun(_, <<"mock-org-id">>, <<"tenant1_apiclient">>, <<"apiclient">>) ->
                               [Client]
                           end),
               
               Context = chef_db:make_context(?API_MIN_VER, ?REQ_ID),
               Result = chef_db:fetch_requestors(Context, <<"mock-org-id">>,
                                                 <<"tenant1_apiclient">>, <<"apiclient">>),
               
               ?assertEqual(Client, Result)
       end},
       
      {"fetch_requestors returns not_found when no match",
       fun() ->
               %% Test: Neither name matches
               meck:expect(chef_sql, fetch_actors_by_name,
                           fun(_, <<"mock-org-id">>, <<"nonexistent">>, <<"alsonothere">>) ->
                               []
                           end),
               
               Context = chef_db:make_context(?API_MIN_VER, ?REQ_ID),
               Result = chef_db:fetch_requestors(Context, <<"mock-org-id">>,
                                                 <<"nonexistent">>, <<"alsonothere">>),
               
               ?assertEqual(not_found, Result)
       end},
       
      {"fetch_requestor/3 calls fetch_requestors/4 with same name (backward compat)",
       fun() ->
               %% Test: 3-parameter wrapper uses same name for both parameters
               User = #chef_user{
                   server_api_version = ?API_MIN_VER,
                   id = <<"user-id">>,
                   authz_id = <<"authz-id">>,
                   username = <<"alice">>,
                   public_key = <<"key">>,
                   hashed_password = <<"hash">>,
                   salt = <<"salt">>,
                   hash_type = <<"bcrypt">>,
                   external_authentication_uid = <<"">>,
                   recovery_authentication_enabled = <<"0">>
               },
               
               %% Should call with Name for both parameters
               meck:expect(chef_sql, fetch_actors_by_name,
                           fun(_, <<"mock-org-id">>, <<"alice">>, <<"alice">>) ->
                               [User]
                           end),
               
               Context = chef_db:make_context(?API_MIN_VER, ?REQ_ID),
               Result = chef_db:fetch_requestor(Context, <<"mock-org-id">>, <<"alice">>),
               
               ?assertEqual(User, Result)
       end}
     ]
    }.
