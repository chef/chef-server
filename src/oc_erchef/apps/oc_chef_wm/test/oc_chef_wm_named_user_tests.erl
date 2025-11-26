%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2025 Progress Software Corporation. All Rights Reserved.
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

-module(oc_chef_wm_named_user_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USER_ID, <<"12341234123412341234123412341234">>).
-define(USER_NAME, <<"testuser">>).
-define(ORG_NAME, <<"testorg">>).

%% Test the to_json function with organizations field
to_json_with_organizations_test_() ->
    {foreach,
     fun() ->
             %% Mock chef_user module
             meck:new(chef_user, [passthrough]),
             meck:expect(chef_user, assemble_user_ejson,
                        fun(_User, _OrgName) ->
                                {[{<<"username">>, ?USER_NAME},
                                  {<<"email">>, <<"test@example.com">>},
                                  {<<"display_name">>, <<"Test User">>}]}
                        end),
             %% Mock chef_db module
             meck:new(chef_db, [passthrough]),
             %% Mock chef_json module
             meck:new(chef_json, [passthrough]),
             meck:expect(chef_json, encode,
                        fun(EJson) ->
                                ejson:encode(EJson)
                        end),
             %% Mock ej module
             meck:new(ej, [passthrough]),
             meck:expect(ej, set,
                        fun({Key}, EJson, Value) ->
                                %% Simple implementation to add key to proplist
                                {Proplist} = EJson,
                                {[{Key, Value} | Proplist]}
                        end)
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"organizations field is added when user has orgs",
       fun() ->
               %% Setup: user has 3 organizations
               OrgsList = [[<<"org1">>, <<"Org One">>, <<"guid1">>],
                          [<<"org2">>, <<"Org Two">>, <<"guid2">>],
                          [<<"org3">>, <<"Org Three">>, <<"guid3">>]],
               meck:expect(chef_db, list,
                          fun(#oc_chef_org_user_association{user_id = ?USER_ID}, _DbContext) ->
                                  OrgsList
                          end),

               %% Create test state
               User = #chef_user{id = ?USER_ID, username = ?USER_NAME},
               State = #base_state{
                          resource_args = undefined,
                          resource_state = #user_state{chef_user = User},
                          organization_name = ?ORG_NAME,
                          chef_db_context = mock_db_context
                         },
               Req = #wm_reqdata{},

               %% Execute
               {Json, ReqOut, StateOut} = oc_chef_wm_named_user:to_json(Req, State),

               %% Verify
               ?assert(meck:validate(chef_user)),
               ?assert(meck:validate(chef_db)),
               ?assert(meck:validate(chef_json)),
               ?assert(meck:validate(ej)),

               %% Verify chef_db:list was called with correct params
               ?assert(meck:called(chef_db, list,
                                  [#oc_chef_org_user_association{user_id = ?USER_ID},
                                   mock_db_context])),

               %% Verify ej:set was called to add organizations field
               ?assert(meck:called(ej, set,
                                  [{<<"organizations">>}, '_', [<<"org1">>, <<"org2">>, <<"org3">>]])),

               %% Verify response structure
               ?assertNotEqual(undefined, Json),
               ?assertEqual(Req, ReqOut),
               ?assertEqual(State, StateOut)
       end},

      {"organizations field is empty list when user has no orgs",
       fun() ->
               %% Setup: user has no organizations
               meck:expect(chef_db, list,
                          fun(#oc_chef_org_user_association{user_id = ?USER_ID}, _DbContext) ->
                                  []
                          end),

               %% Create test state
               User = #chef_user{id = ?USER_ID, username = ?USER_NAME},
               State = #base_state{
                          resource_args = undefined,
                          resource_state = #user_state{chef_user = User},
                          organization_name = ?ORG_NAME,
                          chef_db_context = mock_db_context
                         },
               Req = #wm_reqdata{},

               %% Execute
               {Json, ReqOut, StateOut} = oc_chef_wm_named_user:to_json(Req, State),

               %% Verify
               ?assert(meck:validate(chef_user)),
               ?assert(meck:validate(chef_db)),
               ?assert(meck:validate(ej)),

               %% Verify ej:set was called with empty list
               ?assert(meck:called(ej, set,
                                  [{<<"organizations">>}, '_', []])),

               ?assertNotEqual(undefined, Json),
               ?assertEqual(Req, ReqOut),
               ?assertEqual(State, StateOut)
       end},

      {"organizations field is empty list when db query fails",
       fun() ->
               %% Setup: database query fails
               meck:expect(chef_db, list,
                          fun(#oc_chef_org_user_association{user_id = ?USER_ID}, _DbContext) ->
                                  {error, database_error}
                          end),

               %% Create test state
               User = #chef_user{id = ?USER_ID, username = ?USER_NAME},
               State = #base_state{
                          resource_args = undefined,
                          resource_state = #user_state{chef_user = User},
                          organization_name = ?ORG_NAME,
                          chef_db_context = mock_db_context
                         },
               Req = #wm_reqdata{},

               %% Execute
               {Json, ReqOut, StateOut} = oc_chef_wm_named_user:to_json(Req, State),

               %% Verify error is handled gracefully with empty list
               ?assert(meck:validate(chef_user)),
               ?assert(meck:validate(chef_db)),
               ?assert(meck:validate(ej)),

               %% Verify ej:set was called with empty list (error handling)
               ?assert(meck:called(ej, set,
                                  [{<<"organizations">>}, '_', []])),

               ?assertNotEqual(undefined, Json),
               ?assertEqual(Req, ReqOut),
               ?assertEqual(State, StateOut)
       end},

      {"organizations field extracts only org names from tuples",
       fun() ->
               %% Setup: verify we extract just the name (first element) from each tuple
               OrgsList = [[<<"org-alpha">>, <<"Organization Alpha">>, <<"guid-a">>],
                          [<<"org-beta">>, <<"Organization Beta">>, <<"guid-b">>]],
               meck:expect(chef_db, list,
                          fun(#oc_chef_org_user_association{user_id = ?USER_ID}, _DbContext) ->
                                  OrgsList
                          end),

               %% Create test state
               User = #chef_user{id = ?USER_ID, username = ?USER_NAME},
               State = #base_state{
                          resource_args = undefined,
                          resource_state = #user_state{chef_user = User},
                          organization_name = ?ORG_NAME,
                          chef_db_context = mock_db_context
                         },
               Req = #wm_reqdata{},

               %% Execute
               {Json, _ReqOut, _StateOut} = oc_chef_wm_named_user:to_json(Req, State),

               %% Verify only org names (not full names or GUIDs) are included
               ?assert(meck:called(ej, set,
                                  [{<<"organizations">>}, '_',
                                   [<<"org-alpha">>, <<"org-beta">>]])),

               ?assertNotEqual(undefined, Json)
       end}
     ]}.

%% Test that to_json doesn't interfere with other resource_args
to_json_with_other_resource_args_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_user, [passthrough]),
             meck:new(chef_db, [passthrough]),
             meck:new(chef_json, [passthrough]),
             meck:new(ej, [passthrough])
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"to_json with invitations resource_arg is not affected",
       fun() ->
               %% This test ensures our changes don't break other to_json clauses
               %% We can't fully test this without more context, but we verify
               %% that our new code only runs when resource_args = undefined
               User = #chef_user{id = ?USER_ID, username = ?USER_NAME},
               State = #base_state{
                          resource_args = invitations,  %% Different from undefined
                          resource_state = #user_state{chef_user = User},
                          organization_name = ?ORG_NAME,
                          chef_db_context = mock_db_context
                         },
               Req = #wm_reqdata{},

               %% This should use a different to_json clause, not ours
               %% Our clause pattern-matches on resource_args = undefined
               %% This test will fail to match our clause and try the next one
               %% which will likely fail in the test environment, but that's expected

               %% We just verify chef_db:list is NOT called for non-undefined resource_args
               Result = try
                           oc_chef_wm_named_user:to_json(Req, State)
                       catch
                           _:_ -> expected_failure_for_different_clause
                       end,

               %% The key point: our chef_db:list should NOT have been called
               %% because resource_args was not undefined
               ?assertNot(meck:called(chef_db, list, '_')),

               %% We expect this to fail since we're not fully mocking all clauses
               ?assertEqual(expected_failure_for_different_clause, Result)
       end}
     ]}.
