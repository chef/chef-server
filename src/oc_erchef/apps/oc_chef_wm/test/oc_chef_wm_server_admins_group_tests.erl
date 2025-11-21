%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
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

-module(oc_chef_wm_server_admins_group_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% GLOBAL_PLACEHOLDER_ORG_ID is defined in chef_types.hrl (included via oc_chef_wm.hrl)
-define(SERVER_ADMINS_GROUP_NAME, <<"server-admins">>).
-define(SERVER_ADMINS_AUTHZ_ID, <<"11111111111111111111111111111111">>).

%%
%% Basic module behavior tests
%%

init_returns_ok_test() ->
    ?assertEqual({ok, #group_state{}}, oc_chef_wm_server_admins_group:init_resource_state([])).

request_type_returns_correct_type_test() ->
    ?assertEqual("server_admins_group", oc_chef_wm_server_admins_group:request_type()).

allowed_methods_returns_only_get_test() ->
    State = #base_state{},
    Req = mock_request(),
    {Methods, _Req1, _State1} = oc_chef_wm_server_admins_group:allowed_methods(Req, State),
    ?assertEqual(['GET'], Methods).

%%
%% validate_request tests
%%

validate_request_sets_correct_group_parameters_test() ->
    State = #base_state{resource_state = #group_state{}},
    Req = mock_request(),
    {_Req1, State1} = oc_chef_wm_server_admins_group:validate_request('GET', Req, State),
    
    %% Verify superuser_bypasses_checks is set
    ?assert(State1#base_state.superuser_bypasses_checks),
    
    %% Verify group state contains correct org_id and name
    GroupState = State1#base_state.resource_state,
    Group = GroupState#group_state.oc_chef_group,
    ?assertEqual(?GLOBAL_PLACEHOLDER_ORG_ID, Group#oc_chef_group.org_id),
    ?assertEqual(?SERVER_ADMINS_GROUP_NAME, Group#oc_chef_group.name).

%%
%% forbidden tests
%%

forbidden_allows_superuser_test() ->
    {foreach,
     fun() ->
         meck:new(oc_chef_wm_base),
         meck:expect(oc_chef_wm_base, is_superuser, fun(_Req) -> true end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"superuser is allowed",
       fun() ->
           State = #base_state{},
           Req = mock_request(),
           {Result, _Req1, _State1} = oc_chef_wm_server_admins_group:forbidden(Req, State),
           ?assertEqual(false, Result)
       end}
     ]}.

forbidden_blocks_non_superuser_test() ->
    {foreach,
     fun() ->
         meck:new(oc_chef_wm_base),
         meck:new(chef_wm_util),
         meck:expect(oc_chef_wm_base, is_superuser, fun(_Req) -> false end),
         meck:expect(chef_wm_util, set_json_body, fun(Req, _Body) -> Req end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"non-superuser is forbidden",
       fun() ->
           State = #base_state{},
           Req = mock_request(),
           {Result, _Req1, State1} = oc_chef_wm_server_admins_group:forbidden(Req, State),
           ?assertMatch({{halt, 403}, _, _}, {Result, Req, State1}),
           ?assertEqual(server_admins_forbidden, State1#base_state.log_msg),
           %% Verify set_json_body was called with proper error structure
           ?assert(meck:called(chef_wm_util, set_json_body, [Req, {[{<<"error">>, <<"Access to server-admins group is restricted to superuser only">>}]}]))
       end}
     ]}.

%%
%% auth_info tests
%%

auth_info_returns_404_when_group_not_found_test() ->
    {foreach,
     fun() ->
         meck:new(chef_db),
         meck:new(chef_wm_util),
         meck:expect(chef_db, fetch, fun(_Group, _DbContext) -> not_found end),
         meck:expect(chef_wm_util, error_message_envelope, fun(_Msg) -> {[{<<"error">>, <<"not found">>}]} end),
         meck:expect(chef_wm_util, set_json_body, fun(Req, _Body) -> Req end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"group not found returns 404",
       fun() ->
           State = #base_state{
               chef_db_context = mock_db_context,
               resource_state = #group_state{},
               requestor_id = <<"test_requestor">>
           },
           Req = mock_request(),
           {Result, _Req1, State1} = oc_chef_wm_server_admins_group:auth_info(Req, State),
           ?assertMatch({{halt, 404}, _, _}, {Result, Req, State1}),
           ?assertEqual(server_admins_group_not_found, State1#base_state.log_msg)
       end}
     ]}.

auth_info_returns_group_authz_id_when_found_test() ->
    {foreach,
     fun() ->
         meck:new(chef_db),
         meck:expect(chef_db, fetch, fun(_Group, _DbContext) ->
             #oc_chef_group{
                 authz_id = ?SERVER_ADMINS_AUTHZ_ID,
                 org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
                 name = ?SERVER_ADMINS_GROUP_NAME
             }
         end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"group found returns authz_id",
       fun() ->
           State = #base_state{
               chef_db_context = mock_db_context,
               resource_state = #group_state{},
               requestor_id = <<"test_requestor">>
           },
           Req = mock_request(),
           {Result, _Req1, _State1} = oc_chef_wm_server_admins_group:auth_info(Req, State),
           ?assertEqual({group_id, ?SERVER_ADMINS_AUTHZ_ID}, Result)
       end}
     ]}.

%%
%% resource_exists tests
%%

resource_exists_always_returns_true_test() ->
    State = #base_state{},
    Req = mock_request(),
    {Result, _Req1, _State1} = oc_chef_wm_server_admins_group:resource_exists(Req, State),
    ?assertEqual(true, Result).

%%
%% to_json tests
%%

to_json_returns_properly_formatted_json_test() ->
    {foreach,
     fun() ->
         meck:new(oc_chef_authz_http),
         meck:new(chef_sql),
         meck:new(chef_json),
         
         %% Mock bifrost response
         meck:expect(oc_chef_authz_http, request, 
             fun("/groups/" ++ _, get, _Headers, [], _RequestorId) ->
                 {ok, {[
                     {<<"actors">>, [<<"user1_authz">>, <<"client1_authz">>]},
                     {<<"groups">>, []}
                 ]}}
             end),
         
         %% Mock user name lookup
         meck:expect(chef_sql, select_rows,
             fun({find_user_name_in_authz_ids, [[<<"user1_authz">>, <<"client1_authz">>]]}) ->
                 [[{<<"username">>, <<"pivotal">>}, {<<"authz_id">>, <<"user1_authz">>}]];
                ({find_client_name_in_authz_ids, [[<<"user1_authz">>, <<"client1_authz">>]]}) ->
                 [[{<<"name">>, <<"client1">>}, {<<"authz_id">>, <<"client1_authz">>}]]
             end),
         
         %% Mock chef_json:encode
         meck:expect(chef_json, encode, fun(_Ejson) -> <<"{\"name\":\"server-admins\"}">> end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"to_json fetches members and builds proper JSON",
       fun() ->
           Group = #oc_chef_group{
               authz_id = ?SERVER_ADMINS_AUTHZ_ID,
               org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
               name = ?SERVER_ADMINS_GROUP_NAME
           },
           State = #base_state{
               chef_db_context = mock_db_context,
               requestor_id = <<"test_requestor">>,
               resource_state = #group_state{oc_chef_group = Group}
           },
           Req = mock_request(),
           
           {Json, _Req1, _State1} = oc_chef_wm_server_admins_group:to_json(Req, State),
           
           %% Verify bifrost was queried
           ?assert(meck:called(oc_chef_authz_http, request, 
               ["/groups/" ++ binary_to_list(?SERVER_ADMINS_AUTHZ_ID), get, '_', [], <<"test_requestor">>])),
           
           %% Verify user/client lookups were called
           ?assert(meck:called(chef_sql, select_rows, [{find_user_name_in_authz_ids, '_'}])),
           ?assert(meck:called(chef_sql, select_rows, [{find_client_name_in_authz_ids, '_'}])),
           
           %% Verify chef_json:encode was called
           ?assert(meck:called(chef_json, encode, '_')),
           
           %% Verify JSON is returned
           ?assertEqual(<<"{\"name\":\"server-admins\"}">>, Json)
       end}
     ]}.

to_json_handles_populated_group_members_test() ->
    {foreach,
     fun() ->
         meck:new(oc_chef_group),
         meck:new(chef_json),
         meck:expect(oc_chef_group, assemble_group_ejson, fun(_Group, null) ->
             {[
               {<<"actors">>, [<<"pivotal">>, <<"user2">>, <<"client1">>, <<"client2">>]},
               {<<"users">>, [<<"pivotal">>, <<"user2">>]},
               {<<"clients">>, [<<"client1">>, <<"client2">>]},
               {<<"groups">>, [<<"admins">>, <<"operators">>]},
               {<<"orgname">>, null},
               {<<"name">>, ?SERVER_ADMINS_GROUP_NAME},
               {<<"groupname">>, ?SERVER_ADMINS_GROUP_NAME}
             ]}
         end),
         meck:expect(chef_json, encode, fun(Ejson) -> 
             %% Simple encoding that includes all members
             erlang:iolist_to_binary(io_lib:format("~p", [Ejson]))
         end)
     end,
     fun(_) -> meck:unload() end,
     [
      {"to_json handles group with multiple users, clients, and groups",
       fun() ->
           Group = #oc_chef_group{
               authz_id = ?SERVER_ADMINS_AUTHZ_ID,
               org_id = ?GLOBAL_PLACEHOLDER_ORG_ID,
               name = ?SERVER_ADMINS_GROUP_NAME,
               users = [<<"pivotal">>, <<"user2">>],
               clients = [<<"client1">>, <<"client2">>],
               groups = [<<"admins">>, <<"operators">>]
           },
           State = #base_state{
               resource_state = #group_state{oc_chef_group = Group}
           },
           Req = mock_request(),
           
           {_Json, _Req1, _State1} = oc_chef_wm_server_admins_group:to_json(Req, State),
           
           %% Verify the function was called with the populated group
           ?assert(meck:called(oc_chef_group, assemble_group_ejson, [Group, null]))
       end}
     ]}.

%%
%% malformed_request_message tests
%%

malformed_request_message_throws_error_test() ->
    %% This function should never be called in normal operation
    %% It exists to catch unexpected cases and should throw an error
    ?assertError({unexpected_malformed_request_message, test_value},
                 oc_chef_wm_server_admins_group:malformed_request_message(test_value, mock_req, #base_state{})).

%%
%% Helper functions
%%

mock_request() ->
    %% Simple mock request that satisfies basic requirements
    mock_req.
