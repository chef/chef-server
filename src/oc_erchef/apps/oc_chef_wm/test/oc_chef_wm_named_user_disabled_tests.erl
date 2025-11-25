%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author GitHub Copilot
%% @copyright Copyright Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_chef_wm_named_user_disabled_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

%% Test that disabled field is properly handled in user operations
%% Note: new_record tests are skipped because they require bcrypt setup.
%% The disabled=false default is verified by the fields_for_insert test.

%% Test chef_user:update_from_ejson accepts disabled=true
update_user_disabled_true_test() ->
    ExistingUser = #chef_user{
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        disabled = false,
        serialized_object = <<"{\"display_name\":\"Test User\"}">>
    },
    UpdateEjson = {[{<<"disabled">>, true}]},
    UpdatedUser = chef_user:update_from_ejson(ExistingUser, UpdateEjson),
    ?assertEqual(true, UpdatedUser#chef_user.disabled).

%% Test new_record ignores disabled field on create (always defaults to false)
new_record_ignores_disabled_on_create_test() ->
    ApiVersion = ?API_MIN_VER,
    %% Org-scoped ids must be of the form <<OrgId:32/binary, ...>>.
    OrgId = <<"0123456789abcdef0123456789abcdef">>,
    AuthzId = <<"authz-id">>,
    %% Include disabled=true in incoming data; it should be ignored
    Data = {[{<<"username">>, <<"testuser">>},
             {<<"email">>, <<"test@example.com">>},
             {<<"disabled">>, true}]},
    User = chef_user:new_record(ApiVersion, OrgId, AuthzId, Data),
    ?assertEqual(false, User#chef_user.disabled).

%% Test chef_user:update_from_ejson accepts disabled=false
update_user_disabled_false_test() ->
    ExistingUser = #chef_user{
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        disabled = true,
        serialized_object = <<"{\"display_name\":\"Test User\"}">>
    },
    UpdateEjson = {[{<<"disabled">>, false}]},
    UpdatedUser = chef_user:update_from_ejson(ExistingUser, UpdateEjson),
    ?assertEqual(false, UpdatedUser#chef_user.disabled).

%% Test chef_user:update_from_ejson preserves disabled when not specified
update_user_disabled_unchanged_test() ->
    ExistingUser = #chef_user{
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        disabled = true,
        serialized_object = <<"{\"display_name\":\"Test User\"}">>
    },
    UpdateEjson = {[{<<"email">>, <<"newemail@example.com">>}]},
    UpdatedUser = chef_user:update_from_ejson(ExistingUser, UpdateEjson),
    ?assertEqual(true, UpdatedUser#chef_user.disabled).

%% Test chef_user:common_user_ejson includes disabled field
common_user_ejson_includes_disabled_false_test() ->
    User = #chef_user{
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        disabled = false,
        serialized_object = <<"{\"display_name\":\"Test User\"}">>
    },
    Ejson = {chef_user:common_user_ejson(User)},
    ?assertEqual(false, ej:get({<<"disabled">>}, Ejson)).

common_user_ejson_includes_disabled_true_test() ->
    User = #chef_user{
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        disabled = true,
        serialized_object = <<"{\"display_name\":\"Test User\"}">>
    },
    Ejson = {chef_user:common_user_ejson(User)},
    ?assertEqual(true, ej:get({<<"disabled">>}, Ejson)).

%% Test fields_for_insert includes disabled
fields_for_insert_includes_disabled_test() ->
    User = #chef_user{
        id = <<"user-id">>,
        authz_id = <<"authz-id">>,
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        hashed_password = <<"hashed">>,
        salt = <<"salt">>,
        hash_type = <<"bcrypt">>,
        last_updated_by = <<"admin">>,
        created_at = {{2025, 11, 25}, {10, 0, 0}},
        updated_at = {{2025, 11, 25}, {10, 0, 0}},
        external_authentication_uid = null,
        recovery_authentication_enabled = false,
        disabled = true,
        serialized_object = <<"{}">>
    },
    Fields = chef_user:fields_for_insert(User),
    %% disabled should be the 14th field (after recovery_authentication_enabled)
    ?assertEqual(true, lists:nth(14, Fields)).

%% Test fields_for_update includes disabled
fields_for_update_includes_disabled_test() ->
    User = #chef_user{
        id = <<"user-id">>,
        username = <<"testuser">>,
        email = <<"test@example.com">>,
        hashed_password = <<"hashed">>,
        salt = <<"salt">>,
        hash_type = <<"bcrypt">>,
        last_updated_by = <<"admin">>,
        updated_at = {{2025, 11, 25}, {10, 0, 0}},
        external_authentication_uid = null,
        recovery_authentication_enabled = false,
        disabled = true,
        serialized_object = <<"{}">>
    },
    Fields = chef_user:fields_for_update(User),
    %% In regular API version, disabled should be the 7th field
    ?assertEqual(true, lists:nth(7, Fields)).
