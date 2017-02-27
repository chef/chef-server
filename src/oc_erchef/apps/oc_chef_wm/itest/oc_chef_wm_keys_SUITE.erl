%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_keys_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(CLIENT_NAME, <<"client1">>).
-define(CLIENT_NAME2, <<"client2">>).
-define(CLIENT_AUTHZ_ID, <<"00000000000000000000000000000003">>).

-define(USER_NAME, <<"user1">>).
-define(USER_NAME2, <<"user2">>).
-define(ADMIN_USER_NAME, <<"admin">>).
-define(USER_AUTHZ_ID, <<"00000000000000000000000000000004">>).
-define(USER2_AUTHZ_ID, <<"00000000000000000000000000000006">>).
-define(ADMIN_AUTHZ_ID, <<"90000000000000000000000000000004">>).

-define(ORG_NAME, <<"testorg">>).
-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).

-define(ORG_KEYS_ACCESS_GROUP_NAME, "public_key_read_access").
-define(ORG_KEYS_ACCESS_GROUP_AUTHZ_ID, <<"20000000000000000000000000000002">>).

-define(KEY1NAME, <<"key1">>).
-define(KEY1EXPIRE, {datetime, {{2099,12,31},{00,00,00}}}).
-define(KEY1EXPIRESTRING, <<"2099-12-31T00:00:00Z">>).
-define(KEY2NAME, <<"key2">>).
-define(KEY2EXPIRE, {datetime, {{2010,12,31},{00,00,00}}}).
-define(KEY2EXPIRESTRING, <<"2010-12-31T00:00:00Z">>).

-define(DEFAULT_KEY_ENTRY, {<<"default">>, false}).
-define(KEY_1_ENTRY, { ?KEY1NAME, false } ).
-define(KEY_2_ENTRY, { ?KEY2NAME, true } ).

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db([{app, oc_chef_wm}|LastConfig], "oc_chef_wm_itests"),
    Config2 = setup_helper:start_server(Config),
    make_org(?ORG_NAME, ?ORG_AUTHZ_ID),
    {OrgId, _} = chef_db:fetch_org_metadata(context(), ?ORG_NAME),
    {ok, PubKey} = file:read_file(filename:join([?config(data_dir, Config2), "..","spki_public.pem"])),
    {ok, AltPubKey} = file:read_file(filename:join([?config(data_dir, Config2), "..","public.pem"])),
    {ok, PrivateKeyRE} = re:compile(".*BEGIN (RSA )?PRIVATE KEY.*"),
    {ok, PubKeyRE} = re:compile(".*BEGIN (RSA )?PUBLIC KEY.*"),
    [{org_id, OrgId}, {pubkey, PubKey}, {alt_pubkey, AltPubKey},{pubkey_regex, PubKeyRE}, {privkey_regex, PrivateKeyRE}] ++ Config2.

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

load_mocks() ->
    meck:new(oc_chef_authz_db, [passthrough]),
    meck:expect(oc_chef_authz_db, fetch_group, fun(_Context, _OrgName, _GroupName) ->
						       #oc_chef_group{server_api_version=?API_MIN_VER,
								      id=nil,
								      for_requestor_id=nil,
								      authz_id=?ORG_KEYS_ACCESS_GROUP_AUTHZ_ID,
								      org_id=?ORG_AUTHZ_ID,
								      name=?ORG_KEYS_ACCESS_GROUP_NAME,
								      last_updated_by=nil,
								      created_at=nil,
								      updated_at=nil}
						      end).

unload_mocks() ->
    meck:unload(oc_chef_authz_db).

all() ->
    [list_client_default_key,
     list_user_default_key,
     list_client_multiple_keys,
     list_user_multiple_keys,
     list_client_no_keys,
     list_user_no_keys,
     get_client_default_key,
     get_user_default_key,
     get_client_multiple_keys,
     get_user_multiple_keys,
     get_client_no_keys,
     get_user_no_keys,
     get_client_wrong_key,
     get_user_wrong_key,
     get_key_for_nonexistent_user,
     get_key_for_nonexistent_client,
     post_user_new_valid_key,
     post_user_create_key,
     post_client_new_valid_key,
     post_client_create_key,
     post_new_key_invalid_date,
     post_new_key_invalid_utc_date,
     post_new_key_invalid_digits_date,
     post_new_key_well_formed_invalid_date,
     post_key_with_infinity_date,
     post_key_with_invalid_key_name,
     post_key_with_invalid_public_key,
     post_conflicting_user_key,
     post_conflicting_client_key,
     post_multiple_valid_user_keys,
     post_multiple_valid_client_keys,
     % put_authing_user_key_with_different_user,
     % put_authing_user_key,
     % delete_authing_user_key_with_different_user,
     % delete_authing_user_key,
     delete_valid_user_key,
     delete_invalid_user_key,
     % delete_authing_client_key_with_different_user,
     % delete_authing_client_key,
     % put_authing_client_key_with_different_user,
     % put_authing_client_key,
     delete_valid_client_key,
     delete_invalid_client_key,
     put_valid_partial_client_key,
     put_valid_partial_user_key,
     put_rename_client_key,
     put_rename_user_key,
     put_rename_duplicate_client_key,
     put_rename_duplicate_user_key,
     put_invalid_partial_client_key,
     put_invalid_partial_user_key,
     put_full_client_key,
     put_generate_new_client_key,
     put_full_user_key,
     put_generate_new_user_key
     ].

%% GET /organizations/org/clients/client/keys && GET /users/client/keys
list_client_default_key(_) ->
    load_mocks(),
    Result = http_key_request(get, client, ?CLIENT_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = client_key_list_ejson(?CLIENT_NAME, [?DEFAULT_KEY_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    unload_mocks().

list_user_default_key(_) ->
    Result = http_key_request(get, user, ?USER_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = user_key_list_ejson(?USER_NAME, [?DEFAULT_KEY_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ).

list_client_multiple_keys(_) ->
    load_mocks(),
    Result = http_key_request(get, client, ?CLIENT_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = client_key_list_ejson(?CLIENT_NAME, [?DEFAULT_KEY_ENTRY, ?KEY_1_ENTRY, ?KEY_2_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ),
    unload_mocks().

list_user_multiple_keys(_) ->
    Result = http_key_request(get, user, ?USER_NAME),
    ?assertMatch({ok, "200", _, _} , Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = user_key_list_ejson(?USER_NAME, [?DEFAULT_KEY_ENTRY, ?KEY_1_ENTRY, ?KEY_2_ENTRY]),
    ?assertMatch(ExpectedEJ, BodyEJ).

list_client_no_keys(_) ->
    load_mocks(),
    Result = http_key_request(get, client, ?ADMIN_USER_NAME),
    ?assertMatch({ok, "200", _, "[]"} , Result),
    unload_mocks().

list_user_no_keys(_) ->
    Result = http_key_request(get, user, ?ADMIN_USER_NAME),
    ?assertMatch({ok, "200", _, "[]"} , Result).

%% GET /organizations/org/clients/client/keys/key && GET /users/client/keys/key
get_client_default_key(Config) ->
    load_mocks(),
    Result = http_named_key_request(get, client, ?CLIENT_NAME, "default"),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, <<"default">>, <<"infinity">>),
    ?assertMatch(ExpectedEJ, BodyEJ),
    unload_mocks().

get_user_default_key(Config) ->
    Result = http_named_key_request(get, user, ?USER_NAME, "default"),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, <<"default">>, <<"infinity">>),
    ?assertMatch(ExpectedEJ, BodyEJ).

get_client_multiple_keys(Config) ->
    load_mocks(),
    %% KEY1
    Result = http_named_key_request(get, client, ?CLIENT_NAME, ?KEY1NAME),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, ?KEY1NAME, ?KEY1EXPIRESTRING),
    ?assertMatch(ExpectedEJ, BodyEJ),

    %% KEY2
    Result2 = http_named_key_request(get, client, ?CLIENT_NAME, ?KEY2NAME),
    ?assertMatch({ok, "200", _, _} , Result2),
    BodyEJ2 = decoded_response_body(Result2),
    ExpectedEJ2 = new_key_ejson(Config, ?KEY2NAME, ?KEY2EXPIRESTRING),
    ?assertMatch(ExpectedEJ2, BodyEJ2),
    unload_mocks().

get_user_multiple_keys(Config) ->
    %% KEY1
    Result = http_named_key_request(get, user, ?USER_NAME, ?KEY1NAME),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, ?KEY1NAME, ?KEY1EXPIRESTRING),
    ?assertMatch(ExpectedEJ, BodyEJ),

    %% KEY2
    Result2 = http_named_key_request(get, user, ?USER_NAME, ?KEY2NAME),
    ?assertMatch({ok, "200", _, _}, Result2),
    BodyEJ2 = decoded_response_body(Result2),
    ExpectedEJ2 = new_key_ejson(Config, ?KEY2NAME, ?KEY2EXPIRESTRING),
    ?assertMatch(ExpectedEJ2, BodyEJ2).

get_client_no_keys(_) ->
    Result = http_named_key_request(get, client, ?ADMIN_USER_NAME, "default"),
    ?assertMatch({ok, "404", _, _} , Result).

get_user_no_keys(_) ->
    Result = http_named_key_request(get, user, ?ADMIN_USER_NAME, "default"),
    ?assertMatch({ok, "404", _, _} , Result).

get_client_wrong_key(_) ->
    Result = http_named_key_request(get, client, ?ADMIN_USER_NAME, "wrong_key"),
    ?assertMatch({ok, "404", _, _} , Result).

get_user_wrong_key(_) ->
    Result = http_named_key_request(get, user, ?ADMIN_USER_NAME, "wrong_key"),
    ?assertMatch({ok, "404", _, _} , Result).

get_key_for_nonexistent_user(_) ->
    Result = http_named_key_request(get, user, ?ADMIN_USER_NAME, "default"),
    ?assertMatch({ok, "404", _, _} , Result).

get_key_for_nonexistent_client(_) ->
    Result = http_named_key_request(get, client, ?ADMIN_USER_NAME, "default"),
    ?assertMatch({ok, "404", _, _} , Result).

delete_valid_client_key(Config) ->
    Result = http_named_key_request(delete, client, ?CLIENT_NAME, ?KEY1NAME),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, ?KEY1NAME, ?KEY1EXPIRESTRING),
    ?assertMatch(BodyEJ, ExpectedEJ).

delete_invalid_client_key(_Config) ->
    Result = http_named_key_request(delete, client, ?CLIENT_NAME, "invalid_key"),
    ?assertMatch({ok, "404", _, _}, Result).

put_rename_client_key(_Config) ->
    load_mocks(),
    validate_rename(client , "201", "404"),
    unload_mocks().

put_rename_duplicate_client_key(_Config) ->
    load_mocks(),
    validate_rename(client, "409", "200"),
    unload_mocks().

put_rename_user_key(_Config) ->
    validate_rename(user, "201", "404").

put_rename_duplicate_user_key(_Config) ->
    validate_rename(user, "409", "200").

put_full_client_key(Config) ->
    load_mocks(),
    validate_put_full_key(Config, client),
    unload_mocks().

put_full_user_key(Config) ->
    validate_put_full_key(Config, user).

put_generate_new_client_key(Config) ->
    validate_put_partial_key_valid(Config, client, <<"create_key">>, true).

put_generate_new_user_key(Config) ->
    validate_put_partial_key_valid(Config, user, <<"create_key">>, true).

put_valid_partial_user_key(Config) ->
    load_mocks(),
    validate_put_partial_key_valid(Config, user, <<"expiration_date">>, ?KEY2EXPIRESTRING),
    validate_put_partial_key_valid(Config, user, <<"public_key">>, proplists:get_value(alt_pubkey, Config)),
    unload_mocks().

put_valid_partial_client_key(Config) ->
    load_mocks(),
    validate_put_partial_key_valid(Config, client, <<"expiration_date">>, ?KEY2EXPIRESTRING),
    validate_put_partial_key_valid(Config, client, <<"public_key">>, proplists:get_value(alt_pubkey, Config)),
    unload_mocks().

put_invalid_partial_user_key(_Config) ->
    validate_put_partial_key_invalid(user, <<"name">>, <<"bob^was^here">>),
    validate_put_partial_key_invalid(user, <<"expiration_date">>, <<"yesterday">>),
    validate_put_partial_key_invalid(user, <<"public_key">>, <<"bad key">>).

put_invalid_partial_client_key(_Config) ->
    validate_put_partial_key_invalid(client, <<"name">>, <<"bob^was^here">>),
    validate_put_partial_key_invalid(client, <<"expiration_date">>, <<"yesterday">>),
    validate_put_partial_key_invalid(client, <<"public_key">>, <<"bad key">>).

% TODO - authing requestor
% I'm leaving this commented out but present, because I'd like to find a way to make it work.
% Currently, our stubbed chef_authn mock can't populate a requestor id, which means
% our check to see if keyname + requestor id matches current auth will always pass without
% matching, because required id is never populated.
% I think we can do this by making our mock chef_authn a stateful process (gen_server would be simplest)
% which would allow us to set the requestor id to mock for each reque We could also then extend it
% to perform better authz behavior validation throughout our tests
% We'll want to add similar tests for PUT.
%delete_authing_client_key(_Config) ->
    %Result = http_named_key_request(delete, client, ?CLIENT_NAME, "default"),
    %?assertMatch({ok, "403", _, _}, Result).

%delete_authing_client_key_with_different_user(_Config) ->
    %% This is specifically intended to validate that we can use a different user's
    %% "default" key to authenticate, and delete the client's "default" key without
    %% failing simply because the key name match(The stubbed authz check
    %% will match on key 'default')
    %Result = http_named_key_request(delete, client, ?ADMIN_USER_NAME, "default"),
    %?assertMatch({ok, "200", _, _}, Result).

delete_valid_user_key(Config) ->
    Result = http_named_key_request(delete, user, ?USER_NAME, ?KEY1NAME),
    ?assertMatch({ok, "200", _, _}, Result),
    BodyEJ = decoded_response_body(Result),
    ExpectedEJ = new_key_ejson(Config, ?KEY1NAME, ?KEY1EXPIRESTRING),
    ?assertMatch(BodyEJ, ExpectedEJ).

delete_invalid_user_key(_Config) ->
    Result = http_named_key_request(delete, user, ?USER_NAME, "invalid_key"),
    ?assertMatch({ok, "404", _, _}, Result).

% see "TODO - authing requestor" above
%delete_authing_user_key(_Config) ->
    %Result = http_named_key_request(delete, user, ?USER_NAME, "default"),
    %?assertMatch({ok, "403", _, _}, Result).

%delete_authing_user_key_with_different_user(_Config) ->
    %Result = http_named_key_request(delete, user, ?ADMIN_USER_NAME, "default"),
    %?assertMatch({ok, "200", _, _}, Result).

%% POST /organizations/org/clients/client/keys && POST /users/client/keys
post_client_new_valid_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-24T22:49:08Z">>)),
    Result = http_key_request(post, client, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result).

post_client_create_key(Config) ->
    EJ = new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08Z">>, create_key),
    Body = chef_json:encode(EJ),
    Result = http_key_request(post, client, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result),
    ResultEJ =  decoded_response_body(Result),
    ?assertEqual(false, has_well_formed_public_key(Config, ResultEJ)),
    ?assertEqual(true, has_well_formed_private_key(Config, ResultEJ)).


post_user_new_valid_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08Z">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result).

post_user_create_key(Config) ->
    EJ = new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08Z">>, create_key),
    Body = chef_json:encode(EJ),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result),
    ResultEJ =  decoded_response_body(Result),
    ?assertEqual(false, has_well_formed_public_key(Config, ResultEJ)),
    ?assertEqual(true, has_well_formed_private_key(Config, ResultEJ)).

post_new_key_invalid_date(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"bad-date">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result),

    {_, _, _, UnparsedMessage} = Result,
    [ParsedMessage] = ej:get({<<"error">>},chef_json:decode(UnparsedMessage)),
    ExpectedMessage = ?BAD_DATE_MESSAGE(<<"expiration_date">>),
    ?assertMatch(ExpectedMessage, ParsedMessage).

post_new_key_invalid_utc_date(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-24T22:49:08">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result),

    {_, _, _, UnparsedMessage} = Result,
    [ParsedMessage] = ej:get({<<"error">>},chef_json:decode(UnparsedMessage)),
    ExpectedMessage = ?BAD_DATE_MESSAGE(<<"expiration_date">>),
    ?assertMatch(ExpectedMessage, ParsedMessage).

post_new_key_invalid_digits_date(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2-1-2T2:4:0Z">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result),

    {_, _, _, UnparsedMessage} = Result,
    [ParsedMessage] = ej:get({<<"error">>},chef_json:decode(UnparsedMessage)),
    ExpectedMessage = ?BAD_DATE_MESSAGE(<<"expiration_date">>),
    ?assertMatch(ExpectedMessage, ParsedMessage).

post_new_key_well_formed_invalid_date(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2010-01-35T00:00:00Z">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result),

    {_, _, _, UnparsedMessage} = Result,
    [ParsedMessage] = ej:get({<<"error">>},chef_json:decode(UnparsedMessage)),
    ExpectedMessage = ?BAD_DATE_MESSAGE(<<"expiration_date">>),
    ?assertMatch(ExpectedMessage, ParsedMessage).

post_key_with_infinity_date(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"infinity">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "201", _, _}, Result).

post_key_with_invalid_key_name(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"invalid^character">>, <<"2099-10-25T22:49:08Z">>)),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result).

post_key_with_invalid_public_key(_) ->
    Ejson = {[{name, <<"test1">>}, {public_key, <<"-----BEGIN PUBLIC KEY-----\ninvalid_key\n-----END PUBLIC KEY-----">>}, {expiration_date, <<"2099-10-25T22:49:08Z">>}]},
    Body = chef_json:encode(Ejson),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "400", _, _}, Result).

post_conflicting_user_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08Z">>)),
    http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    Result = http_key_request(post, user, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "409", _, _}, Result).

post_conflicting_client_key(Config) ->
    Body = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-24T22:49:08Z">>)),
    http_key_request(post, client, ?ADMIN_USER_NAME, Body),
    Result = http_key_request(post, client, ?ADMIN_USER_NAME, Body),
    ?assertMatch({ok, "409", _, _}, Result).

post_multiple_valid_user_keys(Config) ->
    Body1 = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-25T22:49:08Z">>)),
    Result1 = http_key_request(post, user, ?ADMIN_USER_NAME, Body1),
    ?assertMatch({ok, "201", _, _}, Result1),
    Body2 = chef_json:encode(new_key_ejson(Config, <<"test2">>, <<"2099-10-25T22:49:08Z">>)),
    Result2 = http_key_request(post, user, ?ADMIN_USER_NAME, Body2),
    ?assertMatch({ok, "201", _, _}, Result2).

post_multiple_valid_client_keys(Config) ->
    Body1 = chef_json:encode(new_key_ejson(Config, <<"test1">>, <<"2099-10-24T22:49:08Z">>)),
    Result1 = http_key_request(post, client, ?ADMIN_USER_NAME, Body1),
    ?assertMatch({ok, "201", _, _}, Result1),
    Body2 = chef_json:encode(new_key_ejson(Config, <<"test2">>, <<"2099-10-24T22:49:08Z">>)),
    Result2 = http_key_request(post, client, ?ADMIN_USER_NAME, Body2),
    ?assertMatch({ok, "201", _, _}, Result2).

%% Test case initializers
init_per_testcase(TestCase, Config) ->
    setup_helper:mock_authz(?CLIENT_AUTHZ_ID),
    init_per_testcase0(TestCase, Config).

init_per_testcase0(TestCase, Config) when TestCase =:= post_new_key_invalid_date;
                                          TestCase =:= post_new_key_invalid_digits_date;
                                          TestCase =:= post_new_key_well_formed_invalid_date;
                                          TestCase =:= post_new_key_invalid_utc_date ->
    make_admin_non_admin_and_client(Config);
init_per_testcase0(TestCase, Config) when TestCase =:= post_client_new_valid_key;
                                          TestCase =:= post_client_create_key;
                                          TestCase =:= post_conflicting_client_key;
                                          TestCase =:= post_multiple_valid_client_keys ->
    make_admin_and_client(Config);
init_per_testcase0(TestCase, Config) when TestCase =:= post_user_new_valid_key;
                                          TestCase =:= post_user_create_key;
                                          TestCase =:= post_key_with_infinity_date;
                                          TestCase =:= post_key_with_invalid_key_name;
                                          TestCase =:= post_key_with_invalid_public_key;
                                          TestCase =:= post_conflicting_user_key;
                                          TestCase =:= post_multiple_valid_user_keys ->
    make_admin_non_admin_and_client(Config);
init_per_testcase0(list_user_default_key,  Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    Config;
init_per_testcase0(list_client_default_key, Config) ->
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase0(Case, Config) when Case =:= list_client_multiple_keys;
                                      Case =:= put_rename_duplicate_client_key ->
    make_client(Config, ?CLIENT_NAME),
    ClientId = client_id(Config, ?CLIENT_NAME),
    add_key(Config, ClientId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, ClientId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase0(Case, Config) when Case =:= list_user_multiple_keys;
                                      Case =:= put_rename_duplicate_user_key->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    UserId = user_id(?USER_NAME),
    add_key(Config, UserId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, UserId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase0(TestCase, Config) when TestCase =:= get_client_no_keys;
                                          TestCase =:= list_client_no_keys ->
    make_client(Config, ?CLIENT_NAME),
    sqerl:adhoc_delete(<<"keys">>, all),
    % make this user after clearing keys, so that we have a user
    % who can make the request.
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase0(list_user_no_keys, Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    sqerl:adhoc_delete(<<"keys">>, all),
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase0(get_user_default_key,  Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    Config;
init_per_testcase0(get_client_default_key, Config) ->
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase0(get_client_multiple_keys, Config) ->
    make_client(Config, ?CLIENT_NAME),
    ClientId = client_id(Config, ?CLIENT_NAME),
    add_key(Config, ClientId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, ClientId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase0(get_user_multiple_keys, Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    UserId = user_id(?USER_NAME),
    add_key(Config, UserId, ?KEY1NAME, ?KEY1EXPIRE),
    add_key(Config, UserId, ?KEY2NAME, ?KEY2EXPIRE),
    Config;
init_per_testcase0(get_user_no_keys, Config) ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    sqerl:adhoc_delete(<<"keys">>, all),
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase0(get_client_wrong_key, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_client(Config, ?CLIENT_NAME),
    Config;
init_per_testcase0(get_user_wrong_key, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    Config;
init_per_testcase0(get_key_for_nonexistent_user, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase0(get_key_for_nonexistent_client, Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    Config;
init_per_testcase0(Case, Config) when Case =:= delete_invalid_client_key;
                                      Case =:= delete_valid_client_key;
                                      Case =:= put_rename_client_key;
                                      Case =:= put_valid_partial_client_key;
                                      Case =:= put_invalid_partial_client_key;
                                      Case =:= put_full_client_key;
                                      Case =:= put_generate_new_client_key ->
    make_client(Config, ?CLIENT_NAME),
    ClientId = client_id(Config, ?CLIENT_NAME),
    add_key(Config, ClientId, ?KEY1NAME, ?KEY1EXPIRE),
    Config;
% TODO see "TODO - authing requestor" above.
%init_per_testcase0(delete_authing_client_key, Config) ->
%make_client(Config, ?CLIENT_NAME),
%Config;
%init_per_testcase0(delete_authing_client_key_with_different_user, Config) ->
%make_admin_and_client(Config),
%ClientId = client_id(Config, ?CLIENT_NAME),
%add_key(Config, ClientId, ?KEY1NAME, ?KEY1EXPIRE),
%Config;
init_per_testcase0(Case, Config) when Case =:= delete_invalid_user_key;
                                      Case =:= delete_valid_user_key;
                                      Case =:= put_rename_user_key;
                                      Case =:= put_rename_duplicate_user_key;
                                      Case =:= put_valid_partial_user_key;
                                      Case =:= put_invalid_partial_user_key;
                                      Case =:= put_full_user_key;
                                      Case =:= put_generate_new_user_key ->
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    UserId = user_id(?USER_NAME),
    add_key(Config, UserId, ?KEY1NAME, ?KEY1EXPIRE),
    Config.

% TODO see "TODO - authing requestor" above.
%init_per_testcase0(delete_authing_user_key, Config) ->
%    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
%    Config;
%init_per_testcase0(delete_authing_user_key_with_different_user, Config) ->
%    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
%    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
%    UserId = user_id(?USER_NAME),
%    add_key(Config, UserId, ?KEY1NAME, ?KEY1EXPIRE),
%    Config;
%init_per_testcase0(_, Config) ->
%    Config.

%% Test case cleanup
end_per_testcase(_, Config) ->
    setup_helper:unmock_authz(),
    sqerl:adhoc_delete("clients", all),
    sqerl:adhoc_delete("users", all),
    Config.

make_admin_non_admin_and_client(Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_user(Config, ?USER_NAME, ?USER_AUTHZ_ID),
    make_client(Config, ?CLIENT_NAME),
    Config.

make_admin_and_client(Config) ->
    make_user(Config, ?ADMIN_USER_NAME, ?ADMIN_AUTHZ_ID),
    make_client(Config, ?CLIENT_NAME),
    Config.

http_key_request(Method, Type, Requestor) ->
    http_key_request(Method, Type, Requestor, <<>>).

http_key_request(Method, user, Requestor, Body) ->
    Url = "http://localhost:8000/users/user1/keys",
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body);
http_key_request(Method, client, Requestor, Body) ->
    Url = "http://localhost:8000/organizations/testorg/clients/client1/keys",
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body).

http_named_key_request(Method, Type, Requestor, Name) ->
    http_named_key_request(Method, Type, Requestor, Name, <<>>).

http_named_key_request(Method, user, Requestor, Name, Body) ->
    Url = "http://localhost:8000/users/user1/keys/" ++ Name,
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body);
http_named_key_request(Method, client, Requestor, Name, Body) ->
    Url = "http://localhost:8000/organizations/testorg/clients/client1/keys/" ++ Name,
    ibrowse:send_req(Url, [{"x-ops-userid", binary_to_list(Requestor)},
                           {"accept", "application/json"},
                           {"content-type", "application/json"}], Method, Body).

% Some helpers to keep noise out of the test.
make_org(OrgName, OrgAuthzId) ->
    Org = chef_object:new_record(oc_chef_organization, ?API_MIN_VER, nil, OrgAuthzId,
                                 {[{<<"name">>, OrgName}, {<<"full_name">>, OrgName}]}),
    chef_db:create(Org, context(), OrgAuthzId).

make_client(Config, Name) ->
    OrgId = proplists:get_value(org_id, Config),
    PubKey = proplists:get_value(pubkey, Config),
    Client = chef_object:new_record(chef_client, ?API_MIN_VER, OrgId, ?CLIENT_AUTHZ_ID,
                                    {[{<<"name">>, Name},
                                      {<<"validator">>, true},
                                      {<<"admin">>, true},
                                      {<<"public_key">>, PubKey}]}),
    chef_db:create(Client, context(), ?CLIENT_AUTHZ_ID).

make_user(Config, Name, AuthzId) ->
    OrgId = proplists:get_value(org_id, Config),
    make_user(Config, Name, AuthzId, OrgId).

make_user(Config, Name, AuthzId, OrgId) ->
    PubKey = proplists:get_value(pubkey, Config),
    Dom = <<"@somewheom">>,
    User = chef_object:new_record(chef_user, ?API_MIN_VER, OrgId, AuthzId,
                                   {[{<<"username">>, Name},
                                     {<<"password">>, <<"zuperzecret">>},
                                     {<<"email">>, <<Name/binary,Dom/binary>>},
                                     {<<"public_key">>, PubKey},
                                     {<<"display_name">>, <<"someone">>}]}),
    chef_db:create(User, context(), ?USER_AUTHZ_ID).

%% TODO: should this be updated to use the POST endpoint?
add_key(Config, Id, KeyName, ExpirationDate) ->
    PubKey = proplists:get_value(pubkey, Config),
    {ok, 1} = sqerl:execute(<<"INSERT INTO KEYS (id, key_name, public_key, key_version, created_at, expires_at, last_updated_by, updated_at)
                               VALUES ($1, $2, $3, 1, CURRENT_TIMESTAMP, $4, 'me', CURRENT_TIMESTAMP )">>,
                  [Id, KeyName, PubKey, ExpirationDate]).

decoded_response_body({_, _, _, Body}) ->
    chef_json:decode(Body).

context() ->
    chef_db:make_context(?API_MIN_VER, <<"AB">>, no_header).

client_id(Config, Name) ->
    OrgId = proplists:get_value(org_id, Config),
    #chef_client{id = ClientId} = chef_db:fetch(#chef_client{org_id = OrgId, name = Name}, context()),
    ClientId.

user_id(Name) ->
    #chef_user{id = UserId} = chef_db:fetch(#chef_user{username = Name}, context()),
    UserId.

%% The guts of some repetitive tests.
validate_put_full_key(Config, ClientOrUser) ->
    UpdateEJ = new_key_ejson(Config, ?KEY2NAME, ?KEY2EXPIRESTRING, alt_pubkey),
    PutResult = http_named_key_request(put, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME, chef_json:encode(UpdateEJ)),
    ?assertMatch({ok, "201", _, _}, PutResult),
    PutResultEJ = decoded_response_body(PutResult),
    ?assertMatch(UpdateEJ, PutResultEJ),

    GetResult = http_named_key_request(get, ClientOrUser, default_requestor(ClientOrUser), ?KEY2NAME),
    ?assertMatch({ok, "200", _, _}, GetResult),
    ActualGetKeyEJ = decoded_response_body(GetResult),
    ?assertMatch(UpdateEJ, ActualGetKeyEJ).

validate_rename(ClientOrUser, RenameCode, GetOldNameCode) ->
    PutResult = http_named_key_request(put, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME, chef_json:encode({[{<<"name">>, ?KEY2NAME}]})),
    ?assertMatch({ok, RenameCode, _, _}, PutResult),
    GetResult1 = http_named_key_request(get, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME),
    ?assertMatch({ok, GetOldNameCode, _, _}, GetResult1),
    GetResult2 = http_named_key_request(get, ClientOrUser, default_requestor(ClientOrUser), ?KEY2NAME),
    % one way or another, KEY2 will exist
    ?assertMatch({ok, "200", _, _}, GetResult2).

validate_put_partial_key_valid(Config, ClientOrUser, <<"create_key">>, true) ->
    UpdateEJ = {[{<<"create_key">>, true}]},
    PutResult = http_named_key_request(put, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME, chef_json:encode(UpdateEJ)),
    PutResultEJ = decoded_response_body(PutResult),
    ?assertMatch({ok, "200", _, _}, PutResult),
    ?assertMatch(true, has_well_formed_private_key(Config, PutResultEJ)),
    % In this case we get back the field that has been changed (public key) in addition to the
    % new private key.
    ?assertEqual(true, has_well_formed_public_key(Config, PutResultEJ));
validate_put_partial_key_valid(Config, ClientOrUser, Field, Value) ->
    UpdateEJ = {[{Field, Value}]},
    PutResult = http_named_key_request(put, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME, chef_json:encode(UpdateEJ)),
    ?assertMatch({ok, "200", _, _}, PutResult),
    PutResultEJ = decoded_response_body(PutResult),
    ?assertMatch(UpdateEJ, PutResultEJ),
    ExpectedGetKeyEJ = ej:set({Field},  new_key_ejson(Config, ?KEY1NAME, ?KEY2EXPIRESTRING), Value),
    GetResult = http_named_key_request(get, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME),
    ?assertMatch({ok, "200", _, _}, GetResult),
    ActualGetKeyEJ = decoded_response_body(GetResult),
    ?assertMatch(ExpectedGetKeyEJ, ActualGetKeyEJ).

validate_put_partial_key_invalid(ClientOrUser, Field, Value) ->
    UpdateEJ = {[{Field, Value}]},
    PutResult = http_named_key_request(put, ClientOrUser, default_requestor(ClientOrUser), ?KEY1NAME, chef_json:encode(UpdateEJ)),
    ?assertMatch({ok, "400", _, _}, PutResult).

%% Expected Results and Inputs
%%
user_key_list_ejson(Name, KeyInfo) ->
    Base = <<"http://localhost:8000/users/">>,
    Keys = <<"/keys/">>,
    key_list_ejson(<<Base/binary,Name/binary,Keys/binary>>, KeyInfo).
client_key_list_ejson(Name, KeyInfo) ->
    Base = <<"http://localhost:8000/organizations/testorg/clients/">>,
    Keys = <<"/keys/">>,
    key_list_ejson(<<Base/binary,Name/binary,Keys/binary>>, KeyInfo).

key_list_ejson(BaseURI, KeyInfo) ->
    [ {[{<<"uri">>, <<BaseURI/binary,KeyName/binary>>},
        {<<"name">>, KeyName},
        {<<"expired">>, Expired}] } || {KeyName, Expired} <- KeyInfo].


new_key_ejson(Config, Name, Expiration) ->
    new_key_ejson(Config, Name, Expiration, pubkey).
new_key_ejson(_Config, Name, Expiration, create_key) ->
    {[{<<"name">>, Name}, {<<"create_key">>, true}, {<<"expiration_date">>, Expiration}]};
new_key_ejson(Config, Name, Expiration, KeyName) ->
    PubKey = proplists:get_value(KeyName, Config),
    {[{<<"name">>, Name}, {<<"public_key">>, PubKey}, {<<"expiration_date">>, Expiration}]}.

has_well_formed_public_key(Config, EJ) ->
    chef_test_suite_helper:value_matches_regex(ej:get({<<"public_key">>}, EJ), ?config(pubkey_regex, Config)).

has_well_formed_private_key(Config, EJ) ->
    chef_test_suite_helper:value_matches_regex(ej:get({<<"private_key">>}, EJ), ?config(privkey_regex, Config)).

default_requestor(client) ->
    ?CLIENT_NAME;
default_requestor(user) ->
    ?USER_NAME.
