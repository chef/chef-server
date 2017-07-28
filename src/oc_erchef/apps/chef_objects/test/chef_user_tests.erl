%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_user_tests).

-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(VD(D), chef_objects_test_utils:versioned_desc(Version, D)).
-define(VDD(D), chef_objects_test_utils:versioned_desc(Version, iolist_to_binary(["[deprecated] ", D]))).

assemble_user_ejson_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     chef_objects_test_utils:make_all_versions_tests(fun assemble_user_ejson_tests/1) }.

assemble_user_ejson_tests(Version) ->
    [{?VD("obtain expected EJSON"),
      fun() ->
              User = make_valid_user_record(Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {?VD("obtain expected EJSON for valid serialized_data"),
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              User = make_valid_user_record(chef_json:encode({persisted_serializable_fields()}), Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version) ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {?VD("silently ignores unknown field values found in serialized_data"),
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              UserWithUnknownPersistedData = persisted_serializable_fields() ++
                                             [ {<<"badfield1">>, <<"anyvalue">>},
                                               {<<"badfield2">>, <<"anyvalue">>}],

              User = make_valid_user_record(chef_json:encode({UserWithUnknownPersistedData}), Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version) ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end}
    ].

assemble_user_ejson_non_deprecated_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     chef_objects_test_utils:make_non_deprecated_tests(fun assemble_user_ejson_non_deprecated_tests/1) }.

assemble_user_ejson_non_deprecated_tests(Version)->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     [{?VD("obtain expected EJSON"),
       fun() ->
               User = make_valid_user_record(Version),
               {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
               ExpectedData = base_user_record_as_ejson(Version),
               ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
       end}
     ]}.

assemble_user_ejson_deprecated_test_() ->
    chef_objects_test_utils:make_deprecated_tests(fun assemble_user_ejson_deprecated_tests/1).
assemble_user_ejson_deprecated_tests(Version) ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
    [{?VDD("obtain expected EJSON"),
      fun() ->
              User = make_valid_user_record(Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {?VDD("obtain expected EJSON for valid serialized_data"),
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              User = make_valid_user_record(chef_json:encode({persisted_serializable_fields()}), Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version) ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {?VDD("silently ignores unknown field values found in serialized_data"),
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              UserWithUnknownPersistedData = persisted_serializable_fields() ++
                                             [ {<<"badfield1">>, <<"anyvalue">>},
                                               {<<"badfield2">>, <<"anyvalue">>}],

              User = make_valid_user_record(chef_json:encode({UserWithUnknownPersistedData}), Version),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(Version) ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end}
    ]}.

parse_binary_json_deprecated_test_() ->
    chef_objects_test_utils:make_deprecated_tests(fun parse_binary_json_deprecated_tests/1).

parse_binary_json_deprecated_tests(Version) ->
    [{?VDD("a null public_key is removed for create"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(Version, chef_json:encode({UserEJson}), create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},
     {?VDD("a null public_key is removed for update"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(Version, chef_json:encode({UserEJson}), update, #chef_user{}),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {?VDD("a valid public_key is preserved for both create and update"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()}],
              Body = chef_json:encode({UserEJson}),
              {ok, Got1} = chef_user:parse_binary_json(Version, Body, create, undefined),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got1)),
              {ok, Got2} = chef_user:parse_binary_json(Version, Body, update, #chef_user{}),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got2))
      end},

     {?VDD("A valid public key is removed when private_key = true"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()},
                                                                 {<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),
              {ok, Got1} = chef_user:parse_binary_json(Version, Body, create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got1))
      end
     },
     {?VDD("An invalid public key is removed from the object when private_key = true (key not validated)"),
      fun() ->
             UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, <<"a bad key">>},
                                                                {<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),

              {ok, Got1} = chef_user:parse_binary_json(Version, Body, create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got1))
      end
    }
    ].

parse_binary_json_test_() ->
    chef_objects_test_utils:make_all_versions_tests(fun parse_binary_json_tests/1).

parse_binary_json_tests(Version) ->
    NonfunctionalFields = [<<"display_name">>, <<"first_name">>, <<"middle_name">>, <<"last_name">>],
    [{?VD("Can create user when all required fields are present"),
      fun() ->
                MinValid = make_min_valid_create_user_ejson(),
                UserJson = chef_json:encode({MinValid}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(MinValid), lists:sort(GotData))
      end
     },
     {?VD("Can create user with empty first name"),
      fun() ->
                UserEJson = make_min_valid_create_user_ejson() ++ [{<<"first_name">>, <<>>},
                                                                   {<<"last_name">>, <<"Bobson">>},
                                                                   {<<"middle_name">>, <<"Joe">>}],
                UserJson = chef_json:encode({UserEJson}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(UserEJson), lists:sort(GotData))
      end
     },
     {?VD("Can create user with empty last name"),
      fun() ->
                UserEJson = make_min_valid_create_user_ejson() ++ [{<<"first_name">>, <<"Alice">>},
                                                                   {<<"last_name">>, <<>>},
                                                                   {<<"middle_name">>, <<"Joe">>}],
                UserJson = chef_json:encode({UserEJson}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(UserEJson), lists:sort(GotData))
      end
     },
     {?VD("Can create user with empty middle name"),
      fun() ->
                UserEJson = make_min_valid_create_user_ejson() ++ [{<<"first_name">>, <<"Alice">>},
                                                                   {<<"last_name">>, <<"Bobson">>},
                                                                   {<<"middle_name">>, <<>>}],
                UserJson = chef_json:encode({UserEJson}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(UserEJson), lists:sort(GotData))
      end
     },
     {?VD("Can create user with all required fields and first, middle, and last names"),
      fun() ->
                UserEJson = make_min_valid_create_user_ejson() ++ [{<<"first_name">>, <<"Alice">>},
                                                                   {<<"last_name">>, <<"Bobson">>},
                                                                   {<<"middle_name">>, <<"Joe">>}],
                UserJson = chef_json:encode({UserEJson}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(UserEJson), lists:sort(GotData))
      end
     },
     {?VD("Can create user when external auth uid is present"),
      fun() ->
                MinValid = make_external_auth_create_user_ejson(),
                UserJson = chef_json:encode({MinValid}),
                {ok, {GotData}} = chef_user:parse_binary_json(Version, UserJson, create, undefined),
                ?assertEqual(lists:sort(MinValid), lists:sort(GotData))
      end
     },
     {?VD("Can not create with only name and password"),
      fun() ->
              UserEjson = chef_json:encode({[
                                             {<<"name">>, <<"bob">>},
                                             {<<"password">>, <<"top secret 123456">>}
                                            ]}),
              ?assertThrow(#ej_invalid{}, chef_user:parse_binary_json(Version, UserEjson, create, undefined))
      end
     },
     {?VD("Valid email must be provided"),
      fun() ->
                    UserEJson = make_min_valid_create_user_ejson(),
                    UserEJson2 = ej:set({<<"email">>}, {UserEJson}, <<"jo my @ bob.com">>),
                    UserJson = chef_json:encode(UserEJson2),
                    ?assertThrow(#ej_invalid{type = fun_match},
                                 chef_user:parse_binary_json(Version, UserJson, create, undefined))
      end},
     {?VD("required fields must be provided"), generator,
      fun() ->
              NukeFields = [<<"email">>, <<"password">>, <<"username">>, <<"display_name">>],
              UserEJson = {make_min_valid_create_user_ejson()},

              [ ?_assertThrow(#ej_invalid{type = missing, key = Key},
                             chef_user:parse_binary_json(Version, chef_json:encode(ej:delete({Key}, UserEJson)), create, undefined))
                             || Key  <- NukeFields]
      end},
     {?VD("Properly formed emails are accepted"), generator,
      fun() ->
              GoodEmail = [<<"me@here.com">>, <<"me@here.com.com.au.zebra">>,
                           <<"me+you@here.com">>,
                           <<"MyMixedCaseEmail@here.com">>,
                           <<"me.you@here.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertMatch({ok, _},
                             chef_user:parse_binary_json(Version, chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create, undefined))
                             || E <- GoodEmail]
      end},
     {?VD("Improperly formed emails are rejected"), generator,
      fun() ->
              BadEmail = [<<"short">>, <<"shorter@short">>,  <<"short\\andinvalid@somewhre.com">>,
                          <<"spaces are not valid@either.com">>, <<"\"spaces are not valid\"@either.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"email">>},
                              chef_user:parse_binary_json(Version, chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create, undefined))
                             || E <- BadEmail]
      end},
     {?VD("Password must be valid"), generator,
      fun() ->
              BadPass = [<<"short">>, 123, [<<"longenoughbutinalist">>], {[]}, true, null],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"password">>},
                             chef_user:parse_binary_json(Version, chef_json:encode(ej:set({<<"password">>}, UserEJson, P)), create, undefined))
               || P<- BadPass]
      end},
     {?VD("Accepts name instead of username"),
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:delete({<<"username">>}, UserEJson),
              UserEJson2 = ej:set({<<"name">>}, UserEJson1, "validname"),
              ?_assertMatch({ok, _}, chef_user:parse_binary_json(Version, chef_json:encode(UserEJson2), create, undefined))
      end
     },
     {?VD("Ignores invalid username if name exists"),
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:set({<<"name">>}, UserEJson, <<"validname">>),
              UserEJson2 = ej:set({<<"username">>}, UserEJson1, <<"invalid!">>),
              Actual = chef_user:parse_binary_json(Version, chef_json:encode(UserEJson2), create, undefined),
              ?_assertMatch({ok, _}, Actual)
      end
     },
     {?VD("Errors on invalid name even if valid username exists"),
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:set({<<"username">>}, UserEJson, <<"validname">>),
              UserEJson2 = ej:set({<<"name">>}, UserEJson1, <<"invalid!">>),
              ?assertThrow(#ej_invalid{key= <<"name">>},
                           chef_user:parse_binary_json(Version, chef_json:encode(UserEJson2), create, undefined))
      end
     },
     {?VD("Error thrown with bad name"),
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              Body = ej:set({<<"username">>}, UserEJson, <<"bad~name">>),
              ?assertThrow(#ej_invalid{key = <<"username">>},
                           chef_user:parse_binary_json(Version, chef_json:encode(Body), update, #chef_user{}))
      end
     },
     {?VD("Errors thrown for invalid public_key data on create "), generator,
      fun() ->
              MungedKey = re:replace(public_key_data(), "A", "2",
                                     [{return, binary}, global]),
              BadKeys = [MungedKey, <<"a very bad key">>, true,
                         113, [public_key_data()], {[]}],
              Body = { make_min_valid_create_user_ejson() },
             [ ?_assertThrow(#ej_invalid{key = <<"public_key">>},
                             chef_user:parse_binary_json(Version, chef_json:encode(ej:set({<<"public_key">>}, Body, Bad)),
                                                         create, undefined))
                || Bad <- BadKeys ]
      end}
    ]
    ++
    [
     {?VD(lists:flatten(io_lib:format("Works with non-ASCII ~s", [Field]))),
      fun() ->
              %% "Maryam", #1 female name in the arab world as of 2015
              Value = <<"مريم 1. O'Mara"/utf8>>,
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:set({Field}, UserEJson, Value),
              {ok, User} = chef_user:parse_binary_json(Version, chef_json:encode(UserEJson1), create, undefined),
              ?assertEqual(ej:get({Field}, User), Value)
      end
     }
     || Field <- NonfunctionalFields
    ].

parse_binary_json_non_deprecated_test_() ->
    chef_objects_test_utils:make_non_deprecated_tests(fun parse_binary_json_non_deprecated_tests /1).

parse_binary_json_non_deprecated_tests(Version) ->
    [{?VD("Public key is rejected as a field for update, even when valid"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()}],
              Body = chef_json:encode({UserEJson}),
              ?assertThrow(key_management_not_supported,
                           chef_user:parse_binary_json(Version, Body, update, #chef_user{}))
      end},
     {?VD("create_key is rejected as a field for update"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"create_key">>, true}],
              Body = chef_json:encode({UserEJson}),
              ?assertThrow(key_management_not_supported,
                           chef_user:parse_binary_json(Version, Body, update, #chef_user{}))
      end},
     {?VD("private_key is rejected as a field for create"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),
              ?assertThrow(private_key_field_not_supported,
                           chef_user:parse_binary_json(Version, Body, create, undefined))
      end},
     {?VD("private_key is rejected as a field for update"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),
              ?assertThrow(private_key_field_not_supported,
                           chef_user:parse_binary_json(Version, Body, update, #chef_user{}))
      end},
    {?VD("When both create_key and public_key are specified on create, an error is thrown"),
      fun() ->
             UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()},
                                                                {<<"create_key">>, true}],
              Body = chef_json:encode({UserEJson}),

              ?assertThrow(create_and_pubkey_specified,
                           chef_user:parse_binary_json(Version, Body, create, undefined))

      end},
    {?VD("When neither create_key and public_key are specified on create, it is permitted"),
      fun() ->
             UserEJson = make_min_valid_create_user_ejson(),
              Body = chef_json:encode({UserEJson}),
              ?assertMatch({ok, _}, chef_user:parse_binary_json(Version, Body, create, undefined))

      end},
    {?VD("When create_key = false and public_key are specified on create, it is permitted"),
      fun() ->
             UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()},
                                                                {<<"create_key">>, false}],
              Body = chef_json:encode({UserEJson}),
              ?assertMatch({ok, _}, chef_user:parse_binary_json(Version, Body, create, undefined))

      end},
     {?VD("a null public_key is rejected for create"),
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              ?assertThrow(#ej_invalid{key = <<"public_key">>},
                             chef_user:parse_binary_json(Version, chef_json:encode({UserEJson}), create, undefined))
      end}
    ].

update_record_test_() ->
    chef_objects_test_utils:make_all_versions_tests(fun update_record_tests/1).

update_record_tests(Version) ->
    {setup,
     fun() ->
        chef_objects_test_utils:bcrypt_setup(),
        SerializedAsEJson = {base_user_record_as_ejson(Version) ++
                             persisted_serializable_fields() },
        make_valid_user_record(chef_json:encode(SerializedAsEJson), Version)
     end,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     fun(User) ->
        UpdateAsEJson = {[ {<<"username">>, <<"martha">>},
                        {<<"email">>, <<"new_email@somewhere.com">>},
                        {<<"garbage">>, <<"be gone">>},
                        {<<"display_name">>, <<"martha stewart pony">>},
                        {<<"public_key">>, public_key_data()},
                        {<<"city">>, undefined},
                        {<<"twitter_account">>, null }]},
        UpdatePassword = {[ {<<"password">>, <<"a new password">>} ]},
        ExtAuthUpdate = [{<<"external_authentication_uid">>, <<"bob">>},
                          {<<"recovery_authentication_enabled">>, true}],
        SerializedObject = chef_json:encode({base_user_record_as_ejson(Version)
                                             ++ persisted_serializable_fields()}),
        UserWithExtAuthData = make_valid_user_record_with_external_auth(SerializedObject, Version),
        User1 = chef_user:update_from_ejson(User, UpdateAsEJson),
        User2 = chef_user:update_from_ejson(User, UpdatePassword),
        User3 = chef_user:update_from_ejson(User, ExtAuthUpdate),
        User4 = chef_user:update_from_ejson(UserWithExtAuthData, UpdateAsEJson),
        [
        {?VD("changed values are changed"),
           fun() ->
                ?assertEqual(<<"new_email@somewhere.com">>, User1#chef_user.email),
                ?assertEqual(<<"martha">>, User1#chef_user.username)
           end
        },
        {?VD("nulled serialized values are deleted from the serialized object"),
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),

                ?assertEqual(ej:get({<<"twitter_account">>}, Data), undefined)
           end
        },
        {?VD("serialized values specified as atom undefined are ignored in the serialized object"),
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"city">>}, Data), <<"hereville">>)
           end
        },
        {?VD("updated serialized values are updated within the serialized object"),
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"display_name">>}, Data), <<"martha stewart pony">>)
           end
        },
        {?VD("garbage values are not retained"),
           fun() ->
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"garbage">>}, Data), undefined)
           end
        },
        {?VD("unchanged password has no side effects"),
           fun() ->
                ?assertEqual(User#chef_user.hashed_password, User#chef_user.hashed_password),
                ?assertEqual(User#chef_user.salt, User#chef_user.salt)
           end
        },
        {?VD("password updates related fields"),
           fun() ->
                ?assertNotEqual(User#chef_user.hashed_password, User2#chef_user.hashed_password),
                ?assertNotEqual(User#chef_user.salt, User2#chef_user.salt)
           end
        },
        {?VD("external authentication data is not when it should not be"),
           fun() ->
                ?assertEqual(null, User1#chef_user.external_authentication_uid),
                ?assertEqual(false, User1#chef_user.recovery_authentication_enabled)
           end
        },
        {?VD("external authentication data is not updated when it should not be"),
           fun() ->
                ?assertEqual(<<"amoney">>, User4#chef_user.external_authentication_uid),
                ?assertEqual(true, User4#chef_user.recovery_authentication_enabled)
           end
        },
        {?VD("external authentication data is updated when it should be"),
          fun() ->
               ?assertEqual(<<"bob">>, User3#chef_user.external_authentication_uid),
               ?assertEqual(true, User3#chef_user.recovery_authentication_enabled)
          end}
         ]
      end
    }.

new_record_test() ->
    chef_objects_test_utils:bcrypt_setup(),
    UserData = {[
                 {<<"name">>, <<"bob">>},
                 {<<"password">>, <<"top secret 123456">>}
                ]},
    User = chef_user:new_record(?API_MIN_VER, ?OSC_ORG_ID ,<<"00000000000000000000000011111111">>, UserData),
    ?assertMatch(#chef_user{}, User),
    ?assertEqual(<<"bob">>, chef_user:name(User)),
    ?assertEqual(null, User#chef_user.external_authentication_uid),
    ?assertEqual(false, User#chef_user.recovery_authentication_enabled),
    chef_objects_test_utils:bcrypt_cleanup(ignore).

new_record_no_password_test() ->
    UserData = {[ {<<"name">>, <<"bob">>} ]},
    User = chef_user:new_record(?API_MIN_VER, ?OSC_ORG_ID ,<<"00000000000000000000000011111111">>, UserData),
    ?assertEqual(null, User#chef_user.hashed_password),
    ?assertEqual(null, User#chef_user.salt),
    ?assertEqual(null, User#chef_user.hash_type).


% Misc exports that were untested
% set_created(#chef_user{}, ActorId}
public_key_data() ->
    <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCyVPW9YXa5PR0rgEW1updSxygB\nwmVpDnHurgQ7/gbh+PmY49EZsfrZSbKgSKy+rxdsVoSoU+krYtHvYIwVfr2tk0FP\nnhAWJaFH654KpuCNG6x6iMLtzGO1Ma/VzHnFqoOeSCKHXDhmHwJAjGDTPAgCJQiI\neau6cDNJRiJ7j0/xBwIDAQAB\n-----END PUBLIC KEY-----">>.

make_valid_user_record(Version) ->
    make_valid_user_record(<<"{}">>, Version).
make_valid_user_record(SerializedObject, Version) ->
    {HashedPassword, Salt ,Type} = chef_password:encrypt("a password"),
    #chef_user{server_api_version = Version,
               username = <<"alice">>,
               email = <<"test@test.com">>,
               authz_id = <<"1234">>,
               public_key = public_key_data(),
               hashed_password = HashedPassword,
               salt = Salt,
               hash_type = Type,
               serialized_object = SerializedObject}.
make_valid_user_record_with_external_auth(SerializedObject, Version) ->
    {HashedPassword, Salt ,Type} = chef_password:encrypt("a password"),
    #chef_user{server_api_version = Version,
               username = <<"alice">>,
               external_authentication_uid = <<"amoney">>,
               recovery_authentication_enabled = true,
               email = <<"test@test.com">>,
               authz_id = <<"1234">>,
               public_key = public_key_data(),
               hashed_password = HashedPassword,
               salt = Salt,
               hash_type = Type,
               serialized_object = SerializedObject}.

make_min_valid_create_user_ejson() ->
    [{<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>},
     {<<"password">>, <<"some good pass">>},
     {<<"display_name">>, <<"alice bobson">>}].

make_external_auth_create_user_ejson() ->
    [{<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>},
     {<<"password">>, <<"some good pass">>},
     {<<"external_authentication_uid">>, <<"alice">>},
     {<<"display_name">>, <<"alice bobson">>}].

base_user_record_as_ejson(?API_v0) ->
    [{<<"public_key">>, public_key_data()},
     {<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>}];
base_user_record_as_ejson(_Version) ->
    [{<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>}].


persisted_serializable_fields() ->
    [{<<"display_name">>, <<"alice bobson">>},
     {<<"first_name">>, <<"alice">>},
     {<<"last_name">>, <<"bobson">>},
     {<<"middle_name">>, <<"o">>},
     {<<"city">>, <<"hereville">>},
     {<<"country">>, <<"UK">>},
     {<<"twitter_account">>, <<"AliceOBobSon">>}].
