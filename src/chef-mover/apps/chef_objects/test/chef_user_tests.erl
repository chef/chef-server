%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2012-2018 Chef Software, Inc.
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


assemble_user_ejson_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
    [{"obtain expected EJSON",
      fun() ->
              User = make_valid_user_record(),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {"obtain expected EJSON for valid serialized_data",
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              User = make_valid_user_record(chef_json:encode({persisted_serializable_fields()})),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson() ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},
     {"silently ignores unknown field values found in serialized_data",
      fun() ->
              % Rather than fight with escaping, quotes, etc, we'll just make some valid ejson the eas yway
              UserWithUnknownPersistedData = persisted_serializable_fields() ++
                                             [ {<<"badfield1">>, <<"anyvalue">>},
                                               {<<"badfield2">>, <<"anyvalue">>}],

              User = make_valid_user_record(chef_json:encode({UserWithUnknownPersistedData})),
              {GotList} = chef_user:assemble_user_ejson(User, ?OSC_ORG_NAME),
              ExpectedData = base_user_record_as_ejson() ++  persisted_serializable_fields(),
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end}
    ]}.

parse_binary_json_test_() ->
    [{"Can create user when all required fields are present",
      fun() ->
                MinValid = make_min_valid_create_user_ejson(),
                UserJson = chef_json:encode({MinValid}),
                {ok, {GotData}} = chef_user:parse_binary_json(UserJson, create, undefined),
                ?assertEqual(lists:sort(MinValid), lists:sort(GotData))
      end
     },
     {"Can create user when external auth uid is present",
      fun() ->
                MinValid = make_external_auth_create_user_ejson(),
                UserJson = chef_json:encode({MinValid}),
                {ok, {GotData}} = chef_user:parse_binary_json(UserJson, create, undefined),
                ?assertEqual(lists:sort(MinValid), lists:sort(GotData))
      end
     },
     {"Can not create with only name and password",
      fun() ->
              UserEjson = chef_json:encode({[
                                             {<<"name">>, <<"bob">>},
                                             {<<"password">>, <<"top secret 123456">>}
                                            ]}),
              ?assertThrow(#ej_invalid{}, chef_user:parse_binary_json(UserEjson, create, undefined))
      end
     },
     {"Valid email must be provided",
      fun() ->
                    UserEJson = make_min_valid_create_user_ejson(),
                    UserEJson2 = ej:set({<<"email">>}, {UserEJson}, <<"jo my @ bob.com">>),
                    UserJson = chef_json:encode(UserEJson2),
                    ?assertThrow(#ej_invalid{type = fun_match},
                                 chef_user:parse_binary_json(UserJson, create, undefined))
      end},
     {"required fields must be provided", generator,
      fun() ->
              NukeFields = [<<"email">>, <<"password">>, <<"username">>, <<"display_name">>],
              UserEJson = {make_min_valid_create_user_ejson()},

              [ ?_assertThrow(#ej_invalid{type = missing, key = Key},
                             chef_user:parse_binary_json(chef_json:encode(ej:delete({Key}, UserEJson)), create, undefined))
                             || Key  <- NukeFields]
      end},
     {"Properly formed emails are accepted", generator,
      fun() ->
              GoodEmail = [<<"me@here.com">>, <<"me@here.com.com.au.zebra">>,
                           <<"me+you@here.com">>,
                           <<"MyMixedCaseEmail@here.com">>,
                           <<"me.you@here.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertMatch({ok, _},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create, undefined))
                             || E <- GoodEmail]
      end},
     {"Improperly formed emails are rejected", generator,
      fun() ->
              BadEmail = [<<"short">>, <<"shorter@short">>,  <<"short\\andinvalid@somewhre.com">>,
                          <<"spaces are not valid@either.com">>, <<"\"spaces are not valid\"@either.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"email">>},
                              chef_user:parse_binary_json(chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create, undefined))
                             || E <- BadEmail]
      end},
     {"Password must be valid", generator,
      fun() ->
              BadPass = [<<"short">>, 123, [<<"longenoughbutinalist">>], {[]}, true, null],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"password">>},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"password">>}, UserEJson, P)), create, undefined))
               || P<- BadPass]
      end},
     {"Accepts name instead of username",
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:delete({<<"username">>}, UserEJson),
              UserEJson2 = ej:set({<<"name">>}, UserEJson1, "validname"),
              ?_assertMatch({ok, _}, chef_user:parse_binary_json(chef_json:encode(UserEJson2), create, undefined))
      end
     },
     {"Error thrown with bad name",
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              Body = ej:set({<<"username">>}, UserEJson, <<"bad~name">>),
              ?assertThrow(#ej_invalid{key = <<"username">>},
                           chef_user:parse_binary_json(chef_json:encode(Body), update, #chef_user{}))
      end
     },
     {"a null public_key is removed for create",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(chef_json:encode({UserEJson}), create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a null public_key is removed for update",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(chef_json:encode({UserEJson}), update, #chef_user{}),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a valid public_key is preserved for both create and update",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()}],
              Body = chef_json:encode({UserEJson}),
              {ok, Got1} = chef_user:parse_binary_json(Body, create, undefined),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got1)),
              {ok, Got2} = chef_user:parse_binary_json(Body, update, #chef_user{}),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got2))
      end},

     {"Errors thrown for invalid public_key data ", generator,
      fun() ->
              MungedKey = re:replace(public_key_data(), "A", "2",
                                     [{return, binary}, global]),
              BadKeys = [MungedKey, <<"a very bad key">>, true,
                         113, [public_key_data()], {[]}],
              Body = { make_min_valid_create_user_ejson() },
             [ ?_assertThrow(#ej_invalid{key = <<"public_key">>},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"public_key">>}, Body, Bad)), update, #chef_user{}))
                || Bad <- BadKeys ]
      end},
     {"A valid public key is removed when private_key = true",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()},
                                                                 {<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),
              {ok, Got1} = chef_user:parse_binary_json(Body, create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got1))
      end
     },
    {"An invalid public key is removed from the object when private_key = true (key not validated)",
      fun() ->
             UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, <<"a bad key">>},
                                                                {<<"private_key">>, true}],
              Body = chef_json:encode({UserEJson}),

              {ok, Got1} = chef_user:parse_binary_json(Body, create, undefined),
              ?assertEqual(undefined, ej:get({"public_key"}, Got1))
      end
    }
    ].


update_record_test_() ->
    {setup,
     fun() ->
        chef_objects_test_utils:bcrypt_setup(),
        SerializedAsEJson = {base_user_record_as_ejson() ++
                             persisted_serializable_fields() },
        make_valid_user_record(chef_json:encode(SerializedAsEJson))
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
        User1 = chef_user:update_from_ejson(User, UpdateAsEJson),
        User2 = chef_user:update_from_ejson(User, UpdatePassword),
        User3 = chef_user:update_from_ejson(User, ExtAuthUpdate),
        [{"changed values are changed",
           fun() ->
                ?assertEqual(<<"new_email@somewhere.com">>, User1#chef_user.email),
                ?assertEqual(<<"martha">>, User1#chef_user.username)
           end
        },
        {"nulled serialized values are deleted from the serialized object",
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),

                ?assertEqual(ej:get({<<"twitter_account">>}, Data), undefined)
           end
        },
        {"serialized values specified as atom undefined are ignored in the serialized object",
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"city">>}, Data), <<"hereville">>)
           end
        },
        {"updated serialized values are updated within the serialized object",
           fun() ->
                % implicit test here that data has been serialized.
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"display_name">>}, Data), <<"martha stewart pony">>)
           end
        },
        {"garbage values are not retained",
           fun() ->
                Data = chef_json:decode(User1#chef_user.serialized_object),
                ?assertEqual(ej:get({<<"garbage">>}, Data), undefined)
           end
        },
        {"unchanged password has no side effects",
           fun() ->
                ?assertEqual(User#chef_user.hashed_password, User#chef_user.hashed_password),
                ?assertEqual(User#chef_user.salt, User#chef_user.salt)
           end
        },
        {"password updates related fields",
           fun() ->
                ?assertNotEqual(User#chef_user.hashed_password, User2#chef_user.hashed_password),
                ?assertNotEqual(User#chef_user.salt, User2#chef_user.salt)
           end
        },
        {"external authentication data is not when it should not be",
           fun() ->
                ?assertEqual(null, User1#chef_user.external_authentication_uid),
                ?assertEqual(false, User1#chef_user.recovery_authentication_enabled)
           end
        },
        {"external authentication data is updated when it should be",
           fun() ->
                ?assertEqual(<<"bob">>, User3#chef_user.external_authentication_uid),
                ?assertEqual(true, User3#chef_user.recovery_authentication_enabled)
           end
        }]
      end
    }.

new_record_test() ->
    chef_objects_test_utils:bcrypt_setup(),
    UserData = {[
                 {<<"name">>, <<"bob">>},
                 {<<"password">>, <<"top secret 123456">>}
                ]},
    User = chef_user:new_record(?OSC_ORG_ID ,<<"00000000000000000000000011111111">>, UserData),
    ?assertMatch(#chef_user{}, User),
    ?assertEqual(<<"bob">>, chef_user:name(User)),
    ?assertEqual(null, User#chef_user.external_authentication_uid),
    ?assertEqual(false, User#chef_user.recovery_authentication_enabled),
    chef_objects_test_utils:bcrypt_cleanup(ignore).

new_record_no_password_test() ->
    UserData = {[ {<<"name">>, <<"bob">>} ]},
    User = chef_user:new_record(?OSC_ORG_ID ,<<"00000000000000000000000011111111">>, UserData),
    ?assertEqual(null, User#chef_user.hashed_password),
    ?assertEqual(null, User#chef_user.salt),
    ?assertEqual(null, User#chef_user.hash_type).


% Misc exports that were untested
% set_created(#chef_user{}, ActorId}
public_key_data() ->
    <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCyVPW9YXa5PR0rgEW1updSxygB\nwmVpDnHurgQ7/gbh+PmY49EZsfrZSbKgSKy+rxdsVoSoU+krYtHvYIwVfr2tk0FP\nnhAWJaFH654KpuCNG6x6iMLtzGO1Ma/VzHnFqoOeSCKHXDhmHwJAjGDTPAgCJQiI\neau6cDNJRiJ7j0/xBwIDAQAB\n-----END PUBLIC KEY-----">>.

make_valid_user_record() ->
    make_valid_user_record(<<"{}">>).
make_valid_user_record(SerializedObject) ->
    {HashedPassword, Salt ,Type} = chef_password:encrypt("a password"),
    #chef_user{username = <<"alice">>,
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

base_user_record_as_ejson() ->
    [{<<"public_key">>, public_key_data()},
     {<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>}].


persisted_serializable_fields() ->
    [{<<"display_name">>, <<"alice bobson">>},
     {<<"first_name">>, <<"alice">>},
     {<<"last_name">>, <<"bobson">>},
     {<<"middle_name">>, <<"o">>},
     {<<"city">>, <<"hereville">>},
     {<<"country">>, <<"UK">>},
     {<<"twitter_account">>, <<"AliceOBobSon">>}].
