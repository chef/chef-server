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

-include("../include/chef_types.hrl").
-include("../include/chef_osc_defaults.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("eunit/include/eunit.hrl").


assemble_user_ejson_test_() ->
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
    ].

parse_binary_json_test_() ->
    [{"Can create user when all required fields are present",
      fun() ->
                MinValid = make_min_valid_create_user_ejson(),
                UserJson = chef_json:encode({MinValid}),
                {ok, {GotData}} = chef_user:parse_binary_json(UserJson, create),
                ?assertEqual(lists:sort(MinValid), lists:sort(GotData))
      end
     },
     {"Can not create with only name and password",
      fun() ->
              UserEjson = chef_json:encode({[
                                             {<<"name">>, <<"bob">>},
                                             {<<"password">>, <<"top secret 123456">>}
                                            ]}),
              ?assertThrow(#ej_invalid{}, chef_user:parse_binary_json(UserEjson, create))
      end
     },
     {"Valid email must be provided",
      fun() ->
                    UserEJson = make_min_valid_create_user_ejson(),
                    UserEJson2 = ej:set({<<"email">>}, {UserEJson}, <<"jo my @ bob.com">>),
                    UserJson = chef_json:encode(UserEJson2),
                    ?assertThrow(#ej_invalid{type = fun_match},
                                 chef_user:parse_binary_json(UserJson, create))
      end},
     {"required fields must be provided", generator,
      fun() ->
              NukeFields = [<<"email">>, <<"password">>, <<"username">>, <<"display_name">>],
              UserEJson = {make_min_valid_create_user_ejson()},

              [ ?_assertThrow(#ej_invalid{type = missing, key = Key},
                             chef_user:parse_binary_json(chef_json:encode(ej:delete({Key}, UserEJson)), create))
                             || Key  <- NukeFields]
      end},
     {"Properly formed emails are accepted", generator,
      fun() ->
              GoodEmail = [<<"me@here.com">>, <<"me@here.com.com.au.zebra">>,
                           <<"me+you@here.com">>,
                           <<"me.you@here.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertMatch({ok, _},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create))
                             || E <- GoodEmail]
      end},
     {"Improperly formed emails are rejected", generator,
      fun() ->
              BadEmail = [<<"short">>, <<"shorter@short">>,  <<"short\\andinvalid@somewhre.com">>,
                          <<"spaces are not valid@either.com">>, <<"\"spaces are not valid\"@either.com">>],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"email">>},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"email">>}, UserEJson, E)), create))
                             || E <- BadEmail]
      end},
     {"Password must be valid", generator,
      fun() ->
              BadPass = [<<"short">>, 123, [<<"longenoughbutinalist">>], {[]}, true, null],
              UserEJson = {make_min_valid_create_user_ejson()},
                    [ ?_assertThrow(#ej_invalid{key = <<"password">>},
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"password">>}, UserEJson, P)), create))
               || P<- BadPass]
      end},
     {"Accepts name instead of username",
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              UserEJson1 = ej:delete({<<"username">>}, UserEJson),
              UserEJson2 = ej:set({<<"name">>}, UserEJson1, "validname"),
              ?_assertMatch({ok, _}, chef_user:parse_binary_json(chef_json:encode(UserEJson2), create))
      end
     },
     {"Error thrown with bad name",
      fun() ->
              UserEJson = {make_min_valid_create_user_ejson()},
              Body = ej:set({<<"username">>}, UserEJson, <<"bad~name">>),
              ?assertThrow(#ej_invalid{key = <<"username">>},
                           chef_user:parse_binary_json(chef_json:encode(Body), update))
      end
     },
     {"a null public_key is removed for create",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(chef_json:encode({UserEJson}), create),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a null public_key is removed for update",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, null}],
              {ok, Got} = chef_user:parse_binary_json(chef_json:encode({UserEJson}), update),
              ?assertEqual(undefined, ej:get({"public_key"}, Got))
      end},

     {"a valid public_key is preserved for both create and update",
      fun() ->
              UserEJson = make_min_valid_create_user_ejson() ++ [{<<"public_key">>, public_key_data()}],
              Body = chef_json:encode({UserEJson}),
              {ok, Got1} = chef_user:parse_binary_json(Body, create),
              ?assertEqual(public_key_data(), ej:get({"public_key"}, Got1)),
              {ok, Got2} = chef_user:parse_binary_json(Body, update),
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
                             chef_user:parse_binary_json(chef_json:encode(ej:set({<<"public_key">>}, Body, Bad)), update))
                || Bad <- BadKeys ]
      end}
    ].


update_record_test() ->
    SerializedAsEJson = {base_user_record_as_ejson() ++ persisted_serializable_fields()},
    User = make_valid_user_record(chef_json:encode(SerializedAsEJson)),
     UpdateAsEJson = {[ {<<"username">>, <<"martha">>},
                        {<<"email">>, <<"new_email">>},
                        {<<"display_name">>, <<"martha stewart pony">>},
                        {<<"some_extra_field">>, <<"irrelevant">>},
                        {<<"public_key">>, public_key_data()} ]},
    NewUser = chef_user:update_from_ejson(User,  UpdateAsEJson),
    ?assertMatch(#chef_user{}, NewUser),
    ?assertEqual(<<"martha">>, chef_user:name(NewUser)).


new_record_test() ->
    UserData = {[
                 {<<"name">>, <<"bob">>},
                 {<<"password">>, <<"top secret 123456">>}
                ]},
    OrgId = ?OSC_ORG_ID,
    AuthzId = <<"00000000000000000000000011111111">>,
    Hashed = <<"blahblah">>,
    Salt = <<"nacl">>,
    HashType = <<"bogus">>,
    Password = {Hashed, Salt, HashType},
    User = chef_user:new_record(OrgId, AuthzId, {UserData, Password}),
    ?assertMatch(#chef_user{}, User),
    ?assertEqual(<<"bob">>, chef_user:name(User)).

public_key_data() ->
    <<"-----BEGIN PUBLIC KEY-----\nMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCyVPW9YXa5PR0rgEW1updSxygB\nwmVpDnHurgQ7/gbh+PmY49EZsfrZSbKgSKy+rxdsVoSoU+krYtHvYIwVfr2tk0FP\nnhAWJaFH654KpuCNG6x6iMLtzGO1Ma/VzHnFqoOeSCKHXDhmHwJAjGDTPAgCJQiI\neau6cDNJRiJ7j0/xBwIDAQAB\n-----END PUBLIC KEY-----">>.

make_valid_user_record() ->
    make_valid_user_record(<<"{}">>).
make_valid_user_record(SerializedObject) ->
    #chef_user{username = <<"alice">>,
               email = <<"test@test.com">>,
               authz_id = <<"1234">>,
               public_key = public_key_data(),
               serialized_object = SerializedObject}.

make_min_valid_create_user_ejson() ->
    [{<<"username">>, <<"alice">>},
     {<<"email">>, <<"test@test.com">>},
     {<<"password">>, <<"some good pass">>},
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
