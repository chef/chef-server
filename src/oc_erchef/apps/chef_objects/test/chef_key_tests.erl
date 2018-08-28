%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% Copyright 2015-2018 Chef Software, Inc. All Rights Reserved.
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

-module(chef_key_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

-define(KEY_NAME, <<"test_key">>).
-define(DEFAULT_EXPIRATION, <<"2099-10-24T22:49:08Z">>).
%% The extra zero denotes UTC
-define(PARSED_DEFAULT_EXPIRATION, {{2099,10,24},{22,49,08}}).

%% example post parse_binary_json
example_key() ->
    example_key(?DEFAULT_EXPIRATION, undefined).

example_key(Expiration, undefined) ->
    example_key(Expiration, load_public_key(primary));
example_key(Expiration, PublicKey) ->
    example_key(?KEY_NAME, Expiration, PublicKey).

example_key(Name, Expiration, PublicKey) ->
    {[{<<"name">>, Name}, {<<"public_key">>, PublicKey}, {<<"expiration_date">>, Expiration}]}.


load_public_key(primary) ->
    {ok, PublicKey} = chef_objects_test_utils:read_file("spki_public.pem"),
    PublicKey;
load_public_key(alternate) ->
    {ok, PublicKey} = chef_objects_test_utils:read_file("public.pem"),
    PublicKey.

update_from_ejson_test_() ->
    OriginalKey = chef_key:new_record(?API_MIN_VER, unused, unused, {<<"A">>, example_key()}),
    [{"check that update_from_ejson returns the updated chef_key when fields are updated",
      fun() ->
              NewKey = load_public_key(alternate),
              UpdateEJ = example_key(<<"NewKeyName">>, <<"infinity">>, NewKey),
              ExpectedKey = OriginalKey#chef_key{key_name = <<"NewKeyName">>, expires_at = ?INFINITY_TIMESTAMP,
                                                 key_version = 0,
                                                 public_key = NewKey,
                                                old_name = ?KEY_NAME},
              ?assertMatch(ExpectedKey, chef_key:update_from_ejson(OriginalKey, UpdateEJ))
      end
     },
     {"check that date is not updated, it is preserved",
      fun() ->
              UpdateEJ = {[{<<"name">>, <<"new_name">>}]},
              ExpectedKey = OriginalKey#chef_key{key_name = <<"new_name">>, expires_at = ?PARSED_DEFAULT_EXPIRATION,
                                                 old_name = ?KEY_NAME},
              ?assertMatch(ExpectedKey, chef_key:update_from_ejson(OriginalKey, UpdateEJ))
      end
     }].


ejson_from_key_test_() ->
    [
     {"check that ejson_from_key replies in correct form with infinity expiration",
      fun() ->
          ExampleKey = example_key(<<"infinity">>, undefined),
          ChefKey = chef_key:new_record(?API_MIN_VER, unused, unused, {<<"A">>, ExampleKey}),
          ?assertMatch(ExampleKey, chef_key:ejson_from_key(ChefKey))
      end
     },
     {"check that ejson_from_key replies in correct form with normal expiration",
      fun() ->
          ExampleKey = example_key(),
          ChefKey = chef_key:new_record(?API_MIN_VER, unused, unused, {<<"A">>, ExampleKey }),
          ?assertMatch(ExampleKey, chef_key:ejson_from_key(ChefKey))
      end
     }
    ].

% Note: removed fields_for_update_test_ - if fields_for_update is wrong, then the
%       update query will fail.
%
new_record_test_() ->
    [{"check that a new record is generated from valid KeyData with properly parsed date",
      fun() ->
	      KeyData = example_key(),
	      ID = <<"testid">>,
          NewRecord = chef_key:new_record(?API_MIN_VER, unused, unused, {ID, KeyData}),
	      ?assertEqual(?PARSED_DEFAULT_EXPIRATION, NewRecord#chef_key.expires_at)
      end
     },
     {"check that a new record is generated from valid KeyData with properly parsed infinity date",
      fun() ->
	      KeyData = example_key(<<"infinity">>, undefined),
	      ID = <<"testid">>,
	      ?assertEqual(#chef_key{server_api_version = ?API_MIN_VER, id = ID, key_name = ?KEY_NAME,
                                 public_key = ej:get({<<"public_key">>}, KeyData),
                                 key_version = 0, expires_at = ?INFINITY_TIMESTAMP},
			   chef_key:new_record(?API_MIN_VER, unused, unused, {ID, KeyData}))
      end
     },
     {"check that KeyData with an invalid date throws bad_date",
      fun() ->
	      KeyData = example_key(<<"not-a-date">>, undefined),
	      ID = <<"testid">>,
	      ?assertThrow({ec_date,{bad_date,_}}, chef_key:new_record(?API_MIN_VER, unused, unused, {ID, KeyData}))
      end
     },
     {"check that KeyData with an invalid public_key throws invalid_public_key",
      fun() ->
	      KeyData = example_key(<<"infinity">>, <<"invalid_key">>),
	      ID = <<"testid">>,
	      ?assertThrow(invalid_public_key,chef_key:new_record(?API_MIN_VER, unused, unused, {ID, KeyData}))
      end
     }
    ].

parse_binary_json_update_test_() ->
    [{"check that a valid key is accepted",
      fun() ->
	      Key = example_key(),
	      EncodedKey = chef_json:encode(Key),
	      ?assertEqual(Key, chef_key:parse_binary_json(EncodedKey, update))
      end},
     {"check that a valid key with an infinity expiration is accepted",
      fun() ->
	      Key = example_key(<<"infinity">>, undefined),
	      EncodedKey = chef_json:encode(Key),
	      ?assertEqual(example_key(<<"infinity">>, undefined), chef_key:parse_binary_json(EncodedKey, update))
      end},
    %{[{<<"name">>, ?KEY_NAME}, {<<"public_key">>, PublicKey}, {<<"expiration_date">>, Expiration}]}.
     {"check that partial update of eaceh field is accepted",
      fun() ->
          KeyField = {[{<<"public_key">>, load_public_key(alternate)}]},
          NameField = {[{<<"name">>, <<"bob">>}]},
          DateField = {[{<<"expiration_date">>, <<"infinity">>}]},

          ?assertEqual(KeyField, chef_key:parse_binary_json(chef_json:encode(KeyField), update)),
          ?assertEqual(NameField, chef_key:parse_binary_json(chef_json:encode(NameField), update)),
          ?assertEqual(DateField, chef_key:parse_binary_json(chef_json:encode(DateField), update))
      end
     },
     {"check that partial update of each field with an invalid value is rejected",
      fun() ->
          BadKeyField = {[{<<"public_key">>, <<"bad key">>}]},
          BadNameField = {[{<<"name">>, <<"bob^was^here">>}]},
          BadDateField = {[{<<"expiration_date">>, <<"tomorrow">>}]},

          ?assertThrow({ej_invalid,fun_match,_,_,_,_,_}, chef_key:parse_binary_json(chef_json:encode(BadKeyField), update)),
          ?assertThrow({ej_invalid, string_match,_,_,_,_,_}, chef_key:parse_binary_json(chef_json:encode(BadNameField), update)),
          ?assertThrow({bad_date, <<"expiration_date">>}, chef_key:parse_binary_json(chef_json:encode(BadDateField), update))
      end
     },
     {"check that partial update with no known fields is rejected",
      fun() ->
          BogusField = {[{<<"bogus">>, <<"ok">>}]},
          ?assertThrow(missing_required_field, chef_key:parse_binary_json(chef_json:encode(BogusField), update))
      end
     },
     {"check that an update with an invalid name is rejected",
      fun() ->
	      OriginalKey = example_key(<<"infinity">>, undefined),
	      Key = ej:set({<<"name">>}, OriginalKey, <<"invalid^name">>),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({ej_invalid,string_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, update))
      end},
     {"check that complete update key with an invalid date is rejected",
      fun() ->
	      Key = example_key(<<"not-a-valid-date">>, undefined),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({bad_date, <<"expiration_date">>}, chef_key:parse_binary_json(EncodedKey, update))
      end},
     {"check that complete update key with an invalid public_key is rejected",
      fun() ->
	      Key = example_key(<<"infinity">>, <<"-----BEGIN PUBLIC KEY-----\ninvalid_key\n-----END PUBLIC KEY-----">>),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({ej_invalid,fun_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, update))
      end}
    ].

parse_binary_json_create_test_() ->
    [{"check that a valid key is accepted",
      fun() ->
	      Key = example_key(),
	      EncodedKey = chef_json:encode(Key),
	      ?assertEqual(Key, chef_key:parse_binary_json(EncodedKey, create))
      end},
     {"check that a valid key with an infinity expiration is accepted",
      fun() ->
	      Key = example_key(<<"infinity">>, undefined),
	      EncodedKey = chef_json:encode(Key),
	      ?assertEqual(example_key(<<"infinity">>, undefined), chef_key:parse_binary_json(EncodedKey, create))
      end},
     {"check that key with an invalid name is rejected",
      fun() ->
	      OriginalKey = example_key(<<"infinity">>, undefined),
	      Key = ej:set({<<"name">>}, OriginalKey, <<"invalid^name">>),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({ej_invalid,string_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, create))
      end},
     {"check that key with an invalid date is rejected",
      fun() ->
	      Key = example_key(<<"not-a-valid-date">>, undefined),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({bad_date, <<"expiration_date">>}, chef_key:parse_binary_json(EncodedKey, create))
      end},
     {"check that key with an invalid public_key is rejected",
      fun() ->
	      Key = example_key(<<"infinity">>, <<"-----BEGIN PUBLIC KEY-----\ninvalid_key\n-----END PUBLIC KEY-----">>),
	      EncodedKey = chef_json:encode(Key),
	      ?assertThrow({ej_invalid,fun_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, create))
      end}
    ].

chef_object_callbacks_test_() ->
    ChefKey = chef_key:new_record(?API_MIN_VER, unused, unused, {<<"A">>, example_key()}),
    OkFun = fun(_) -> ok end,
    [
        ?_assertEqual(true, is_list(chef_key:fields_for_insert(ChefKey))),
        ?_assertEqual(true, is_list(chef_key:fields_for_update(ChefKey))),
        ?_assertEqual(ChefKey#chef_key.key_name, chef_key:name(ChefKey)),
        ?_assertEqual(ChefKey#chef_key.id, chef_key:id(ChefKey)),
        ?_assertEqual(undefined, chef_key:org_id(ChefKey)),
        ?_assertEqual(undefined, chef_key:authz_id(ChefKey)),
        ?_assertEqual(key, chef_key:type_name(ChefKey)),
        ?_assertError(not_indexed, chef_key:ejson_for_indexing(ChefKey, ignored)),

        % TODO would be nice to have a way to test 'for each thing that claims behavior X, run these'
        ?_assertEqual(ok, chef_key:list(ChefKey, OkFun)),
        ?_assertEqual(ok, chef_key:delete(ChefKey, OkFun)),
        ?_assertEqual(ok, chef_key:fetch(ChefKey, OkFun)),
        ?_assertEqual(ok, chef_key:update(ChefKey, OkFun)),
        ?_assertEqual(true, is_list(chef_key:record_fields(ChefKey))),
        ?_assertEqual(true, is_atom(chef_key:update_query(ChefKey))),
        ?_assertEqual(true, is_atom(chef_key:delete_query(ChefKey))),
        ?_assertEqual(true, is_atom(chef_key:list_query(ChefKey))),
        ?_assertEqual(true, is_atom(chef_key:create_query(ChefKey))),
        ?_assertEqual(true, is_atom(chef_key:find_query(ChefKey))),
        ?_assertError(unsupported, chef_key:bulk_get_query(ChefKey)),
        ?_assertEqual(false, chef_key:is_indexed(ChefKey)),


        ?_assertMatch([<<"A">>, ?KEY_NAME], chef_key:fields_for_fetch(ChefKey)),
        { "set_created does so",
          fun() ->
            ChefKey2 = ChefKey#chef_key{created_at = undefined, updated_at= undefined, last_updated_by = undefined},
            ChefKey3 = chef_key:set_created(ChefKey2, <<"me">>),
            #chef_key{created_at = Timestamp, updated_at = Timestamp, last_updated_by = UpdatedBy} = ChefKey3,
            ?assertMatch(<<_/binary>>, Timestamp),
            ?assertEqual(<<"me">>, UpdatedBy)
          end
        },
        { "set_updated does",
          fun() ->
            ChefKey2 = ChefKey#chef_key{created_at = undefined, updated_at= undefined, last_updated_by = undefined},
            ChefKey3 = chef_key:set_updated(ChefKey2, <<"me">>),
            #chef_key{created_at = CreatedTimestamp, updated_at = UpdatedTimestamp, last_updated_by = UpdatedBy} = ChefKey3,
            ?assertMatch(<<_/binary>>, UpdatedTimestamp),
            ?assertMatch(undefined, CreatedTimestamp),
            ?assertEqual(<<"me">>, UpdatedBy)
          end
        }


    ].

ejson_from_list_test() ->
    Decorator = fun(Name) -> iolist_to_binary(["hello ", Name]) end,
    List = [ [ <<"name1">>, true ], [ <<"name2">>, false] ],
    Expected = [ {[{<<"uri">>, <<"hello name1">>}, {<<"name">>, <<"name1">>}, {<<"expired">>, true}]},
                 {[{<<"uri">>, <<"hello name2">>}, {<<"name">>, <<"name2">>}, {<<"expired">>, false}]}
               ],
    ?assertMatch(Expected, chef_key:ejson_from_list(List, Decorator)).
