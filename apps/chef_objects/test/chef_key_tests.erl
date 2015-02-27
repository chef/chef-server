%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80. -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
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

-module(chef_key_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/chef_types.hrl").

-define(KEY_NAME, <<"test_key">>).
-define(DEFAULT_EXPIRATION, <<"2099-10-24T22:49:08">>).
-define(PARSED_DEFAULT_EXPIRATION, {{2099,10,24},{22,49,08}}).

example_key() ->
    example_key(?DEFAULT_EXPIRATION, undefined).

example_key(Expiration, undefined) ->
    {ok, PublicKey} = file:read_file("../test/spki_public.pem"),
    example_key(Expiration, PublicKey);
example_key(Expiration, PublicKey) ->
    {[{<<"name">>, ?KEY_NAME}, {<<"public_key">>, PublicKey}, {<<"expiration_date">>, Expiration}]}.

%% TODO
update_from_ejson_test_() ->
    [{"check that ejson_for_indexing returns the updated ejson",
      fun() ->
	      ?assertError(need_to_implement, chef_key:update_from_ejson(a,b))
      end
     }].

%% TODO
fields_for_update_test_() ->
    [{"check that the same data parse that happens in flatten happens in fields_for_update",
      fun() ->
	      ?assertError(need_to_implement, chef_key:fields_for_update(#chef_key{}))
      end
     }].

new_record_test_() ->
    [{"check that a new record is generated from valid KeyData with properly parsed date",
      fun() ->
	      KeyData = example_key(),
	      ID = <<"testid">>,
	      ?assertEqual(#chef_key{id = ID, key_name = ?KEY_NAME,
				     public_key = ej:get({<<"public_key">>}, KeyData),
				     key_version = 0, expires_at = ?PARSED_DEFAULT_EXPIRATION},
			   chef_key:new_record(unused, unused, {ID, KeyData}))
      end
     },
     {"check that a new record is generated from valid KeyData with properly parsed infinity date",
      fun() ->
	      KeyData = example_key(<<"infinity">>, undefined),
	      ID = <<"testid">>,
	      ?assertEqual(#chef_key{id = ID, key_name = ?KEY_NAME,
				     public_key = ej:get({<<"public_key">>}, KeyData),
				     key_version = 0, expires_at = ?INFINITY_TIMESTAMP},
			   chef_key:new_record(unused, unused, {ID, KeyData}))
      end
     },
     {"check that KeyData with an invalid date throws bad_date",
      fun() ->
	      KeyData = example_key(<<"not-a-date">>, undefined),
	      ID = <<"testid">>,
	      ?assertThrow({ec_date,{bad_date,_}}, chef_key:new_record(unused, unused, {ID, KeyData}))
      end
     },
     {"check that KeyData with an invalid public_key throws invalid_public_key",
      fun() ->
	      KeyData = example_key(<<"infinity">>, <<"invalid_key">>),
	      ID = <<"testid">>,
	      ?assertThrow(invalid_public_key,chef_key:new_record(unused, unused, {ID, KeyData}))
      end
     }
    ].

parse_expiration_test_() ->
    [{"check that parse_expiration handles infinity properly",
      fun() ->
	      ?assertEqual(?INFINITY_TIMESTAMP, chef_key:parse_expiration(<<"infinity">>))
      end
     }].

parse_binary_json_test_() ->
    [{"check that a valid key is accepted",
      fun() ->
	      Key = example_key(),
	      EncodedKey = jiffy:encode(Key),
	      ?assertEqual(Key, chef_key:parse_binary_json(EncodedKey, undefined))
      end},
     {"check that a valid key with an infinity expiration is accepted",
      fun() ->
	      Key = example_key(<<"infinity">>, undefined),
	      EncodedKey = jiffy:encode(Key),
	      ?assertEqual(Key, chef_key:parse_binary_json(EncodedKey, undefined))
      end},
     {"check that key with an invalid name is rejected",
      fun() ->
	      OriginalKey = example_key(<<"infinity">>, undefined),
	      Key = ej:set({<<"name">>}, OriginalKey, <<"invalid^name">>),
	      EncodedKey = jiffy:encode(Key),
	      ?assertThrow({ej_invalid,string_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, undefined))
      end},
     {"check that key with an invalid date is rejected",
      fun() ->
	      Key = example_key(<<"not-a-valid-date">>, undefined),
	      EncodedKey = jiffy:encode(Key),
	      ?assertThrow({ec_date,{bad_date,_}}, chef_key:parse_binary_json(EncodedKey, undefined))
      end},
     {"check that key with an invalid public_key is rejected",
      fun() ->
	      Key = example_key(<<"infinity">>, <<"-----BEGIN PUBLIC KEY-----\ninvalid_key\n-----END PUBLIC KEY-----">>),
	      EncodedKey = jiffy:encode(Key),
	      ?assertThrow({ej_invalid,fun_match,_,_,_,_,_}, chef_key:parse_binary_json(EncodedKey, undefined))
      end},
     {"check that parse_binary_json fails on existing keys",
      fun() ->
	      ?assertError(unsupported, chef_key:parse_binary_json(undefined, #chef_key{}))
      end
     }
    ].
