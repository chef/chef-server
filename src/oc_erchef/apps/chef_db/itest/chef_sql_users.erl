%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2015 Chef, Inc. All Rights Reserved.
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

-module(chef_sql_users).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

-define(PUBLIC_KEY, <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwxOFcrbsV7bEbqzOvW5u"
		      "W5lyB23qsenlUdIGyRttqzGEaki01s7X+PpYy4BLfmVVmA6A6FCbL38CzzTUFX1a"
		      "p6LYQR2Pb1tYjBjZZMUiVnjEgl12Zd1JF8dsPMj2BgPggx5GaGLvCOsajZ0YCDgW"
		      "WkoO/HAEbztFIx2jdSCyD0ZH0ep4fSGDjmkN+5XurS0dBH8J5qPeJjriA/s/RzUb"
		      "ULjr3gvfg49onHxr/kTKbhc78GBOfKSH1ftECCoWnidadW7/lfKbAZ3xiSjLsIxS"
		      "KxavHMeCuSgyReDZpsFOn2Saie26jvLxWrGyn870yIh36wMvCvWKwUQPnluSnstJ"
		      "xwIDAQAB">>).

-define(OTHER_PUBLIC_KEY, <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEArpz8ZFn6ptXTCGJ9WLxw
                            2EnoxAcWiw1NOtXtZ5G59XUyY9VBIaXDiQeMblG6FMGT5TexZ2uKHsW+WBRHsNUz
                            Tng/gjYKsbX/vUqOnmlUHqg8a9nvPlNK2UFT9wL93g+4NAudOGsd5DREA/rdQVSy
                            wRx3NqxpY92J9jcUldUGm3QCvHYA/VTZhdqtIFQP5E3w3eEYCyYVRgXCYztYLdKY
                            AqpU1SeWxwLBO9t/XF4eezqxf5EvHCOPBxYIxJsl3RPWmAEvLWzefNhrPgGZ3o/u
                            Fufx8Dq3nPOyFY/wGdHYHeGIkgynxJ4gRoZ5NmmvSWs1338V8yTe8zUqPmeM9eEM
                            awIDAQAB">>).


make_user(Prefix) ->
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    chef_user_record(AzId, false).

%% TODO: no such thing as an admin user anymore,
%% remove this function and update code / chef_user_record()
make_admin_user(Prefix) ->
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    chef_user_record(AzId, true).

chef_user_record(AzId, _Admin) ->
    #chef_user{
       server_api_version = ?API_MIN_VER,
       id = AzId,
       authz_id = AzId,
       username = AzId,
       email = AzId,
       public_key = ?PUBLIC_KEY,
       pubkey_version = 1,
       hashed_password = <<"secretHaxorz">>,
       salt = <<"kosher">>,
       hash_type = <<"bcrypt">>,
       last_updated_by = chef_test_suite_helper:actor_id(),
       created_at = {datetime,{{2011,10,1},{16,47,46}}},
       updated_at = {datetime,{{2011,10,1},{16,47,46}}},
       external_authentication_uid = <<"an open id of some kind">>,
       recovery_authentication_enabled = false,
       serialized_object = <<"{}">>
      }.

%%%======================================================================
%%% USERS
%%%======================================================================

insert_user_data() ->
    Users = [make_user(<<"user01">>), make_user(<<"user02">>)],
    Expected = lists:duplicate(length(Users), {ok, 1}),
    Results = [itest_util:create_record(User) || User <- Users],
    ?assertEqual(Expected, Results).

fetch_user_data() ->
    Expected = make_user(<<"user03">>),
    Username = Expected#chef_user.username,
    %% Make sure client create succeeds
    ?assertEqual({ok, 1}, itest_util:create_record(Expected)),
    Result = chef_db:fetch(#chef_user{username = Username}, chef_db:make_context(?API_MIN_VER, <<"ABCD">>)),
    ?assertEqual(Expected, Result).

fetch_user_list() ->
    Users = [make_user(<<"user04">>), make_user(<<"user05">>)],
    CreatedResults = lists:duplicate(length(Users), {ok, 1}),
    Created = [itest_util:create_record(User) || User <- Users ],
    ?assertEqual(CreatedResults, Created),

    Results = itest_util:list_records(#chef_user{}),
    Expected = [ User#chef_user.username || User <- Users ],
    ?assertEqual(Expected, Results).

delete_user_data() ->
    User = make_user(<<"user06">>),
    ?assertEqual({ok, 1}, itest_util:create_record(User)),
    Result = itest_util:delete_record(User),
    ?assertEqual({ok, 1}, Result),
    Username = User#chef_user.username,
    Result1 = chef_db:fetch(#chef_user{username = Username}, chef_db:make_context(?API_MIN_VER, <<"ABCD">>)),
    ?assertEqual(not_found, Result1).

update_user_data() ->
    User = make_user(<<"user07">>),
    ?assertEqual({ok, 1}, itest_util:create_record(User)),

    %% Check that public key we inserted is correct
    Username = User#chef_user.username,
    CreatedUser = chef_db:fetch(#chef_user{username = Username}, chef_db:make_context(?API_MIN_VER, <<"ABCD">>)),
    ?assertEqual(?PUBLIC_KEY, CreatedUser#chef_user.public_key),

    %% Update public key
    UpdatedUserData = User#chef_user{ public_key = ?OTHER_PUBLIC_KEY },
    Result = itest_util:update_record(UpdatedUserData),
    ?assertEqual({ok, 1}, Result),

    %% Did the public key really update?
    PersistedUser = chef_db:fetch(#chef_user{username = Username}, chef_db:make_context(?API_MIN_VER, <<"ABCD">>)),
    ?assertEqual(?OTHER_PUBLIC_KEY, PersistedUser#chef_user.public_key),

    %% Cleanup admin user so count_admin_users() tests will work
    {ok, 1} = chef_sql:delete_object(chef_object:delete_query(UpdatedUserData), chef_object:id(UpdatedUserData)).

count_admin_users() ->
    User = make_admin_user(<<"user08">>),
    ?assertEqual({ok, 1}, itest_util:create_record(User)),
    User2 = make_admin_user(<<"user09">>),
    ?assertEqual({ok, 1}, itest_util:create_record(User2)).
