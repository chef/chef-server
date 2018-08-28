%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
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

-module(chef_password_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OSC_DEFAULT_HASH_TYPE, <<"erlang-bcrypt-0.5.0">>).
-define(OSC_MIGRATION_HASH_TYPE, <<"sha1+bcrypt">>).
-define(DEFAULT_HASH_TYPE, <<"bcrypt">>).
-define(MIGRATION_HASH_TYPE, <<"SHA1-bcrypt">>).

encrypt_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     [{"encrypts a password",
       fun() ->
               {Hashed, Salt, Type} = chef_password:encrypt("fizzbuzz"),
               [ ?assert(is_binary(B)) || B <- [Hashed, Salt, Type] ]
       end},

      {"empty password not allowed",
       fun() ->
               ?assertError({invalid_password, ""}, chef_password:encrypt("")),
               ?assertError({invalid_password, <<"">>}, chef_password:encrypt(<<"">>))
       end}

     ]}.

bcrypt_round_trip_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     [
      {"basic roundtrip", generator,
       fun() ->
               Passwords = [<<"a">>, <<"a long password">>, "string123"],
               PassData = [ {P, chef_password:encrypt(P)} || P <- Passwords ],
               [ ?_assertEqual(true, chef_password:verify(P, Data))
                 || {P, Data} <- PassData ]
       end},

      {"salt not specified roundtrip",
       fun() ->
               P = "a long password",
               {Hash, _, Type} = chef_password:encrypt(P),
               ?assertEqual(true, chef_password:verify(P, {Hash, <<>>, Type}))
       end},

      {"wrong password is wrong",
       fun() ->
               ?assertEqual(false,
                            chef_password:verify("two",
                                                    chef_password:encrypt("one")))
       end}

     ]}.

cross_type_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     [
      {"treats a OSC_DEFAULT_HASH_TYPE as a DEFAULT_HASH_TYPE",
       fun() ->
            {Hash, Salt, ?DEFAULT_HASH_TYPE} = chef_password:encrypt("a password for me"),
            ?assertEqual(true, chef_password:verify("a password for me",
                                                       {Hash, Salt, ?OSC_DEFAULT_HASH_TYPE}))
       end}
     ]}.

unmigrated_round_trip_test_() ->
    {setup,
     fun() ->
             chef_objects_test_utils:bcrypt_setup(),
             osc_sha1_examples() ++ ec_sha1_examples()
     end,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     fun(Examples) ->
             [

              [ ?_assertEqual(true, chef_password:verify(Pass, {Hash, Salt, null}))
                || {Pass, Hash, Salt} <- Examples]
              ]
     end}.

migrated_round_trip_test_() ->
    {setup,
     fun() ->
            chef_objects_test_utils:bcrypt_setup(),
            osc_migrated_examples() ++ ec_migrated_examples()
     end,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     fun(MigratedExamples) ->
             [
              [ ?_assertEqual(true, chef_password:verify(Pass, Prev))
                || {Pass, Prev} <- MigratedExamples ],

              {"wrong password is wrong",
               fun() ->
                       {_, DbData} = hd(MigratedExamples),
                       WrongPass = "not the password",
                       ?assertEqual(false, chef_password:verify(WrongPass, DbData))
               end}
              ]
     end}.

upgrade_test_() ->
    {setup,
     fun chef_objects_test_utils:bcrypt_setup/0,
     fun chef_objects_test_utils:bcrypt_cleanup/1,
     [
       {"upgrades password data when current form is sha1", generator,
       fun() ->
            Data = [hd(osc_sha1_examples())] ++ [hd(ec_sha1_examples())],
            [ ?_assertMatch({_, _, ?DEFAULT_HASH_TYPE}, chef_password:upgrade(Password, {Hash, Salt, null})) ||
                             {Password, Hash, Salt} <- Data ]

       end},
       {"does not upgrade a password when current form is bcrypt",
       fun() ->
            Password = "a password for me",
            Original = chef_password:encrypt(Password),
            ?assertEqual(Original, chef_password:upgrade(Password, Original))
       end}
     ]}.


slow_compare_test_() ->
    Tests = [
             %% positive cases
             {{"abc", "abc"}, true},
             {{"", ""}, true},
             {{"a", "a"}, true},
             {{"aaaaaa", "aaaaaa"}, true},
             %% negative cases
             {{"a", "b"}, false},
             {{"abc", "aXc"}, false},
             {{"abc", "abX"}, false},
             {{"abX", "abc"}, false},
             {{"", "a"}, false},
             {{"a", ""}, false},
             {{"abc", "abc123"}, false}],
    [ ?_assertEqual(Expect, chef_password:slow_compare(A, B))
      || {{A, B}, Expect} <- Tests ].

%% Example data generated with password_gen.rb
%% Format is {Password, HashedPass, Salt}.
osc_sha1_examples() ->
    [
        {<<"a">>, <<"af75371eb751df0016370c3d28d2ad74670d5e52">>, <<"2014-06-25 11:43:48 -0400SFaNUO5tJTntDSilxtw0slOcoVgmFg">>},
        {<<"b">>, <<"076e95e29e26ba631c862a8281aba6049b6ec920">>, <<"2014-06-25 11:43:48 -04004nADNlKYncv6iBBEsISgV7iPvMuQbk">>},
        {<<"fizzbuz">>, <<"e49b70947b5e6358111b17be0f5c37d34d2afbe6">>, <<"2014-06-25 11:43:48 -0400m7acRVNa3P6Yg4qpnDisCFvA8bEqSm">>},
        {<<"sesame">>, <<"537abd120fcc28e5be3b2faa2aeafe5f47d0171d">>, <<"2014-06-25 11:43:48 -0400FjzotYDqRB2o4IFbcHKMX30x8iIZsM">>},
        {<<"apple">>, <<"7dd7b5cc04a7b3f59098356fa582c17834ba98a8">>, <<"2014-06-25 11:43:48 -04002ZMgJTBxbCVPCiXhlvVKpuTA2J6YLq">>}
    ].
ec_sha1_examples() ->
    [
        {<<"a">>, <<"8975d201890516f658ff92f4df9068027707a03f">>, <<"2014-06-25 11:43:48 -0400fpk83jDbAOc0OBcV4ogF653GVVOMRn">>},
        {<<"b">>, <<"17ee8561bf7397150e71174517ab974cdd65b82e">>, <<"2014-06-25 11:43:48 -0400jQVVx4gq6Ehk2DbZ4vaY8nOh5FY5cx">>},
        {<<"fizzbuz">>, <<"3da5133e640f88d230aa5e7eccb6d416496e1552">>, <<"2014-06-25 11:43:48 -04006gpZ4GsmEeFPFBhQ0KgnIEWzB8HwPF">>},
        {<<"sesame">>, <<"29540420f534f6542b344e3ef861ac3cfc0bf497">>, <<"2014-06-25 11:43:48 -0400BnDpBdHxSehr7t8evqsuhATbhQpZbT">>},
        {<<"apple">>, <<"602f9c8cbec7f723205957e5ab79aa384a97c76c">>, <<"2014-06-25 11:43:48 -0400WhloJNsMAmlIHDXrOIUmLVPc3y0T3M">>}
    ].

%% migration data generators
osc_migrated_examples() ->
    [ {Password, do_osc_migration(SHA1, Salt)} || {Password, SHA1, Salt} <- osc_sha1_examples()  ].


ec_migrated_examples() ->
    [ {Password, do_ec_migration(SHA1, Salt)} || {Password, SHA1, Salt} <- ec_sha1_examples()  ].

% Perform midway migration to sha+bcrypt for OSC
% this function only existed in chef_password for purpsoses
% of testing, so it is moved here
do_osc_migration(SHA, Salt) ->
    {ok, BcryptSalt} = bcrypt:gen_salt(),
    {ok, HashedPass} = bcrypt:hashpw(SHA, BcryptSalt),
    CompoundSalt = iolist_to_binary([BcryptSalt, <<"\t">>, Salt]),
    {list_to_binary(HashedPass), CompoundSalt, ?OSC_MIGRATION_HASH_TYPE}.


% Perform midway migration to sha+bcrypt for EC
% this currently exists in chef_mover/src/mover_user_hash_converter.erl
% and is executed during the user password migration phase of an EC upgrade.
% Very similar to OSC migration, but does not store compound salt - instead
% stores the original salt, because bcrypt salt is part of the bcrypt hash itself.
do_ec_migration(SHA, Salt) ->
    {ok, BcryptSalt} = bcrypt:gen_salt(),
    {ok, HashedPass} = bcrypt:hashpw(SHA, BcryptSalt),
    {list_to_binary(HashedPass), Salt, ?MIGRATION_HASH_TYPE}.

