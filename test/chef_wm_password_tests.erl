%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
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

-module(chef_wm_password_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    %% turn the number of rounds way down so that running the tests
    %% doesn't take too long.
    application:set_env(bcrypt, default_log_rounds, 4),
    [ ensure_start(App) || App <- [crypto, bcrypt] ],
    ok.

cleanup(_) ->
    error_logger:tty(false),
    application:stop(bcrypt),
    error_logger:tty(true).


encrypt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"encrypts a password",
       fun() ->
               {Hashed, Salt, Type} = chef_wm_password:encrypt("fizzbuzz"),
               [ ?assert(is_binary(B)) || B <- [Hashed, Salt, Type] ]
       end},

      {"empty password not allowed",
       fun() ->
               ?assertError({invalid_password, ""}, chef_wm_password:encrypt("")),
               ?assertError({invalid_password, <<"">>}, chef_wm_password:encrypt(<<"">>))
       end}

     ]}.

bcrypt_round_trip_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"basic roundtrip", generator,
       fun() ->
               Passwords = [<<"a">>, <<"a long password">>, "string123"],
               PassData = [ {P, chef_wm_password:encrypt(P)} || P <- Passwords ],
               [ ?_assertEqual(true, chef_wm_password:verify(P, Data))
                 || {P, Data} <- PassData ]
       end},

      {"wrong password is wrong",
       fun() ->
               ?assertEqual(false,
                            chef_wm_password:verify("two",
                                                    chef_wm_password:encrypt("one")))
       end}

     ]}.
    


%% Example data generated with the Ruby code found at the bottom of
%% this file. Format is {Password, HashedPass, Salt}.
ruby_examples() ->
    [
     {<<"a">>, <<"7a9589e57fd63d1d9a222ef46ec5533335dcf17b">>,
      <<"2012-09-13 16:26:31 -0700wBSQfHuJwXMUprGBS5DB1I7oiE5h4p">>},

     {<<"b">>, <<"10eb130a8c7e69426c7e5e27a149b1989c2263a3">>,
      <<"2012-09-13 16:26:31 -0700HzvX3eKpiXrUmlhcqQUjAgl00TZeBn">>},

     {<<"fizzbuz">>, <<"746999ea6212e1527d6d8df02244974460fb1f24">>,
      <<"2012-09-13 16:26:31 -0700VRnaEjCwvgGKzpJFHGYkjWu0xc7QS8">>},

     {<<"sesame">>, <<"e73cdca4fa427ce949ef0a4f55b09e008b38f1fe">>,
      <<"2012-09-13 16:26:31 -0700I6Dx1b5qO7b8S8C62uTNVJWaP4VSpG">>},

     {<<"apple">>, <<"e30e7ac9bd7ba12b04601bccc3cb5e219aa67f76">>,
      <<"2012-09-13 16:26:31 -0700sofGiQFCSs42teEg0nfl2QLnc2Ar7r">>}].

%% Generate post-migration data. Note that the bcrypt application must
%% be running in order for this call to succeed.
migrated_examples() ->
    [ {Pass, chef_wm_password:migrate_sha1(SHA, OrigSalt)}
      || {Pass, SHA, OrigSalt} <- ruby_examples() ].

migration_round_trip_test_() ->
    {setup,
     fun() ->
             setup(),
             migrated_examples()
     end,
     fun cleanup/1,
     fun(MigratedExamples) ->
             [
              [ ?_assertEqual(true, chef_wm_password:verify(Pass, Prev))
                || {Pass, Prev} <- MigratedExamples ],

              {"wrong password is wrong",
               fun() ->
                       {_, DbData} = hd(MigratedExamples),
                       WrongPass = "not the password",
                       ?assertEqual(false, chef_wm_password:verify(WrongPass, DbData))
               end}
              ]
     end}.
     %%  end},
     %% ]}.

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
    [ ?_assertEqual(Expect, chef_wm_password:slow_compare(A, B))
      || {{A, B}, Expect} <- Tests ].

ensure_start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Error ->
            error(Error)
    end.

%% ## Ruby class used for generating sample data
%% class ShaHasher
%%   def self.generate_salt
%%     salt = Time.now.to_s
%%     chars = ("a".."z").to_a + ("A".."Z").to_a + ("0".."9").to_a
%%     1.upto(30) { |i| salt << chars[rand(chars.size-1)] }
%%     salt
%%   end
%%
%%   def self.encrypt_password(password, salt)
%%     Digest::SHA1.hexdigest("--#{salt}--#{password}--")
%%   end
%%
%%   def self.make_examples(words)
%%     words.each do |w|
%%       s = self.generate_salt
%%       puts self.fmt_entry(w, self.encrypt_password(w, s), s)
%%     end
%%   end
%%
%%   def self.fmt_entry(pass, hash, salt)
%%     '{<<"%s">>, <<"%s">>, <<"%s">>}' % [pass, hash, salt]
%%   end
%% end
