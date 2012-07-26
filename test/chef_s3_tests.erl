%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
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


-module(chef_s3_tests).

-include_lib("eunit/include/eunit.hrl").

base64_checksum_test_() ->
    TestData = [{<<"00ba0db453b47c4c0bb530cf8e481a70">>, <<"ALoNtFO0fEwLtTDPjkgacA==">>},
                {<<"25480201827ba22eef212617006c1491">>, <<"JUgCAYJ7oi7vISYXAGwUkQ==">>}],
    [
     {"Converts a binary string hex MD5 to binary",
      fun() ->
              ?assertEqual(chef_s3:base64_checksum(MD5),
                           Base64)
      end
     } || {MD5, Base64} <- TestData
    ].


make_key_test() ->
    OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
    Checksum = <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,

    ?assertEqual(chef_s3:make_key(OrgId, Checksum),
                 "organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb").

url_test_() ->
    MockedModules = [mini_s3],
    {
      foreach,
      fun() ->
              test_utils:mock(MockedModules, [passthrough]),
              application:set_env(chef_common, s3_platform_bucket_name, "testbucket"),
              application:set_env(chef_common, s3_access_key_id, "super_id"),
              application:set_env(chef_common, s3_secret_key_id, "super_secret"),
              application:set_env(chef_common, s3_url, "https://FAKE_S3.com"),

              meck:expect(mini_s3, new, 3, blah),
              meck:expect(mini_s3, get_object_metadata, 4, something),

              meck:expect(mini_s3, s3_url,
                          fun(_Method, BucketName, Key, _Lifetime, _ContentMD5, _Config) ->
                                  X = "https://FAKE_S3.com/" ++ BucketName ++ "/" ++ Key,
                                  list_to_binary(X)
                          end)
      end,
      fun(_) ->
              test_utils:unmock(MockedModules)
      end,
      [{"Generates presigned url for post",
        fun() ->
                OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
                Lifetime = 15,
                Checksum = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                Type = put,

                ?assertEqual(chef_s3:generate_presigned_url(OrgId, Lifetime, Type, Checksum),
                             <<"https://FAKE_S3.com/testbucket/organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>),
                test_utils:validate_modules(MockedModules)
        end},
      {"Generates presigned url for get",
        fun() ->
                OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
                Lifetime = 15,
                Checksum = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                Type = get,

                ?assertEqual(chef_s3:generate_presigned_url(OrgId, Lifetime, Type, Checksum),
                             <<"https://FAKE_S3.com/testbucket/organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>),
                test_utils:validate_modules(MockedModules)
        end},
       {"Generates presigned checksum / url tuples in a batch",
        fun() ->
                OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
                Lifetime = 15,
                Type = put,
                Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                             <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                             <<"cccccccccccccccccccccccccccccccc">>],

                ?assertEqual(chef_s3:generate_presigned_urls(OrgId, Lifetime, Type, Checksums),
                             [{<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                               <<"https://FAKE_S3.com/testbucket/organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>},
                              {<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                               <<"https://FAKE_S3.com/testbucket/organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>},
                              {<<"cccccccccccccccccccccccccccccccc">>,
                               <<"https://FAKE_S3.com/testbucket/organization-deadbeefdeadbeefdeadbeefdeadbeef/checksum-cccccccccccccccccccccccccccccccc">>}]),
                test_utils:validate_modules(MockedModules)
        end}]}.

check_test_() ->
    MockedModules = [mini_s3, chef_s3],
    {foreach,
     fun() ->
             test_utils:mock(MockedModules, [passthrough]),
             meck:expect(chef_s3, get_config, fun() -> mock_config end),
             application:set_env(chef_common, s3_platform_bucket_name, "testbucket"),

             %% Actual value doesn't matter; we're not going to use it anyway
             meck:expect(mini_s3, new, 3, ignored),

             meck:expect(mini_s3, get_object_metadata,
                         fun(_Bucket, Key, _Options, mock_config) ->
                                 %% Since we're mocking, we need an easy way to control what
                                 %% happens in this function.  We're going to take the last
                                 %% character in the Key (which happens to be the last
                                 %% character in the checksum).  If that character is an
                                 %% "a", we'll consider the checksum to have been uploaded;
                                 %% if it's an "e", it's missing; if it's an "f", timeout;
                                 %% any other character means some error occurred

                                 Reversed = lists:reverse(Key),
                                 case Reversed of
                                     [$a|_] ->
                                         %% The real value is unimportant, since it's
                                         %% ignored; the important thing is that it wasn't
                                         %% an abnormal return
                                         ok;
                                     [$e|_] ->
                                         %% In real code, it'd be:
                                         %% erlang:error({aws_error, ...})
                                         meck:exception(error, {aws_error, {http_error, 404, blah, blah}});
                                     [$f|_] ->
                                         timer:sleep(10 * 1000);
                                     _ ->
                                         %% Here, it'd just be:
                                         %% throw(oops)
                                         meck:exception(throw, oops)
                                 end
                         end),
             OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,

             OrgId
     end,
     fun(_OrgId) ->
             test_utils:unmock(MockedModules)
     end,
     [
      fun(OrgId) ->
              [{"Finds all the checksums",
                fun() ->

                        %% They all end in "a", so they should all be found
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {error, 0}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        test_utils:validate_modules(MockedModules)
                end},

               {"Finds all the checksums (greater than parallel limit)",
                fun() ->

                        %% They all end in "a", so they should all be found
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                     <<"ccccccccccccccccccccccccccccccca">>,
                                     <<"ddddddddddddddddddddddddddddddda">>,
                                     <<"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeea">>,
                                     <<"fffffffffffffffffffffffffffffffa">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                            <<"ccccccccccccccccccccccccccccccca">>,
                                            <<"ddddddddddddddddddddddddddddddda">>,
                                            <<"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeea">>,
                                            <<"fffffffffffffffffffffffffffffffa">>]},
                                      {missing, []},
                                      {error, 0}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        test_utils:validate_modules(MockedModules)
                end},

              {"Missing one of the checksums",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {error, 0}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        test_utils:validate_modules(MockedModules)
                end},
               {"Error for one of the checksums",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbd">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {error, 1}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        test_utils:validate_modules(MockedModules)
                end},
               {"Timeout!",
                %% 20 seconds is waaaaaaay liberal here
                {timeout, 20, fun() ->
                                      Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                                   <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                                   <<"cccccccccccccccccccccccccccccccf">>],

                                      ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                                    {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                                    {error, 1}},
                                                   chef_s3:check_checksums(OrgId, Checksums)),
                                      test_utils:validate_modules(MockedModules)
                              end}
               },
               {"One of each",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"cccccccccccccccccccccccccccccccd">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {error, 1}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        test_utils:validate_modules(MockedModules)
                end},
               {"All files are already in the system!",
                fun() ->
                        Checksums = [],
                        ?assertEqual({{ok, []},
                                      {missing, []},
                                      {error, 0}},
                                     chef_s3:check_checksums(OrgId, Checksums))
                        %% Don't actually need to validate the mocks, since this shouldn't call them

                end}
              ]
      end]}.
