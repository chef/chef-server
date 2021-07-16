%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
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

%% There are no public API functions in mini_s3 for deserializing
%% AWS config information
-record(config, {
          s3_url="http://s3.amazonaws.com"::string(),
          access_key_id::string(),
          secret_access_key::string(),
          bucket_access_type=vhost

}).
setup_s3(InternalS3Url, ExternalS3Url) ->
    MockedModules = [mini_s3],
    chef_objects_test_utils:mock(MockedModules, [passthrough]),
    application:set_env(chef_objects, s3_platform_bucket_name, "testbucket"),
    application:set_env(chef_objects, s3_access_key_id, "super_id"),
    application:set_env(chef_objects, s3_secret_key_id, "super_secret"),
    application:set_env(chef_objects, s3_url, InternalS3Url),
    application:set_env(chef_objects, s3_external_url, ExternalS3Url),

    meck:expect(mini_s3, new, 3, mock_mini_s3_ctx),
    meck:expect(mini_s3, get_object_metadata, 4, mock_metadata).

generate_presigned_url_uses_configured_s3_url_test_() ->
    MockedModules = [mini_s3],
    HostHeaderUrl = "https://api.example.com:443",
    OrgId = <<"deadbeefdeadbeefdeadbeefdeadbeef">>,
    Checksum = <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
    Lifetime = 15,
    Expect_s3_url = fun(ExpectMethod, ExpectUrl) ->
                            meck:expect(mini_s3, s3_url,
                                        fun(HTTPMethod, Bucket, _Key, MyLifeTime, _ContentMD5,
                                            #config{s3_url = S3Url}) ->
                                                ?assertEqual(ExpectMethod, HTTPMethod),
                                                ?assertEqual("testbucket", Bucket),
                                                ?assertEqual(Lifetime, MyLifeTime),
                                                ?assertEqual(ExpectUrl, S3Url),
                                                stub_s3_url_response
                                        end)
                    end,
    [{"Calls mini_s3:s3_url with HostHeaderUrl when"
      "external s3 url is set to host_header",
      {foreach,
       fun() ->
               InternalS3Url = "https://FAKE_S3.com",
               ExternalS3Url = host_header,
               setup_s3(InternalS3Url, ExternalS3Url),
               {InternalS3Url, ExternalS3Url}
       end,
       fun(_) ->
               chef_objects_test_utils:unmock(MockedModules)
       end,
       [
        fun({_InternalS3Url, _ExternalS3Url}) ->
                [
                 {" (" ++ atom_to_list(Method) ++ ")",
                  fun() ->
                          Expect_s3_url(Method, HostHeaderUrl),
                          chef_s3:generate_presigned_url(OrgId, Lifetime, Method,
                                                         Checksum, HostHeaderUrl),
                          chef_objects_test_utils:validate_modules(MockedModules)
                  end}
                 || Method <- [put, get]] ++
                    [{" (batch checksums)",
                      fun() ->
                              Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                           <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                                           <<"cccccccccccccccccccccccccccccccc">>],
                              Expect_s3_url(put, HostHeaderUrl),
                              chef_s3:generate_presigned_urls(OrgId, Lifetime, put,
                                                              Checksums, HostHeaderUrl),
                              chef_objects_test_utils:validate_modules(MockedModules)
                      end}]
        end]}},

     {"Calls mini_s3:s3_url with InternalS3Url when"
      "external s3 url is set same as internal",
      {foreach,
       fun() ->
               InternalS3Url = "https://FAKE_S3.com",
               ExternalS3Url = InternalS3Url,
               setup_s3(InternalS3Url, ExternalS3Url),
               {InternalS3Url, ExternalS3Url}
       end,
       fun(_) ->
               chef_objects_test_utils:unmock(MockedModules)
       end,
       [
        fun({InternalS3Url, _ExternalS3Url}) ->
                [
                 {" (" ++ atom_to_list(Method) ++ ")",
                  fun() ->
                          Expect_s3_url(Method, InternalS3Url),
                          chef_s3:generate_presigned_url(OrgId, Lifetime, Method,
                                                         Checksum, HostHeaderUrl),
                          chef_objects_test_utils:validate_modules(MockedModules)
                  end}
                 || Method <- [put, get]] ++
                    [{" (batch checksums)",
                      fun() ->
                              Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                           <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                                           <<"cccccccccccccccccccccccccccccccc">>],
                              Expect_s3_url(put, InternalS3Url),
                              chef_s3:generate_presigned_urls(OrgId, Lifetime, put,
                                                              Checksums, HostHeaderUrl),
                              chef_objects_test_utils:validate_modules(MockedModules)
                      end}]
        end]}},

     {"Calls mini_s3:s3_url with external url when"
      "external s3 url is customized",
      {foreach,
       fun() ->
               InternalS3Url = "https://FAKE_S3.com",
               ExternalS3Url = "https://external-s3.com",
               setup_s3(InternalS3Url, ExternalS3Url),
               {InternalS3Url, ExternalS3Url}
       end,
       fun(_) ->
               chef_objects_test_utils:unmock(MockedModules)
       end,
       [
        fun({_InternalS3Url, ExternalS3Url}) ->
                [
                 {" (" ++ atom_to_list(Method) ++ ")",
                  fun() ->
                          Expect_s3_url(Method, ExternalS3Url),
                          chef_s3:generate_presigned_url(OrgId, Lifetime, Method,
                                                         Checksum, HostHeaderUrl),
                          chef_objects_test_utils:validate_modules(MockedModules)
                  end}
                 || Method <- [put, get]] ++
                    [{" (batch checksums)",
                      fun() ->
                              Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                           <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                                           <<"cccccccccccccccccccccccccccccccc">>],
                              Expect_s3_url(put, ExternalS3Url),
                              chef_s3:generate_presigned_urls(OrgId, Lifetime, put,
                                                              Checksums, HostHeaderUrl),
                              chef_objects_test_utils:validate_modules(MockedModules)
                      end}]
        end]}}
    ].

checksum_test_() ->
    MockedModules = [mini_s3, chef_s3, chef_s3_ops],
    {foreach,
     fun() ->
             %% Temporarily disable logging chef_s3:delete_checksums/2
             error_logger:tty(false),
             chef_objects_test_utils:mock(MockedModules, [passthrough]),
             meck:expect(chef_s3, get_internal_config, fun() -> mock_config end),
             application:set_env(chef_objects, s3_platform_bucket_name, "testbucket"),

             application:set_env(chef_objects, s3_parallel_ops_fanout, 3),
             application:set_env(chef_objects, s3_parallel_ops_timeout, 5000),

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
                                         meck:exception(error, {aws_error, {http_error, 404, blah}});
                                     [$f|_] ->
                                         timer:sleep(10 * 1000);
                                     _ ->
                                         %% Here, it'd just be:
                                         %% throw(oops)
                                         meck:exception(throw, oops)
                                 end
                         end),
             %% FIXME - copy pasta
             meck:expect(mini_s3, delete_object,
                         fun(_Bucket, Key, mock_config) ->
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
                                         [{delete_marker, foo}, {version_id, <<"1.0">>}];
                                     [$e|_] ->
                                         %% In real code, it'd be:
                                         %% erlang:error({aws_error, ...})
                                         meck:exception(error, {aws_error, {http_error, 404, blah}});
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
             %% Re-enable logging
             error_logger:tty(true),
             chef_objects_test_utils:unmock(MockedModules)
     end,
     [
      fun(OrgId) ->
              [{"Fetch: Finds all the checksums",
                fun() ->

                        %% They all end in "a", so they should all be found
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                end},

               {"Fetch: Finds all the checksums (greater than parallel limit)",
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
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                end},

              {"Fetch: Missing one of the checksums",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                end},
               {"Fetch: Error for one of the checksums",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbd">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {timeout, []},
                                      {error, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbd">>]}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                end},
               {"Fetch: Timeout!",
                %% 20 seconds is waaaaaaay liberal here
                {timeout, 20, fun() ->
                                      Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                                   <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                                   <<"cccccccccccccccccccccccccccccccf">>],

                                      ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                                    {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                                    {timeout, [<<"cccccccccccccccccccccccccccccccf">>]},
                                                    {error, []}},
                                                   chef_s3:check_checksums(OrgId, Checksums)),
                                      chef_objects_test_utils:validate_modules(MockedModules)
                              end}
               },
               {"Fetch: One of each",
                fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"cccccccccccccccccccccccccccccccd">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {timeout, []},
                                      {error, [<<"cccccccccccccccccccccccccccccccd">>]}},
                                     chef_s3:check_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                end},
               {"Fetch: All files are already in the system!",
                fun() ->
                        Checksums = [],
                        ?assertEqual({{ok, []},
                                      {missing, []},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:check_checksums(OrgId, Checksums))
                        %% Don't actually need to validate the mocks, since this shouldn't call them

                end},
                {"Delete: Finds all the checksums",
                 fun() ->

                        %% They all end in "a", so they should all be found
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:delete_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                 end},
                {"Delete: Multiple chunk groups",
                 fun() ->
                        %% This should produce 3 chunk groups (3,3,1)
                        %% They all end in "a", so they should all be found
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                     <<"ccccccccccccccccccccccccccccccca">>,
                                     <<"ddddddddddddddddddddddddddddddda">>,
                                     <<"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeea">>,
                                     <<"fffffffffffffffffffffffffffffffa">>,
                                     <<"ggggggggggggggggggggggggggggggga">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbba">>,
                                            <<"ccccccccccccccccccccccccccccccca">>,
                                            <<"ddddddddddddddddddddddddddddddda">>,
                                            <<"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeea">>,
                                            <<"fffffffffffffffffffffffffffffffa">>,
                                            <<"ggggggggggggggggggggggggggggggga">>]},
                                      {missing, []},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:delete_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                 end},
                {"Delete: Missing one of the checksums",
                 fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {timeout, []},
                                      {error, []}},
                                     chef_s3:delete_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                 end},
                {"Delete: Error for one of the checksums",
                 fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbd">>,
                                     <<"ccccccccccccccccccccccccccccccca">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                            <<"ccccccccccccccccccccccccccccccca">>]},
                                      {missing, []},
                                      {timeout, []},
                                      {error, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbd">>]}},
                                     chef_s3:delete_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                 end},
                {"Delete: Timeout!",
                 %% 20 seconds is waaaaaaay liberal here
                 {timeout, 20, fun() ->
                                      Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                                   <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                                   <<"cccccccccccccccccccccccccccccccf">>],

                                      ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                                    {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                                    {timeout, [<<"cccccccccccccccccccccccccccccccf">>]},
                                                    {error, []}},
                                                   chef_s3:delete_checksums(OrgId, Checksums)),
                                      chef_objects_test_utils:validate_modules(MockedModules)
                              end}
                },
                {"Delete: One of each",
                 fun() ->
                        Checksums = [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                                     <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>,
                                     <<"cccccccccccccccccccccccccccccccd">>],

                        ?assertEqual({{ok, [<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>]},
                                      {missing, [<<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbe">>]},
                                      {timeout, []},
                                      {error, [<<"cccccccccccccccccccccccccccccccd">>]}},
                                     chef_s3:delete_checksums(OrgId, Checksums)),
                        chef_objects_test_utils:validate_modules(MockedModules)
                 end}
              ]
      end]}.
