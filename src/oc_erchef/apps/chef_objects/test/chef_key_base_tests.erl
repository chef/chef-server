%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%%
%% Copyright 2015-2018 Chef Software, Inc.
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

-module(chef_key_base_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

public_key_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("spki_public.pem"),
    Bin.

cert_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("cert.pem"),
    Bin.

set_key_pair_test_() ->
    Ejson = {[]},
    PrivateKey = <<"private">>,
    DataForType = fun(key) -> public_key_data();
                     (cert) -> cert_data()
                  end,
    KeyForType = fun(key) -> <<"public_key">>;
                    (cert) -> <<"certificate">>
                 end,
    NotKeyForType = fun(key) -> <<"certificate">>;
                       (cert) -> <<"public_key">>
                    end,
    Tests = [
             begin
                 Got = chef_key_base:set_key_pair(Ejson,
                                                  {public_key, DataForType(Type)},
                                                  {private_key, PrivateKey}),
                 [?_assertEqual(PrivateKey, ej:get({<<"private_key">>}, Got)),
                  ?_assertEqual(DataForType(Type), ej:get({KeyForType(Type)}, Got)),
                  ?_assertEqual(undefined, ej:get({NotKeyForType(Type)}, Got))]
             end
             || Type <- [key, cert] ] ++
            [
               {"public key is set but private key is not when private key is not provided and public is",
                fun() ->
                        KeyData = public_key_data(),
                        Got = chef_key_base:set_key_pair(Ejson, {public_key, KeyData}, {private_key, undefined}),
                        ?assertEqual(undefined, ej:get({<<"private_key">>}, Got)),
                        ?assertEqual(KeyData, ej:get({<<"public_key">>}, Got))


                end}],

    lists:flatten(Tests).

set_public_key_test_() ->
    EJ = {[]},
    KeyData = public_key_data(),
    [{"public key is set in ejson when provided",
      fun() ->
        NewEJ = chef_key_base:set_public_key(EJ, KeyData),
        ?assertEqual(KeyData, ej:get({<<"public_key">>}, NewEJ) )
      end},
     {"public key is null in ejson when null is set",
      fun() ->
        NewEJ = chef_key_base:set_public_key(EJ, null),
        ?assertEqual(null, ej:get({<<"public_key">>}, NewEJ))
      end},
     {"public key is undefined and cert is set in ejson when cert is provided",
      fun() ->
        CertData = cert_data(),
        NewEJ = chef_key_base:set_public_key(EJ, CertData),
        ?assertEqual(CertData, ej:get({<<"certificate">>}, NewEJ)),
        ?assertEqual(undefined, ej:get({<<"public_key">>}, NewEJ))
      end}
    ].

maybe_generate_key_pair_test_() ->
    {setup,
     fun chef_objects_test_utils:keygen_setup/0,
     fun chef_objects_test_utils:keygen_cleanup/1,
     [
      {"when called with create_key true, a key pair is generated and passed into the continuation fun",
      fun() ->
        Echo = fun(Data) -> Data end,
        {Pub, Priv} = chef_key_base:maybe_generate_key_pair({[{<<"create_key">>, true}]}, Echo),
        ?assertEqual(true, is_binary(Pub)),
        ?assertEqual(true, is_binary(Priv))

      end},
      {"when called with create_key false and a public key, the key is not generated and the public key is passed into the continuation fun",
      fun() ->
        Echo = fun(Data) -> Data end,
        Result = chef_key_base:maybe_generate_key_pair({[{<<"create_key">>, false},{<<"public_key">>, <<"pubkey">>}]},
                                                      Echo),
        ?assertMatch({<<"pubkey">>, undefined}, Result)

      end}
     ]
    }.

maybe_generate_key_pair_deprecated_test_() ->
    KeyData = public_key_data(),
    {setup,
     fun chef_objects_test_utils:keygen_setup/0,
     fun chef_objects_test_utils:keygen_cleanup/1,
     [{"when private_key is specified, the response contains a public and private key",
       fun() ->
         EJ = {[{<<"private_key">>, true}]},
         NewEJ = chef_key_base:maybe_generate_key_pair(EJ),
         ?assertEqual(true, is_binary(ej:get({<<"private_key">>}, NewEJ))),
         ?assertEqual(true, is_binary(ej:get({<<"public_key">>}, NewEJ)))
       end},
      {"when a valid public key is specified, the response is not modified.",
       fun() ->
         EJ = {[{<<"public_key">>, KeyData}]},
         NewEJ = chef_key_base:maybe_generate_key_pair(EJ),
         ?assertMatch(EJ,  NewEJ)
       end},
      {"when a valid public_key is specified alongside private_key:true, the response contains a new public and private key",
       fun() ->

         EJ = {[{<<"public_key">>, KeyData},
                {<<"private_key">>, true}]},
         NewEJ = chef_key_base:maybe_generate_key_pair(EJ),
         ?assertEqual(true, is_binary(ej:get({<<"private_key">>}, NewEJ))),
         ?assertEqual(true, is_binary(ej:get({<<"public_key">>}, NewEJ))),
         ?assertNotEqual(KeyData, ej:get({<<"public_key">>}, NewEJ))

       end}
     ]}.
    %validate_public_key_fields_test_() ->
