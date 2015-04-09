%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc Paradise <marc@chef.io>
%%
%% Copyright 2015 Chef Software, Inc.
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
-include("../../include/chef_types.hrl").

public_key_data() ->
    {ok, Bin} = file:read_file("../test/spki_public.pem"),
    Bin.

cert_data() ->
    {ok, Bin} = file:read_file("../test/cert.pem"),
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
             || Type <- [key, cert] ],
    lists:flatten(Tests).

