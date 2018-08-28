%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
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


-module(chef_client_tests).
-include("chef_types.hrl").
-include("chef_osc_defaults.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(VD(D), chef_objects_test_utils:versioned_desc(Version,D)).
-define(VDD(D), chef_objects_test_utils:versioned_desc(Version, iolist_to_binary(["[deprecated] ", D]))).


assemble_client_ejson_test_() ->
    chef_objects_test_utils:make_deprecated_tests(fun assemble_client_ejson_deprecated_tests/1) ++
    chef_objects_test_utils:make_non_deprecated_tests(fun assemble_client_ejson_non_deprecated_tests/1).

assemble_client_ejson_deprecated_tests(Version) ->
    [{?VDD("obtain expected EJSON"),
      fun() ->
              Client = #chef_client{server_api_version = Version,
                                    name = <<"alice">>,
                                    admin = true,
                                    validator = false,
                                    public_key = public_key_data()},
              {GotList} = chef_client:assemble_client_ejson(Client, <<"ponyville">>),
              FilteredList = chomp_key_in_json_list(GotList),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"public_key">>, public_key_data()},
                              {<<"validator">>, false},
                              {<<"name">>, <<"alice">>},
                              {<<"clientname">>, <<"alice">>},
                              {<<"orgname">>, <<"ponyville">>}
                             ],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(FilteredList))
      end},
     {?VDD("converts certificate to public key"),
      fun() ->
              Client = #chef_client{server_api_version = Version,
                                    name = <<"alice">>,
                                    admin = true,
                                    validator = false,
                                    public_key = cert_data()},
              {GotList} = chef_client:assemble_client_ejson(Client, <<"ponyville">>),
              FilteredList = chomp_key_in_json_list(GotList),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"public_key">>, public_key_data()},
                              {<<"validator">>, false},
                              {<<"name">>, <<"alice">>},
                              {<<"clientname">>, <<"alice">>},
                              {<<"orgname">>, <<"ponyville">>}
                             ],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(FilteredList))
      end}
    ].


assemble_client_ejson_non_deprecated_tests(Version) ->
    [{?VD("obtain expected EJSON"),
      fun() ->
              Client = #chef_client{server_api_version = Version,
                                    name = <<"alice">>,
                                    validator = false },
              {GotList} = chef_client:assemble_client_ejson(Client, <<"ponyville">>),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"validator">>, false},
                              {<<"name">>, <<"alice">>},
                              {<<"clientname">>, <<"alice">>},
                              {<<"orgname">>, <<"ponyville">>}
                             ],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end}
    ].

parse_binary_json_test_() ->
    chef_objects_test_utils:make_all_versions_tests(fun parse_binary_json_tests/1) ++
    chef_objects_test_utils:make_deprecated_tests(fun parse_binary_json_deprecated_tests/1) ++
    chef_objects_test_utils:make_non_deprecated_tests(fun parse_binary_json_non_deprecated_tests/1).


%% These are stable behaviors across all supported api versions.
parse_binary_json_tests(Version) ->
    [{?VD("Error thrown on mismatched names in create"),
     fun() ->
         Body = <<"{\"name\":\"name\",\"clientname\":\"notname\"}">>,
         ?assertThrow({client_name_mismatch}, chef_client:parse_binary_json(Version, Body, undefined))
     end},
     {?VD("Error thrown on mismatched names in update"),
      fun() ->
          Body = <<"{\"name\":\"name\",\"clientname\":\"notname\"}">>,
          ?assertThrow({client_name_mismatch}, chef_client:parse_binary_json(Version, Body, undefined))
      end},
     {?VD("Can create with only name"),
      fun() ->
          Body = <<"{\"name\":\"name\"}">>,
          {ok, Client} = chef_client:parse_binary_json(Version, Body, undefined),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end},
     {?VD("Can create with only clientname"),
      fun() ->
          Body = <<"{\"clientname\":\"name\"}">>,
          {ok, Client} = chef_client:parse_binary_json(Version, Body, undefined),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end},
     {?VD("Error thrown with no name or clientname"),
      fun() ->
          Body = <<"{\"validator\":false}">>,
          ?assertThrow({both_missing, <<"name">>, <<"clientname">>},
                       chef_client:parse_binary_json(Version, Body, undefined))
      end},
     {?VD("Error thrown with bad name"),
      fun() ->
          Body = <<"{\"name\":\"bad~name\"}">>,
          ?assertThrow({bad_client_name, <<"bad~name">>,
                        <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>},
                       chef_client:parse_binary_json(Version, Body, undefined))
      end}
    ].


parse_binary_json_deprecated_tests(Version) ->
    [
     {?VDD("Accepted when private_key is included for create or update"),
      fun() ->
          Body = chef_json:encode({[{ <<"private_key">>, true }, { name, <<"a_name">> }]}),
          ?assertMatch({ok, _},
                       chef_client:parse_binary_json(Version, Body, undefined)),
          ?assertMatch({ok, _},
                       chef_client:parse_binary_json(Version, Body, #chef_client{}))
      end},
     {?VDD("Accepted when public_key is included for create or update"),
      fun() ->
          Body = chef_json:encode({[{ <<"public_key">>, public_key_data()}, { name, <<"a_name">> }]}),
          ?assertMatch({ok, _},
                       chef_client:parse_binary_json(Version, Body, #chef_client{})),
          ?assertMatch({ok, _},
                       chef_client:parse_binary_json(Version, Body, undefined))
      end}
    ].


% Once the behavior they replace is retired, move these tests in parse_binary_json_test_
parse_binary_json_non_deprecated_tests(Version) ->
    [
     {?VD("Error thrown when private_key is included for create or update"),
      fun() ->
          Body = chef_json:encode({[{ <<"private_key">>, true }, { name, <<"a_name">> }]}),
          ?assertThrow(private_key_field_not_supported,
                       chef_client:parse_binary_json(Version, Body, undefined)),
          ?assertThrow(private_key_field_not_supported,
                       chef_client:parse_binary_json(Version, Body, #chef_client{}))
      end},
     {?VD("Error thrown when public_key is included for update"),
      fun() ->
          Body = chef_json:encode({[{ <<"public_key">>, public_key_data()}, { name, <<"a_name">> }]}),
          ?assertThrow(key_management_not_supported,
                       chef_client:parse_binary_json(Version, Body, #chef_client{}))
      end},
     {?VD("Error thrown zero when create_key is included for update"),
      fun() ->
          Body = chef_json:encode({[{ <<"create_key">>, true}, {name, <<"a_name">> }]}),
          ?assertThrow(key_management_not_supported,
                       chef_client:parse_binary_json(Version, Body, #chef_client{}))
      end}
   ].


new_record_test_() ->
    chef_objects_test_utils:make_deprecated_tests(fun new_record_deprecated_tests/1) ++
    chef_objects_test_utils:make_non_deprecated_tests(fun new_record_tests/1).


new_record_tests(Version) ->
    [
     {?VD("new_record creates a correct new record without admin or public key data"),
      fun() ->
        PubkeyData = public_key_data(),
        OrgId = <<"12345678123456781234567812345678">>,
        AuthzId = <<"00000000000000000000000011111111">>,
        ClientData = {[{<<"name">>, <<"my-client">>},
                       {<<"alpha">>, <<"bravo">>}, % Ignored
                       {<<"admin">>, true}, % Ignored
                       {<<"validator">>, true},
                       {<<"public_key">>, PubkeyData}]}, % Ignored
        Client = chef_client:new_record(Version, OrgId, AuthzId, ClientData),
        ?assertMatch(#chef_client{server_api_version = Version,
                                 org_id = OrgId,
                                 authz_id = AuthzId,
                                 admin = false, % we default false in the record
                                 validator = true,
                                 name = <<"my-client">>,
                                 public_key = undefined,
                                 pubkey_version = undefined}, Client)


     end}
   ].

new_record_deprecated_tests(Version) ->
    [
     {?VD("new_record creates a correct new record"),
      fun() ->
        PubkeyData = public_key_data(),
        OrgId = <<"12345678123456781234567812345678">>,
        AuthzId = <<"00000000000000000000000011111111">>,
        ClientData = {[{<<"name">>, <<"my-client">>},
                       {<<"alpha">>, <<"bravo">>}, % Ignored
                       {<<"admin">>, true},
                       {<<"validator">>, true},
                       {<<"public_key">>, PubkeyData}]},
        Client = chef_client:new_record(Version, OrgId, AuthzId, ClientData),
        ?assertMatch(#chef_client{server_api_version = Version,
                                  org_id = OrgId,
                                  authz_id = AuthzId,
                                  admin = true,
                                  validator = true,
                                  name = <<"my-client">>,
                                  public_key = PubkeyData,
                                  pubkey_version = ?KEY_VERSION}, Client)

     end}
    ].

update_from_ejson_test_() ->
    chef_objects_test_utils:make_deprecated_tests(fun update_from_ejson_deprecated_tests/1) ++
    chef_objects_test_utils:make_non_deprecated_tests(fun update_from_ejson_tests/1) ++
    chef_objects_test_utils:make_all_versions_tests(fun update_from_ejson_common_tests/1).

update_from_ejson_tests(Version) ->
    [
     {?VD("update_from_ejson updates the fields but ignores admin and public_key"),
      fun() ->
              KeyData = public_key_data(),
              EJ = {[{<<"name">>, <<"a_name">>}, {<<"validator">>, true},
                     {<<"public_key">>, KeyData}, {<<"admin">>, true}]},
              OriginalClient = #chef_client{server_api_version = Version},
              NewClient = chef_client:update_from_ejson(OriginalClient, EJ),
              ?assertMatch(#chef_client{server_api_version = Version,
                                        name = <<"a_name">>,
                                        validator = true,
                                        admin = false, % we default false in the record
                                        pubkey_version = undefined,
                                        public_key = undefined}, NewClient)
      end}
    ].

update_from_ejson_common_tests(Version) ->
    [
     {?VD("update_from_ejson updates the fields when key is not set"),
      fun() ->
              EJ = {[{<<"name">>, <<"a_name">>}, {<<"validator">>, true}]},
              OriginalClient = #chef_client{server_api_version = Version},
              NewClient = chef_client:update_from_ejson(OriginalClient, EJ),
              ?assertMatch(#chef_client{server_api_version = Version,
                                        name = <<"a_name">>,
                                        validator = true}, NewClient)
      end}
     ].

update_from_ejson_deprecated_tests(Version) ->
    [
     {?VDD("update_from_ejson updates the fields when key is not set"),
      fun() ->
              EJ = {[{<<"name">>, <<"a_name">>}, {<<"validator">>, true}]},
              OriginalClient = #chef_client{server_api_version = Version},
              NewClient = chef_client:update_from_ejson(OriginalClient, EJ),
              ?assertMatch(#chef_client{server_api_version = Version,
                                        name = <<"a_name">>,
                                        validator = true}, NewClient)
      end},
     {?VDD("update_from_ejson updates the fields including public key when key is set"),
      fun() ->
              KeyData = public_key_data(),
              EJ = {[{<<"name">>, <<"a_name">>}, {<<"validator">>, true},
                     {<<"public_key">>, KeyData}]},
              OriginalClient = #chef_client{server_api_version = Version},
              NewClient = chef_client:update_from_ejson(OriginalClient, EJ),
              ?assertMatch(#chef_client{server_api_version = Version,
                                        name = <<"a_name">>,
                                        validator = true,
                                        pubkey_version = ?KEY_VERSION,
                                        public_key = KeyData}, NewClient)
      end}
    ].


%% remove trailing whitespace from a string
chomp(In) ->
    re:replace(In, "\\s+$", "", [{return, binary}]).

chomp_key_in_json_list(List) ->
    lists:map(fun({<<"public_key">>, KeyData}) ->
                      {<<"public_key">>, chomp(KeyData)};
                 (Other) ->
                      Other
              end, List).

%% the public key data here is derived from the certificate in
%% cert_data/1
public_key_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("spki_public.pem"),
    chomp(Bin).

cert_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("cert.pem"),
    chomp(Bin).
