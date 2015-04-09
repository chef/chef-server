%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
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


-module(chef_client_tests).
-include("../../include/chef_types.hrl").
-include("../../include/chef_osc_defaults.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    {ok, Bin} = file:read_file("../test/spki_public.pem"),
    chomp(Bin).

cert_data() ->
    {ok, Bin} = file:read_file("../test/cert.pem"),
    chomp(Bin).

assemble_client_ejson_test_() ->
    [{"obtain expected EJSON",
      fun() ->
              Client = #chef_client{name = <<"alice">>,
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
     {"converts certificate to public key",
      fun() ->
              Client = #chef_client{name = <<"alice">>,
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



parse_binary_json_test_() ->
    [{"Error thrown on mismatched names",
      fun() ->
          Body = <<"{\"name\":\"name\",\"clientname\":\"notname\"}">>,
          ?assertThrow({client_name_mismatch}, chef_client:parse_binary_json(Body, <<"name">>))
      end
     },
     {"Can create with only name",
      fun() ->
          Body = <<"{\"name\":\"name\"}">>,
          {ok, Client} = chef_client:parse_binary_json(Body, <<"name">>),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end
     },
     {"Can create with only clientname",
      fun() ->
          Body = <<"{\"clientname\":\"name\"}">>,
          {ok, Client} = chef_client:parse_binary_json(Body, <<"name">>),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end
     },
     {"Error thrown with no name or clientname",
      fun() ->
          Body = <<"{\"validator\":false}">>,
          ?assertThrow({both_missing, <<"name">>, <<"clientname">>},
                       chef_client:parse_binary_json(Body, undefined))
      end
     },
     {"Error thrown with bad name",
      fun() ->
          Body = <<"{\"name\":\"bad~name\"}">>,
          ?assertThrow({bad_client_name, <<"bad~name">>,
                        <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>},
                       chef_client:parse_binary_json(Body, <<"bad~name">>))
      end
     }
    ].


new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"00000000000000000000000011111111">>,
    ClientData = {[{<<"name">>, <<"my-client">>}, {<<"alpha">>, <<"bravo">>}]},
    Client = chef_client:new_record(?API_MIN_VER, OrgId, AuthzId, ClientData),
    ?assertMatch(#chef_client{}, Client),
    %% TODO: validate more fields?
    ?assertEqual(<<"my-client">>, chef_client:name(Client)).
