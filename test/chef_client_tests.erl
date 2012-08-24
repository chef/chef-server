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

-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-include_lib("eunit/include/eunit.hrl").

osc_assemble_client_ejson_test_() ->
    [{"obtain expected EJSON",
      fun() ->
              Client = #chef_client{name = <<"alice">>,
                                    admin = true,
                                    validator = false,
                                    public_key = <<"PUBKEY">>},
              {GotList} = chef_client:osc_assemble_client_ejson(Client, ?OSC_ORG_NAME),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"public_key">>, <<"PUBKEY">>},
                              {<<"validator">>, false},
                              {<<"admin">>, true},
                              {<<"name">>, <<"alice">>}],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end},

     {"sets defaults if 'undefined' is encountered",
      fun() ->
              Client = #chef_client{},
              {GotList} = chef_client:osc_assemble_client_ejson(Client, ?OSC_ORG_NAME),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"validator">>, false},
                              {<<"admin">>, false},
                              {<<"name">>, <<"">>}],
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotList))
      end
      }
    ].

osc_parse_binary_json_test_() ->
    [{"Can create with only URL name and has default values",
      fun() ->
              {ok, Client} = chef_client:osc_parse_binary_json(<<"{}">>, <<"alice">>),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"validator">>, false},
                              {<<"admin">>, false},
                              {<<"name">>, <<"alice">>}],
              {GotData} = Client,
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotData))
      end
     },

     {"Error thrown when missing both name and URL name",
      fun() ->
              Body = <<"{\"validator\":false}">>,
              ?assertThrow({both_missing, <<"name">>, <<"URL-NAME">>},
                           chef_client:osc_parse_binary_json(Body, undefined))
      end
     },

     {"Error thrown with bad name",
      fun() ->
              Body = <<"{\"name\":\"bad~name\"}">>,
              ?assertThrow({bad_client_name, <<"bad~name">>,
                            <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>},
                           chef_client:osc_parse_binary_json(Body, <<"bad~name">>))
      end
     },

     {"Inherits values from current client",
      fun() ->
              CurClient = #chef_client{admin = true, validator = true},
              {ok, Client} = chef_client:osc_parse_binary_json(<<"{}">>, <<"alice">>, CurClient),
              ExpectedData = [{<<"json_class">>, <<"Chef::ApiClient">>},
                              {<<"chef_type">>, <<"client">>},
                              {<<"validator">>, true},
                              {<<"admin">>, true},
                              {<<"name">>, <<"alice">>}],
              {GotData} = Client,
              ?assertEqual(lists:sort(ExpectedData), lists:sort(GotData))
              
      end
     },

     {"Inherits values from current client but can override true to false",
      fun() ->
              %% override true with false
              CurClient = #chef_client{admin = true, validator = true},
              {ok, Client} = chef_client:osc_parse_binary_json(<<"{\"validator\":false, \"admin\":false}">>,
                                                           <<"alice">>, CurClient),
              ?assertEqual(false, ej:get({"admin"}, Client)),
              ?assertEqual(false, ej:get({"validator"}, Client))
      end
     },

     {"Inherits values from current client but can override false to true",
       fun() ->
               %% override false with true
               CurClient = #chef_client{admin = false, validator = false},
               {ok, Client} = chef_client:osc_parse_binary_json(<<"{\"validator\":true, \"admin\":true}">>,
                                                            <<"alice">>, CurClient),
               ?assertEqual(true, ej:get({"admin"}, Client)),
               ?assertEqual(true, ej:get({"validator"}, Client))
       end
     }
    ].


oc_parse_binary_json_test_() ->
    [{"Error thrown on mismatched names",
      fun() ->
          Body = <<"{\"name\":\"name\",\"clientname\":\"notname\"}">>,
          ?assertThrow({client_name_mismatch}, chef_client:oc_parse_binary_json(Body, <<"name">>))
      end
     },
     {"Can create with only name",
      fun() ->
          Body = <<"{\"name\":\"name\"}">>,
          {ok, Client} = chef_client:oc_parse_binary_json(Body, <<"name">>),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end
     },
     {"Can create with only clientname",
      fun() ->
          Body = <<"{\"clientname\":\"name\"}">>,
          {ok, Client} = chef_client:oc_parse_binary_json(Body, <<"name">>),
          Name = ej:get({<<"name">>}, Client),
          ClientName = ej:get({<<"clientname">>}, Client),
          ?assertEqual(Name, ClientName)
      end
     },
     {"Error thrown with no name or clientname",
      fun() ->
          Body = <<"{\"validator\":false}">>,
          ?assertThrow({both_missing, <<"name">>, <<"clientname">>},
                       chef_client:oc_parse_binary_json(Body, undefined))
      end
     },
     {"Error thrown with bad name",
      fun() ->
          Body = <<"{\"name\":\"bad~name\"}">>,
          ?assertThrow({bad_client_name, <<"bad~name">>,
                        <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>},
                       chef_client:oc_parse_binary_json(Body, <<"bad~name">>))
      end
     }
    ].
