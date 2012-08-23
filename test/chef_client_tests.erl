%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
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

-include_lib("eunit/include/eunit.hrl").

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

