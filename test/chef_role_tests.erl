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


-module(chef_role_tests).

-include_lib("eunit/include/eunit.hrl").

basic_role() ->
    {[
      {<<"name">>, <<"test_role">>},
      {<<"description">>, <<"This is a testing role">>},
      {<<"json_class">>, <<"Chef::Role">>},
      {<<"chef_type">>, <<"role">>},
      {<<"default_attributes">>, {[]}},
      {<<"override_attributes">>, {[]}},
      {<<"run_list">>, []}
     ]}.

validate_role_test_() ->
    [
     {"Validate that a role with a missing name is rejected",
      fun() ->
              R = ej:delete({<<"name">>}, basic_role()),
              ?assertThrow({missing, <<"name">>},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a chef_type not equal to 'role' is rejected",
      fun() ->
              R = ej:set({<<"chef_type">>}, basic_role(), <<"BLAHBLAH">>),
              ?assertThrow({mismatch, {<<"chef_type">>, "role", <<"BLAHBLAH">>}},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a json_class not equal to 'Chef::Role' is rejected",
      fun() ->
              R = ej:set({<<"json_class">>}, basic_role(), <<"BLAHBLAH">>),
              ?assertThrow({mismatch, {<<"json_class">>, "Chef::Role", <<"BLAHBLAH">>}},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a bogus run list is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, basic_role(),
                         [<<"recipe[foo]">>, <<"fake[not_good]">>]),
              ?assertThrow({bad_run_list, _},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a bogus env_run_lists entry is rejected",
      fun () ->
              R = ej:set({<<"env_run_lists">>}, basic_role(),
                         {[{<<"preprod">>, [<<"recipe[foo]">>]},
                           {<<"prod">>, [<<"fake[not_good]">>]}]}),
              ?assertThrow({bad_run_lists, _},
                           chef_role:validate_role(R, create))
      end}
    ].

set_default_values_test_() ->
    [
     {"Validate that a role with a missing attributes get appropriate defaults set",
      fun() ->
              WithDefaults = chef_role:set_default_values(basic_role()),
              ?assertEqual(ej:get({<<"json_class">>}, WithDefaults),
                           <<"Chef::Role">>),
              ?assertEqual(ej:get({<<"chef_type">>}, WithDefaults),
                           <<"role">>),
              ?assertEqual(ej:get({<<"default_attributes">>}, WithDefaults),
                           {[]}),
              ?assertEqual(ej:get({<<"override_attributes">>}, WithDefaults),
                           {[]})
      end},
     {"Validate that fields with existing values are not overwritten by defaults",
      fun() ->
              Role = {[{<<"name">>, <<"test_role">>},
                       %% not bothering to check on json_class or chef_type, since those can only have one value
                       {<<"default_attributes">>, {[{<<"awesomeness">>, <<"considerable">>}]}},
                       {<<"override_attributes">>, {[{<<"awesomeness">>, <<"considerable">>}]}}
                      ]},
              WithDefaults = chef_role:set_default_values(Role),
              ?assertEqual(ej:get({<<"default_attributes">>}, WithDefaults),
                           {[{<<"awesomeness">>, <<"considerable">>}]}),
              ?assertEqual(ej:get({<<"override_attributes">>}, WithDefaults),
                           {[{<<"awesomeness">>, <<"considerable">>}]})
      end}
    ].
