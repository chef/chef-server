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
-include_lib("ej/include/ej.hrl").

basic_role() ->
    {[
      {<<"name">>, <<"test_role">>},
      {<<"description">>, <<"This is a testing role">>},
      {<<"json_class">>, <<"Chef::Role">>},
      {<<"chef_type">>, <<"role">>},
      {<<"default_attributes">>, {[]}},
      {<<"override_attributes">>, {[]}},
      {<<"run_list">>, []},
      {<<"env_run_lists">>, {[]}}
     ]}.


role_environments_test_() ->
    [{"empty env_run_lists",
      ?_assertEqual([<<"_default">>], chef_role:environments(basic_role()))},

     {"non-empty env_run_lists",
      fun() ->
              Role = ej:set({<<"env_run_lists">>}, basic_role(),
                            {[{<<"e2">>, {[]}}, {<<"e1">>, {[]}}]}),
              ?assertEqual([<<"_default">>, <<"e1">>, <<"e2">>],
                           chef_role:environments(Role))
      end},

     {"_default does not get duplicated",
      fun() ->
              Role = ej:set({<<"env_run_lists">>}, basic_role(),
                            {[{<<"_default">>, {[]}}, {<<"e1">>, {[]}}]}),
              ?assertEqual([<<"_default">>, <<"e1">>],
                           chef_role:environments(Role))
      end}
    ].

validate_role_test_() ->
    [
     {"Validate that a role with a missing name is rejected",
      fun() ->
              R = ej:delete({<<"name">>}, basic_role()),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a chef_type not equal to 'role' is rejected",
      fun() ->
              R = ej:set({<<"chef_type">>}, basic_role(), <<"BLAHBLAH">>),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a json_class not equal to 'Chef::Role' is rejected",
      fun() ->
              R = ej:set({<<"json_class">>}, basic_role(), <<"BLAHBLAH">>),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a default_attributes that is not a proplist is rejected",
      fun() ->
              R = ej:set({<<"default_attributes">>}, basic_role(), <<"BLAHBLAH">>),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},

     {"Validate that a bogus run list is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, basic_role(),
                         [<<"recipe[foo]">>, <<"fake[not_good]">>]),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},
     {"Validate that a bogus env_run_lists entry is rejected",
      fun () ->
              R = ej:set({<<"env_run_lists">>}, basic_role(),
                         {[{<<"preprod">>, [<<"recipe[foo]">>]},
                           {<<"prod">>, [<<"fake[not_good]">>]}]}),
              ?assertThrow(#ej_invalid{},
                           chef_role:validate_role(R, create))
      end},
     {"Role with extra top-level key is invalid for create",
      fun() ->
              R = ej:set({<<"not_valid_key">>}, basic_role(), <<"some junk">>),
              ?assertThrow({invalid_key, <<"not_valid_key">>},
                          chef_role:validate_role(R, create))
      end},
     {"Role with extra top-level key is invalid for update",
      fun() ->
              R = ej:set({<<"not_valid_key">>}, basic_role(), <<"some junk">>),
              ?assertThrow({invalid_key, <<"not_valid_key">>},
                           chef_role:validate_role(R,{update, ej:get({<<"name">>}, R)}))
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

parse_binary_json_test_() ->
    [{"Ensure that all variants of recipes in a run list are properly normalized",
      fun() ->
              R = basic_role(),
              RunList = [<<"foo">>,
                         <<"bar::default">>,
                         <<"baz::quux@1.0.0">>,
                         <<"recipe[web]">>,
                         <<"role[prod]">>],
              WithRunList = ej:set({<<"run_list">>}, R, RunList),
              JSON = jiffy:encode(WithRunList),

              {ok, Processed} = chef_role:parse_binary_json(JSON, create),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"run_list">>}, Processed)),

              {ok, ProcessedForUpdate} = chef_role:parse_binary_json(JSON, {update, ej:get({<<"name">>}, R)}),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"run_list">>}, ProcessedForUpdate))

      end
     },
     {"Ensure that all variants of recipes in environment run lists are properly normalized",
      fun() ->
              R = basic_role(),
              RunLists ={[{<<"prod">>, [<<"foo">>,
                                        <<"bar::default">>,
                                        <<"baz::quux@1.0.0">>,
                                        <<"recipe[web]">>,
                                        <<"role[prod]">>]},
                          {<<"dev">>, [<<"foo">>, <<"bar">>]}]},
              WithRunLists = ej:set({<<"env_run_lists">>}, R, RunLists),
              JSON = jiffy:encode(WithRunLists),

              %% Test for create
              {ok, Processed} = chef_role:parse_binary_json(JSON, create),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"env_run_lists">>, <<"prod">>}, Processed)),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar]">>],
                           ej:get({<<"env_run_lists">>, <<"dev">>}, Processed)),

              %% Test for update
              {ok, ProcessedForUpdate} = chef_role:parse_binary_json(JSON, {update, ej:get({<<"name">>}, R)}),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"env_run_lists">>, <<"prod">>}, ProcessedForUpdate)),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar]">>],
                           ej:get({<<"env_run_lists">>, <<"dev">>}, ProcessedForUpdate))
      end
     }
    ].

normalize_test_() ->
    [{Message,
      fun() ->
              ?assertEqual(Normalized,
                           chef_role:normalize(Input))
      end}
    || {Message, Input, Normalized} <- [
                                        {"Normalizes a role's run list",
                                         ej:set({<<"run_list">>}, basic_role(), [<<"foo">>, <<"bar">>, <<"baz">>]),
                                         ej:set({<<"run_list">>}, basic_role(), [<<"recipe[foo]">>, <<"recipe[bar]">>, <<"recipe[baz]">>])},
                                        {"Normalizes a role's environment run lists",
                                         ej:set({<<"env_run_lists">>},
                                                basic_role(),
                                                {[{<<"prod">>, [<<"foo">>, <<"bar">>, <<"baz">>]},
                                                  {<<"dev">>, [<<"oof">>, <<"rab">>, <<"zab">>]}]}),
                                         ej:set({<<"env_run_lists">>},
                                                basic_role(),
                                                {[{<<"prod">>, [<<"recipe[foo]">>, <<"recipe[bar]">>, <<"recipe[baz]">>]},
                                                  {<<"dev">>, [<<"recipe[oof]">>, <<"recipe[rab]">>, <<"recipe[zab]">>]}]})}
                                       ]
    ].
