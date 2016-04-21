%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
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


-module(chef_cookbook_version_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").
-include("chef_types.hrl").

-define(MAX_GOOD_VERSION, "9223372036854775807").
-define(MAX_GOOD_VERSION_INT, list_to_integer(?MAX_GOOD_VERSION)).
-define(BAD_POSITIVE_VERSION, "9223372036854775808").

basic_cookbook(Name, Version) ->
    basic_cookbook(Name, Version, []).

basic_cookbook(Name, Version, Options) ->
    %% Little helper function to cut down on verbosity
    Value = fun(Key, Default) ->
                    proplists:get_value(Key, Options, Default)
            end,
    {[
      {<<"name">>, <<Name/binary, "-", Version/binary>>},
      {<<"cookbook_name">>, Name},
      {<<"version">>, Version},
      {<<"chef_type">>, <<"cookbook_version">>},
      {<<"json_class">>, <<"Chef::CookbookVersion">>},
      {<<"frozen?">>, Value(frozen, false)},
      {<<"metadata">>, {[
                         {<<"version">>, Version},
                         {<<"name">>, Name},
                         {<<"dependencies">>, Value(dependencies, {[]})},
                         {<<"attributes">>, Value(attributes, {[]})},
                         {<<"long_description">>, Value(long_description, <<"">>)}
                        ]}}
     ]}.

minimal_cookbook_is_valid_test() ->
    CookbookEjson = basic_cookbook(<<"php">>, <<"1.2.3">>),
    Got = chef_cookbook_version:validate_cookbook(CookbookEjson, {<<"php">>, <<"1.2.3">>}),
    ?assertEqual({ok, CookbookEjson}, Got).

valid_resources_test() ->
    CB0 = basic_cookbook(<<"php">>, <<"1.2.3">>),
    NameVer = {<<"php">>, <<"1.2.3">>},
    CB = ej:set({<<"resources">>}, CB0,
                [{[
                   {<<"name">>, <<"a1">>},
                   {<<"path">>, <<"c/b/a1">>},
                   {<<"checksum">>, <<"abababababababababababababababab">>},
                   {<<"specificity">>, <<"default">>}
                  ]}]),
    ?assertEqual({ok, CB}, chef_cookbook_version:validate_cookbook(CB, NameVer)).

top_level_key_validation_test() ->
    Name = <<"test_name">>,
    Version = <<"1.2.3">>,
    Cookbook = basic_cookbook(Name, Version),
    [
     {"Cookbook with extra top-level key is invalid for create",
      fun() ->
              C = ej:set({<<"not_valid_key">>}, Cookbook, <<"some junk">>),
              ?assertThrow({invalid_key, <<"not_valid_key">>},
                           chef_cookbook_version:validate_role(C, {Name, Version}))
      end}
    ].

bad_resources_test_() ->
    CB0 = basic_cookbook(<<"php">>, <<"1.2.3">>),
    NameVer = {<<"php">>, <<"1.2.3">>},
    [
     {"resource value must be an array not an object",
      fun() ->
              CB = ej:set({<<"resources">>}, CB0, {[]}),
              ?assertThrow(#ej_invalid{type = json_type},
                           chef_cookbook_version:validate_cookbook(CB, NameVer))
      end},

     {"resource value must be an array not a string",
      fun() ->
              CB = ej:set({<<"resources">>}, CB0, <<"not-this">>),
              ?assertThrow(#ej_invalid{type = json_type},
                           chef_cookbook_version:validate_cookbook(CB, NameVer))
      end},

     {"resource array value must not be empty",
      fun() ->
              CB = ej:set({<<"resources">>}, CB0, [{[]}]),
              ?assertThrow(#ej_invalid{type = array_elt},
                           chef_cookbook_version:validate_cookbook(CB, NameVer))
      end}

    ].

valid_dependencies_test() ->
    CB0 = basic_cookbook(<<"php">>, <<"1.2.3">>),
    NameVer = {<<"php">>, <<"1.2.3">>},
    CB = ej:set({<<"metadata">>, <<"dependencies">>}, CB0,
                {[
                  {<<"apache2">>, <<"> 1.0.0">>},
                  {<<"apache3">>, <<">= 2.0.0">>},
                  {<<"crazy">>, <<"= 1.0">>},
                  {<<"aa">>, <<"~> 1.2.3">>},
                  {<<"bb">>, <<"< 1.2.3">>},
                  {<<"cc">>, <<"<= 1.2.3">>},
                  {<<"dd">>, <<"4.4.4">>}
                 ]}),
    ?assertEqual({ok, CB}, chef_cookbook_version:validate_cookbook(CB, NameVer)).

bad_dependencies_test_() ->
    CB0 = basic_cookbook(<<"php">>, <<"1.2.3">>),
    NameVer = {<<"php">>, <<"1.2.3">>},
    [
     {"cookbook name must be valid",
      fun() ->
              CB = ej:set({<<"metadata">>, <<"dependencies">>}, CB0,
                          {[
                            {<<"not valid name">>, <<"> 1.0.0">>}
                           ]}),
              ?assertThrow(#ej_invalid{type = object_key},
                           chef_cookbook_version:validate_cookbook(CB, NameVer))

      end},

     {"cookbook name must be valid",
      fun() ->
              CB = ej:set({<<"metadata">>, <<"dependencies">>}, CB0,
                          {[
                            {<<"apache2">>, <<"1b">>}
                           ]}),
              ?assertThrow(#ej_invalid{type = object_value},
                           chef_cookbook_version:validate_cookbook(CB, NameVer))

      end}

    ].

% http://tickets.chef.io/browse/CHEF-3976
providing_constraint_test_() ->
    CB0 = basic_cookbook(<<"php">>, <<"1.2.3">>),
    NameVer = {<<"php">>, <<"1.2.3">>},
    SetProviding = fun(Providing) ->
                           ej:set({<<"metadata">>, <<"providing">>}, CB0, Providing)
                   end,
    AllowedValues = [<<"cats::sleep">>,
                     <<"here(:kitty, :time_to_eat)">>,
                     <<"service[snuggle]">>,
                     <<"">>,
                     1,
                     true,
                     null,
                     [<<"arrays allowed">>],
                     {[{<<"key">>, <<"objects_allowed">>}]}],
    %% bad values tests skipped (providing should accept anything)
    %% allowed value tests
    [
      fun() ->
              CB = SetProviding(P),
              ?assertEqual({ok, CB}, chef_cookbook_version:validate_cookbook(CB, NameVer))
      end || P <- AllowedValues
    ].

assemble_cookbook_ejson_test_() ->
    VHostUrl = "https://api.example.org",
    CBEJson = basic_cookbook(<<"php">>,
                             <<"1.2.3">>,
                             [
                              {long_description, <<"Behold, this is a long description!  And my, what a loooooooooooooooong description it is!">>},
                              {dependencies, {[{<<"ruby">>, []}]}}
                             ]),
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
    [
     {"basic rehydration test",
      fun() ->
              OrgId = <<"12341234123412341234123412341234">>,
              AuthzId = <<"auth">>,
              Record = chef_object:new_record(chef_cookbook_version,
                                              ?API_MIN_VER,
                                              OrgId,
                                              AuthzId,
                                              CBEJson),

              chef_test_utility:ejson_match(CBEJson,
                                            chef_cookbook_version:assemble_cookbook_ejson(Record, VHostUrl))

      end},

     {"minimal rehydration test",
      fun() ->
              OrgId = <<"12341234123412341234123412341234">>,
              AuthzId = <<"auth">>,
              Record = chef_object:new_record(chef_cookbook_version,
                                              ?API_MIN_VER,
                                              OrgId,
                                              AuthzId,
                                              CBEJson),

              MinCb = chef_cookbook_version:minimal_cookbook_ejson(Record, VHostUrl),
              ?assertEqual(undefined, ej:get({"metadata", "attributes"}, MinCb)),
              ?assertEqual(undefined, ej:get({"metadata", "long_description"}, MinCb)),
              ?assertEqual({[{<<"ruby">>, []}]}, ej:get({"metadata", "dependencies"}, MinCb))
      end}
    ]}.

version_to_binary_test() ->
    ?assertEqual(<<"1.2.3">>, chef_cookbook_version:version_to_binary({1,2,3})),
    ?assertEqual(<<"0.0.1">>, chef_cookbook_version:version_to_binary({0,0,1})).

parse_version_test_() ->
    GoodVersions = [{<<"1.2">>, {1, 2, 0}},
                    {<<"1.2.3">>, {1, 2, 3}},
                    {<<"1.2.", ?MAX_GOOD_VERSION>>, {1,2, ?MAX_GOOD_VERSION_INT}},
                    {<<"1.", ?MAX_GOOD_VERSION>>, {1, ?MAX_GOOD_VERSION_INT, 0}},
                    {<<?MAX_GOOD_VERSION, ".0">>, {?MAX_GOOD_VERSION_INT, 0, 0}},
                    {<<"0.",?MAX_GOOD_VERSION,".0">>, {0, ?MAX_GOOD_VERSION_INT, 0}},
                    {<<"0.0.0">>, {0, 0, 0}},
                    {<<"123.456.789">>, {123, 456, 789}}],
    [ ?_assertEqual(Expect, chef_cookbook_version:parse_version(In)) ||
        {In, Expect} <- GoodVersions ].

parse_version_badversion_test_() ->
    BadVersions = [
                   <<"0">>,
                   <<"1">>,
                   <<?MAX_GOOD_VERSION>>,
                   <<"">>,
                   <<"A">>,
                   <<"1.2.a">>,
                   <<"1.2.0.0">>,
                   <<"-3">>,
                   <<"1.2.-1">>,
                   <<"1.-2.1">>,
                   <<"-1.2.1">>,
                   << ?BAD_POSITIVE_VERSION, ".2.1">>,
                   <<"1.",?BAD_POSITIVE_VERSION,".1">>,
                   <<"1.2.",?BAD_POSITIVE_VERSION >>
                   %% we store maj, min, pat in pg int field in the db, so we should enforce
                   %% the max value and avoid wrapping.
                  ],
    [ ?_assertError(badarg, chef_cookbook_version:parse_version(V)) || V <- BadVersions ].

dependencies_to_depsolver_constraints_test_() ->
    {foreachx,
     fun({Terms, _Expected}) ->
             jiffy:encode({Terms})
     end,
     fun(_, _) ->
             ok
     end,
     [ {{XTerms, XExpected},
        fun({_Terms, Expected}, JSON) ->
                {Description,
                 fun() ->
                         Actual = chef_object_base:depsolver_constraints(JSON),
                         ?assertEqual(Expected, Actual)
                 end}
        end}
       || {Description, XTerms, XExpected} <- [
                                               {"No dependency information", [],[]},

                                               {"One dependency",
                                                [{<<"apache">>, <<"> 1.0.0">>}],
                                                [{<<"apache">>, <<"1.0.0">>, '>'}]},

                                               {"Many dependencies",
                                                [
                                                 {<<"apache">>, <<"> 1.0.0">>},
                                                 {<<"mysql">>, <<"= 5.0.0">>},
                                                 {<<"ultra_fantastic_awesome_sauce">>, <<"= 6.6.6">>}
                                                ],
                                                [
                                                 {<<"apache">>, <<"1.0.0">>, '>'},
                                                 {<<"mysql">>, <<"5.0.0">>, '='},
                                                 {<<"ultra_fantastic_awesome_sauce">>, <<"6.6.6">>, '='}
                                                ]
                                               },

                                               {"All kinds of dependencies and constraint combinations",
                                                [
                                                 {<<"apache1">>, <<"> 1.0.0">>},
                                                 {<<"apache2">>, <<"> 1.0">>},
                                                 {<<"apache3">>, <<"> 1">>},

                                                 {<<"mysql1">>, <<"= 1.0.0">>},
                                                 {<<"mysql2">>, <<"= 1.0">>},
                                                 {<<"mysql3">>, <<"= 1">>},

                                                 {<<"nginx1">>, <<"< 1.0.0">>},
                                                 {<<"nginx2">>, <<"< 1.0">>},
                                                 {<<"nginx3">>, <<"< 1">>},

                                                 {<<"php1">>, <<"<= 1.0.0">>},
                                                 {<<"php2">>, <<"<= 1.0">>},
                                                 {<<"php3">>, <<"<= 1">>},

                                                 {<<"nagios1">>, <<">= 1.0.0">>},
                                                 {<<"nagios2">>, <<">= 1.0">>},
                                                 {<<"nagios3">>, <<">= 1">>},

                                                 {<<"ultra_fantastic_awesome_sauce1">>, <<"~> 1.0.0">>},
                                                 {<<"ultra_fantastic_awesome_sauce2">>, <<"~> 1.0">>},
                                                 {<<"ultra_fantastic_awesome_sauce3">>, <<"~> 1">>},

                                                 {<<"monkey_patches1">>, <<"1.0.0">>},
                                                 {<<"monkey_patches2">>, <<"1.0">>},
                                                 {<<"monkey_patches3">>, <<"1">>}
                                                ],
                                                [
                                                 {<<"apache1">>, <<"1.0.0">>, '>'},
                                                 {<<"apache2">>, <<"1.0">>, '>'},
                                                 {<<"apache3">>, <<"1">>, '>'},

                                                 {<<"mysql1">>, <<"1.0.0">>, '='},
                                                 {<<"mysql2">>, <<"1.0">>, '='},
                                                 {<<"mysql3">>, <<"1">>, '='},

                                                 {<<"nginx1">>, <<"1.0.0">>, '<'},
                                                 {<<"nginx2">>, <<"1.0">>, '<'},
                                                 {<<"nginx3">>, <<"1">>, '<'},

                                                 {<<"php1">>, <<"1.0.0">>, '<='},
                                                 {<<"php2">>, <<"1.0">>, '<='},
                                                 {<<"php3">>, <<"1">>, '<='},

                                                 {<<"nagios1">>, <<"1.0.0">>, '>='},
                                                 {<<"nagios2">>, <<"1.0">>, '>='},
                                                 {<<"nagios3">>, <<"1">>, '>='},

                                                 {<<"ultra_fantastic_awesome_sauce1">>, <<"1.0.0">>, '~>'},
                                                 {<<"ultra_fantastic_awesome_sauce2">>, <<"1.0">>, '~>'},
                                                 {<<"ultra_fantastic_awesome_sauce3">>, <<"1">>, '~>'},

                                                 {<<"monkey_patches1">>, <<"1.0.0">>, '='},
                                                 {<<"monkey_patches2">>, <<"1.0">>, '='},
                                                 {<<"monkey_patches3">>, <<"1">>, '='}

                                                ]
                                               }
                                              ]
     ]
    }.


recipe_name_test_() ->
    [{"Appropriately removes a '.rb' extension (normal, everyday case)",
      ?_assertEqual(<<"foo">>,
                    chef_cookbook_version:recipe_name(<<"foo.rb">>))},
     {"Behaves if recipes don't end in '.rb' (shouldn't happen in practice, though)",
      ?_assertEqual(<<"foo">>,
                    chef_cookbook_version:recipe_name(<<"foo">>))},
     {"Can still produce bizarre recipe names, if given a bizarre recipe name",
      ?_assertEqual(<<"foo.rb">>,
                    chef_cookbook_version:recipe_name(<<"foo.rb.rb">>))}].

maybe_qualify_name_test_() ->
    [{"Qualifies a 'normal' recipe name",
      ?_assertEqual(<<"cookbook::recipe">>,
                    chef_cookbook_version:maybe_qualify_name(<<"cookbook">>, <<"recipe.rb">>))},
     {"Qualifies a recipe name even without a '.rb' suffix (shouldn't happen in practice, though)",
      ?_assertEqual(<<"cookbook::recipe">>,
                    chef_cookbook_version:maybe_qualify_name(<<"cookbook">>, <<"recipe">>))},
     {"Does NOT qualifies a 'default' recipe; uses just the cookbook name instead",
      ?_assertEqual(<<"cookbook">>,
                    chef_cookbook_version:maybe_qualify_name(<<"cookbook">>, <<"default.rb">>))},
     {"Does NOT qualifies a 'default' recipe, even if it doesn't end in '.rb'; uses just the cookbook name instead",
      ?_assertEqual(<<"cookbook">>,
                    chef_cookbook_version:maybe_qualify_name(<<"cookbook">>, <<"default">>))},
     %% Not really expecting this next one to happen, just documenting the behavior
     {"Does NOT treat a recipe as the default if it is not named either 'default.rb' or just 'default'",
      ?_assertEqual(<<"cookbook::default.rb">>,
                    chef_cookbook_version:maybe_qualify_name(<<"cookbook">>, <<"default.rb.rb">>))}
    ].

base_cookbook_name_test_() ->
    Tests = [
             %% {Input, Expected}
             {<<"apache2::default">>, <<"apache2">>},
             {"apache2::default",     <<"apache2">>},
             {<<"apache2">>,          <<"apache2">>},
             {"apache2",              <<"apache2">>}
            ],
    [ ?_assertEqual(Expected, chef_cookbook_version:base_cookbook_name(Input))
      || {Input, Expected} <- Tests ].

new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"00000000000000000000000011111111">>,
    CBVData = example_cookbook_version_json(),
    CBV = chef_cookbook_version:new_record(?API_MIN_VER, OrgId, AuthzId, CBVData),
    ?assertMatch(#chef_cookbook_version{}, CBV),
    %% TODO: validate more fields?
    ?assertEqual(<<"apache2">>, chef_cookbook_version:name(CBV)).

example_cookbook_version_json() ->
    Name = <<"apache2">>,
    Version = <<"1.2.3">>,
    {[
      {<<"name">>, <<Name/binary, "-", Version/binary>>},
      {<<"cookbook_name">>,  Name},
      {<<"json_class">>, <<"Chef::CookbookVersion">>},
      {<<"chef_type">>, <<"cookbook_version">>},
      {<<"metadata">>,  {[
                          {<<"description">>,  <<"A fabulous new cookbook">>},
                          {<<"long_description">>,  <<"">>},
                          {<<"maintainer">>,  <<"Your Name">>},
                          {<<"maintainer_email">>,  <<"youremail@example.com">>},
                          {<<"license">>,  <<"Apache v2.0">>},
                          {<<"platforms">>,  {[]}},
                          {<<"dependencies">>,  {[{<<"foo">>, <<"> 1.0">>}]}},
                          {<<"recommendations">>,  {[]}},
                          {<<"suggestions">>,  {[]}},
                          {<<"conflicting">>,  {[]}},
                          {<<"providing">>,  {[]}},
                          {<<"replacing">>,  {[]}},
                          {<<"attributes">>,  {[]}},
                          {<<"groupings">>,  {[]}},
                          {<<"recipes">>,  {[]}},
                          {<<"version">>,  Version}
                         ]}}
     ]}.

id_test() ->
    ?assertEqual(<<"1">>, chef_object:id(#chef_cookbook_version{id = <<"1">>})).

name_test() ->
    ?assertEqual(<<"a_name">>,
                  chef_object:name(#chef_cookbook_version{name =  <<"a_name">>})).
