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

-module(chef_node_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").

extended_node() ->
    {[{<<"name">>, <<"a_node">>},
      {<<"chef_environment">>, <<"prod">>},
      {<<"json_class">>, <<"Chef::Node">>},
      {<<"chef_type">>, <<"node">>},
      {<<"run_list">>, [<<"recipe[web]">>, <<"role[prod]">>]},
      {<<"normal">>, {[]}},
      {<<"override">>, {[]}},
      {<<"automatic">>, {[]}},
      {<<"default">>, {[]}}
     ]}.

run_list_extraction_test_() ->
    RunList = [ list_to_binary(X) ||
                  X <- ["recipe[ssh]", "role[opscode-account]",
                        "role[opscode-support]", "recipe[opscode-github]",
                        "role[rs-prod]", "recipe[users::sysadmins]",
                        "recipe[users::system]", "recipe[ntp]"] ],
    Roles = [<<"opscode-account">>, <<"opscode-support">>, <<"rs-prod">>],
    Recipes = [<<"ssh">>, <<"opscode-github">>, <<"users::sysadmins">>,
               <<"users::system">>, <<"ntp">>],
    [?_assertEqual([], chef_node:extract_roles([])),
     ?_assertEqual([], chef_node:extract_recipes([])),
     ?_assertEqual(Roles, chef_node:extract_roles(RunList)),
     ?_assertEqual(Recipes, chef_node:extract_recipes(RunList))].

validate_json_node_test_() ->
    [{"check that a valid node is accepted for create",
      fun() ->
          N = extended_node(),
          ?assertEqual({ok, N}, chef_node:validate_json_node(N, create))
      end},
     {"check that a valid node is accepted for update",
      fun() ->
          N = extended_node(),
          ?assertEqual({ok, N}, chef_node:validate_json_node(N, {update, <<"a_node">>}))
      end},
     {"check that a node with a missing name is rejected",
      fun() ->
          N = extended_node(),
          BadN = ej:delete({<<"name">>}, N),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with a name mismatch is rejected for update",
      fun() ->
          N = extended_node(),
          ?assertThrow({url_json_name_mismatch, {<<"b_node">>, <<"a_node">>, "Node"}},
                       chef_node:validate_json_node(N, {update, <<"b_node">>}))
      end},
     {"check that a node with missing name in json is accepted for update",
      fun() ->
          N = ej:delete({"name"}, extended_node()),
          ?assertEqual({ok, ej:set({"name"}, N, <<"a_node">>)},
                       chef_node:validate_json_node(N, {update, <<"a_node">>}))
      end},
     {"check that run_list is required for create and update",
      fun() ->
          N = ej:delete({"run_list"}, extended_node()),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(N, create)),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(N, {update, <<"a_node">>}))
      end},
     {"check that a node with an invalid name is rejected (1)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"name">>}, N, <<"~dog">>),
              ?assertThrow(#ej_invalid{},
                      chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with an invalid name is rejected (2)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"name">>}, N, <<"~dog">>),
              ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with an invalid name is rejected (3)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"name">>}, N, <<"~dog">>),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with an invalid name is rejected (4)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"name">>}, N, <<"~dog">>),
              ?assertThrow(#ej_invalid{},
                           chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with an correct json class is accepted)",
      fun() ->
          N = extended_node(),
          GoodN = ej:set({<<"json_class">>}, N, <<"Chef::Node">>),
          ?assertEqual({ok, GoodN}, chef_node:validate_json_node(GoodN, create))
      end},
     {"check that a node with an bad json class is rejected)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"json_class">>}, N, <<"Chef::Role">>),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(BadN, create))
      end},
     {"check that a node with an bad json proplist in the normal field is rejected)",
      fun() ->
          N = extended_node(),
          BadN = ej:set({<<"json_class">>}, N, []),
          ?assertThrow(#ej_invalid{},
                       chef_node:validate_json_node(BadN, create))
      end},
     {"check that a bogus run list is rejected",
      fun() ->
              N = extended_node(),
              BadN = ej:set({<<"run_list">>},
                            N,
                            [<<"recipe[foo]">>, <<"fake[not_good]">>]),
              ?assertThrow(#ej_invalid{},
                           chef_node:validate_json_node(BadN, create))
      end}].

insert_autofill_fields_test_() ->
    [{"check that missing fields are inserted",
     fun() ->
             N =  {[
                    {<<"name">>, <<"a_node">>}
                   ]},
             N2 = chef_node:insert_autofill_fields(N),
             ?assertEqual(<<"_default">>, ej:get({<<"chef_environment">>}, N2)),
             ?assertEqual({[]}, ej:get({<<"override">>}, N2)),
             ?assertEqual([], ej:get({<<"run_list">>}, N2))
     end
     }].

mk_dummy_node(L, 0) -> L;
mk_dummy_node(L, C) -> mk_dummy_node(["deadbeef"|L], C-1).

parse_check_binary_as_json_node_test_() ->
    [{"check that a valid node is accepted",
      fun() ->
          N = extended_node(),
          NB = jiffy:encode(N),
          chef_node:parse_check_binary_as_json_node(NB, create),
          chef_node:parse_check_binary_as_json_node(NB, {update, <<"a_node">>})
      end},
     {"check that invalid json is rejected",
      fun() ->
          NB = mk_dummy_node([], 200),
          ?assertThrow({error, _}, chef_node:parse_check_binary_as_json_node(NB, create)),
          ?assertThrow({error, _},
                       chef_node:parse_check_binary_as_json_node(NB, {update, <<"a_node">>}))
      end},
     {"check that a node with an invalid name is rejected (1)",
      fun() ->
              N = extended_node(),
              BadN = ej:set({<<"name">>}, N, <<"~dog">>),
              NB = jiffy:encode(BadN),
              ?assertThrow(#ej_invalid{},
                           chef_node:parse_check_binary_as_json_node(NB, create)),
              ?assertThrow({url_json_name_mismatch, _},
                           chef_node:parse_check_binary_as_json_node(NB, {update, <<"a_node">>}))
      end}
    ].
