%% Copyright 2012-2013-2018 Chef Software, Inc. All Rights Reserved.
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
-include("chef_types.hrl").

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
      end},
     {"Ensure that all variants of recipes in a run list are properly normalized",
      fun() ->
              N = extended_node(),
              RunList = [<<"foo">>,
                         <<"bar::default">>,
                         <<"baz::quux@1.0.0">>,
                         <<"recipe[web]">>,
                         <<"role[prod]">>],
              WithRunList = ej:set({<<"run_list">>}, N, RunList),
              JSON = jiffy:encode(WithRunList),

              %% Test for create
              {ok, Processed} = chef_node:parse_check_binary_as_json_node(JSON, create),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"run_list">>}, Processed)),

              %% Test for update
              {ok, ProcessedForUpdate} = chef_node:parse_check_binary_as_json_node(JSON, {update, ej:get({<<"name">>}, N)}),
              ?assertEqual([<<"recipe[foo]">>, <<"recipe[bar::default]">>, <<"recipe[baz::quux@1.0.0]">>, <<"recipe[web]">>, <<"role[prod]">>],
                           ej:get({<<"run_list">>}, ProcessedForUpdate))

      end
     }
    ].

normalize_test_() ->
    [{"Normalizes a node's run list",
      fun() ->
              Input = ej:set({<<"run_list">>}, extended_node(),
                             [<<"foo">>, <<"bar">>, <<"baz">>]),
              Normalized = ej:set({<<"run_list">>}, extended_node(),
                                  [<<"recipe[foo]">>, <<"recipe[bar]">>, <<"recipe[baz]">>]),
              ?assertEqual(Normalized,
                           chef_node:normalize(Input))
      end}].

new_record_test() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"00000000000000000000000011111111">>,
    NodeData = {[{<<"name">>, <<"my-node">>}, {<<"alpha">>, <<"bravo">>}]},
    Node = chef_node:new_record(OrgId, AuthzId, NodeData),
    ?assertMatch(#chef_node{}, Node),
    %% TODO: validate more fields?
    ?assertEqual(<<"my-node">>, chef_node:name(Node)).

ejson_for_indexing_test_() ->
    [{"empty_node_test",
      fun() ->
              Node = merge(basic_node(), {[{<<"default">>, {[]}}]}),
              assert_ejson_equal(basic_node_index(),
                                 chef_object:ejson_for_indexing(basic_node_record(), Node))
      end},

     {"default_only_test",
      fun() ->
              Defaults = {[{<<"a">>, 1}]},
              Node = merge(basic_node(), {[{<<"default">>, Defaults}]}),
              assert_ejson_equal(merge(basic_node_index(), Defaults),
                                 chef_object:ejson_for_indexing(basic_node_record(), Node))
      end},

     {"normal_over_default_test",
      fun() ->
              Defaults = {[{<<"a">>, 1}, {<<"b">>, 2}]},
              Normal = {[{<<"b">>, 11}, {<<"c">>, 3}]},
              Node = merge(basic_node(), {[{<<"default">>, Defaults},
                                           {<<"normal">>, Normal}]}),
              Expect = merge(basic_node_index(), {[{<<"a">>, 1}, {<<"b">>, 11},
                                                   {<<"c">>, 3}]}),
              Got = chef_object:ejson_for_indexing(basic_node_record(), Node),
              assert_ejson_equal(Expect, Got)
      end},

     {"override_over_normal_over_default_test",
      fun() ->
              Defaults = {[{<<"a">>, 1}, {<<"b">>, 2}]},
              Normal = {[{<<"b">>, 11}, {<<"c">>, 3}]},
              Override = {[{<<"b">>, 22}, {<<"d">>, 4}]},
              Node = merge(basic_node(),
                           {[{<<"default">>, Defaults}, {<<"normal">>, Normal},
                             {<<"override">>, Override}]}),
              Expect = merge(basic_node_index(),
                             {[{<<"a">>, 1}, {<<"b">>, 22}, {<<"c">>, 3}, {<<"d">>, 4}]}),
              Got = chef_object:ejson_for_indexing(basic_node_record(), Node),
              assert_ejson_equal(Expect, Got)
      end},

     {"automatic_over_override_over_normal_over_default_test",
      fun() ->
              Defaults = {[{<<"a">>, 1}, {<<"b">>, 2}]},
              Normal = {[{<<"b">>, 11}, {<<"c">>, 3}]},
              Override = {[{<<"b">>, 22}, {<<"d">>, 4}]},
              Automatic = {[{<<"b">>, 22}, {<<"d">>, 40}, {<<"e">>, 5}]},
              Node = merge(basic_node(),
                           {[{<<"default">>, Defaults}, {<<"normal">>, Normal},
                             {<<"override">>, Override}, {<<"automatic">>, Automatic}]}),
              Expect = merge(basic_node_index(),
                             {[{<<"a">>, 1}, {<<"b">>, 22}, {<<"c">>, 3}, {<<"d">>, 40},
                               {<<"e">>, 5}]}),
              Got = chef_object:ejson_for_indexing(basic_node_record(), Node),
              assert_ejson_equal(Expect, Got)
      end}].

update_from_ejson_test_() ->
    Node = #chef_node{name = <<"old_node">>, environment = <<"old_env">>},
    RawNode = basic_node(),
    [{"chef_node fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Node1 = chef_object:update_from_ejson(Node, RawNode),
                GotData = Node1#chef_node.serialized_object,
                GotEjson = jiffy:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"a_node">>, Node1#chef_node.name),
                ?assertEqual(<<"prod">>, Node1#chef_node.environment),
                ?assertEqual(RawNode, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].


assert_ejson_equal(E1, E2) ->
    ?assertEqual(to_sorted_list(E1), to_sorted_list(E2)).

basic_node() ->
    {[{<<"name">>, <<"a_node">>},
      {<<"chef_environment">>, <<"prod">>},
      {<<"run_list">>, [<<"recipe[web]">>, <<"role[prod]">>]}
     ]}.

basic_node_record() ->
    #chef_node{name = <<"a_node">>, environment = <<"prod">>}.

basic_node_index() ->
    {[{<<"name">>, <<"a_node">>},
      {<<"chef_type">>, <<"node">>},
      {<<"chef_environment">>, <<"prod">>},
      {<<"recipe">>, [<<"web">>]},
      {<<"role">>, [<<"prod">>]},
      {<<"run_list">>, [<<"recipe[web]">>, <<"role[prod]">>]}
     ]}.

%% @doc Merge two proplists together, returning a proplist.  Treats them as dictionaries to
%% prevent repeated keys.  Values in L2 take precedence of values in L1.
merge({L1}, {L2}) ->
    D1 = dict:from_list(L1),
    D2 = dict:from_list(L2),
    Merged = dict:merge(fun(_K,_V1,V2) -> V2 end, D1,D2),
    {dict:to_list(Merged)}.

to_sorted_list({L}) ->
    lists:sort(L).
