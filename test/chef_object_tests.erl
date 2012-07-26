%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
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


-module(chef_object_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

id_test_() ->
    [?_assertEqual(<<"1">>, chef_object:id(#chef_data_bag_item{id = <<"1">>})),
     ?_assertEqual(<<"1">>, chef_object:id(#chef_data_bag{id = <<"1">>})),
     ?_assertEqual(<<"1">>, chef_object:id(#chef_environment{id = <<"1">>})),
     ?_assertEqual(<<"1">>, chef_object:id(#chef_node{id = <<"1">>})),
     ?_assertEqual(<<"1">>, chef_object:id(#chef_role{id = <<"1">>})),
     ?_assertEqual(<<"1">>, chef_object:id(#chef_cookbook_version{id = <<"1">>}))].

set_created_test_() ->
    ActorId = <<"12121212121212121212121212121212">>,
    [?_assertMatch(<<_Year:4/binary, "-", _Month:2/binary, "-", _Day:2/binary,
                     " ", _H:2/binary, ":", _M:2/binary, ":", _S:2/binary>>,
                   (chef_object:set_created(#chef_node{}, ActorId))#chef_node.created_at),
     fun() ->
             Obj = chef_object:set_created(#chef_role{}, ActorId),
             ?assertMatch(<<_Year:4/binary, "-", _Month:2/binary, "-", _Day:2/binary,
                            " ", _H:2/binary, ":", _M:2/binary, ":", _S:2/binary>>,
                          Obj#chef_role.created_at),
             ?assertEqual(Obj#chef_role.created_at, Obj#chef_role.updated_at),
             ?assertEqual(ActorId, Obj#chef_role.last_updated_by)
     end].

name_test_() ->
    [?_assertEqual({<<"bag name">>, <<"item name">>},
                   chef_object:name(#chef_data_bag_item{data_bag_name = <<"bag name">>,
                                                        item_name =  <<"item name">>})),
     ?_assertEqual(<<"a_name">>, chef_object:name(#chef_data_bag{name =  <<"a_name">>})),
     ?_assertEqual(<<"a_name">>, chef_object:name(#chef_environment{name =  <<"a_name">>})),
     ?_assertEqual(<<"a_name">>, chef_object:name(#chef_node{name =  <<"a_name">>})),
     ?_assertEqual(<<"a_name">>, chef_object:name(#chef_role{name =  <<"a_name">>})),
     ?_assertEqual(<<"a_name">>,
                   chef_object:name(#chef_cookbook_version{name =  <<"a_name">>}))].

type_name_test_() ->
    [
     ?_assertEqual(data_bag_item, chef_object:type_name(#chef_data_bag_item{})),
     ?_assertEqual(data_bag, chef_object:type_name(#chef_data_bag{})),
     ?_assertEqual(environment, chef_object:type_name(#chef_environment{})),
     ?_assertEqual(node, chef_object:type_name(#chef_node{})),
     ?_assertEqual(role, chef_object:type_name(#chef_role{}))
    ].

new_record_test_() ->
    OrgId = <<"12345678123456781234567812345678">>,
    AuthzId = <<"authz-123">>,
    NameJson = {[{<<"name">>, <<"the-name">>}, {<<"alpha">>, <<"bravo">>}]},
    IdJson = {[{<<"id">>, <<"the-name">>}, {<<"alpha">>, <<"bravo">>}]},
    DbTypes = [pgsql, mysql],
    NameForInput = fun(Name) when is_binary(Name) ->
                           Name;
                      ({BagName, {ItemData}}) when is_binary(BagName) ->
                           ItemName = proplists:get_value(<<"id">>, ItemData),
                           {BagName, ItemName};
                      (Obj={L}) when is_list(L) ->
                           ej:get({"cookbook_name"}, Obj,
                                  ej:get({"name"}, Obj))
                   end,
    Tests = [
             {chef_data_bag, OrgId, AuthzId, <<"bag-name">>},
             {chef_data_bag_item, OrgId, AuthzId, {<<"bag-name">>, IdJson}},
             {chef_node, OrgId, AuthzId, NameJson},
             {chef_role, OrgId, AuthzId, NameJson},
             {chef_cookbook_version, OrgId, AuthzId,
              example_cookbook_version_json()}
            ],
    [ {atom_to_list(RecName),
       fun() ->
              Got = chef_object:new_record(RecName, O, A, Data, DbType),
              ?assertEqual(NameForInput(Data), chef_object:name(Got)),
              ?assertEqual(32, size(chef_object:id(Got)))
      end} || {RecName, O, A, Data} <- Tests, DbType <- DbTypes ].

%% ejson_for_indexing tests

data_bag_ejson_for_indexing_test() ->
    Bag = #chef_data_bag{name = <<"the_bag_name">>},
    Expected = {[{<<"name">>, <<"the_bag_name">>},
                {<<"chef_type">>, <<"data_bag">>},
                {<<"json_class">>, <<"Chef::DataBag">>}]},
    Got = chef_object:ejson_for_indexing(Bag, <<"the_bag_name">>),
    ?assertEqual(Expected, Got).

data_bag_item_ejson_for_indexing_test() ->
    RawItem = {[{<<"id">>, <<"the_item_name">>},
                {<<"a_key">>, <<"a_value">>}]},
    Item = #chef_data_bag_item{data_bag_name = <<"the_bag_name">>,
                               item_name = <<"the_item_name">>},
    Expected = {[{<<"name">>, <<"data_bag_item_the_bag_name_the_item_name">>},
                {<<"data_bag">>, <<"the_bag_name">>},
                {<<"chef_type">>, <<"data_bag_item">>},
                {<<"json_class">>, <<"Chef::DataBagItem">>},
                {<<"raw_data">>, RawItem}]},
    Got = chef_object:ejson_for_indexing(Item, RawItem),
    ?assertEqual(Expected, Got).

role_ejson_for_indexing_test_() ->
    Role = #chef_role{name = <<"a_role">>},
    RawRole = {[{<<"name">>, <<"a_role">>},
                {<<"description">>, <<"role description">>},
                {<<"json_class">>, <<"Chef::Role">>},
                {<<"default_attributes">>, {[{<<"a">>, <<"b">>}]}},
                {<<"override_attributes">>, {[{<<"a">>, <<"b">>},
                                              {<<"x">>, <<"y">>}]}},
                {<<"chef_type">>, <<"role">>},
                {<<"run_list">>, [<<"recipe[apache2]">>, <<"role[web]">>]},
                {<<"env_run_lists">>, {[]}}
               ]},
    [{"empty env_run_lists",
      fun() ->
              Expected = RawRole,
              Got = chef_object:ejson_for_indexing(Role, RawRole),
              ?assertEqual(Expected, Got)
      end},
     {"_default in env_run_lists is removed",
      fun() ->
              EnvRunListsExpected = {[{<<"prod">>, [<<"recipe[a2]">>, <<"role[b3]">>]}]},
              EnvRunLists = {[{<<"prod">>, [<<"recipe[a2]">>, <<"role[b3]">>]},
                              {<<"_default">>, [<<"recipe[a0]">>, <<"role[b0]">>]}]},
              RawRole1 = ej:set({<<"env_run_lists">>}, RawRole, EnvRunLists),
              Expected = ej:set({<<"env_run_lists">>}, RawRole, EnvRunListsExpected),
              Got = chef_object:ejson_for_indexing(Role, RawRole1),
              ?assertEqual(Expected, Got)
      end}
    ].

node_ejson_for_indexing_test_() ->
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

%% END ejson_for_indexing tests

%% update_from_ejson tests
data_Bag_item_update_from_ejson_test_() ->
    RawItem = {[{<<"id">>, <<"the_item_name">>},
                {<<"a_key">>, <<"a_value">>}]},
    Item = #chef_data_bag_item{data_bag_name = <<"the_bag_name">>,
                               item_name = <<"old_item_name">>},
    [{"chef_data_bag_item fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Item1 = chef_object:update_from_ejson(Item, RawItem, DbType),
                GotData = Item1#chef_data_bag_item.serialized_object,
                GotEjson = ejson:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"the_item_name">>, Item1#chef_data_bag_item.item_name),
                ?assertEqual(RawItem, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

environment_update_from_ejson_test_() ->
    Env = #chef_environment{name = <<"old_name">>},
    RawEnv = {[{<<"name">>, <<"new_name">>},
                {<<"description">>, <<"environment description">>},
                {<<"json_class">>, <<"Chef::Environment">>}
               %% FIXME: fill out more compelte environment json object
               ]},
    [{"chef_environment fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Env1 = chef_object:update_from_ejson(Env, RawEnv, DbType),
                GotData = Env1#chef_environment.serialized_object,
                GotEjson = ejson:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"new_name">>, Env1#chef_environment.name),
                ?assertEqual(RawEnv, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

node_update_from_ejson_test_() ->
    Node = #chef_node{name = <<"old_node">>, environment = <<"old_env">>},
    RawNode = basic_node(),
    [{"chef_node fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Node1 = chef_object:update_from_ejson(Node, RawNode, DbType),
                GotData = Node1#chef_node.serialized_object,
                GotEjson = ejson:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"a_node">>, Node1#chef_node.name),
                ?assertEqual(<<"prod">>, Node1#chef_node.environment),
                ?assertEqual(RawNode, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

role_update_from_ejson_test_() ->
    Role = #chef_role{name = <<"a_role">>},
    RawRole = {[{<<"name">>, <<"new_name">>},
                {<<"description">>, <<"role description">>},
                {<<"json_class">>, <<"Chef::Role">>},
                {<<"default_attributes">>, {[{<<"a">>, <<"b">>}]}},
                {<<"override_attributes">>, {[{<<"a">>, <<"b">>},
                                              {<<"x">>, <<"y">>}]}},
                {<<"chef_type">>, <<"role">>},
                {<<"run_list">>, [<<"recipe[apache2]">>, <<"role[web]">>]},
                {<<"env_run_lists">>, {[]}}
               ]},

    [{"chef_role fields are set from json for all dbs",
      [
       {atom_to_list(DbType),
        fun() ->
                Role1 = chef_object:update_from_ejson(Role, RawRole, DbType),
                GotData = Role1#chef_role.serialized_object,
                GotEjson = ejson:decode(chef_db_compression:decompress(GotData)),
                ?assertEqual(<<"new_name">>, Role1#chef_role.name),
                ?assertEqual(RawRole, GotEjson)
        end} || DbType <- [mysql, pgsql] ]}
    ].

%% END update_from_ejson

basic_node_record() ->
    #chef_node{name = <<"a_node">>, environment = <<"prod">>}.

basic_node() ->
    {[{<<"name">>, <<"a_node">>},
      {<<"chef_environment">>, <<"prod">>},
      {<<"run_list">>, [<<"recipe[web]">>, <<"role[prod]">>]}
     ]}.

basic_node_index() ->
    {[{<<"name">>, <<"a_node">>},
      {<<"chef_type">>, <<"node">>},
      {<<"chef_environment">>, <<"prod">>},
      {<<"recipe">>, [<<"web">>]},
      {<<"role">>, [<<"prod">>]},
      {<<"run_list">>, [<<"recipe[web]">>, <<"role[prod]">>]}
     ]}.

merge({L1}, {L2}) ->
    {L1 ++ L2}.

to_sorted_list({L}) ->
    lists:sort(L).

assert_ejson_equal(E1, E2) ->
    ?assertEqual(to_sorted_list(E1), to_sorted_list(E2)).

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

depsolver_constraints_test_() ->
    {foreachx,
     fun(_) -> ok end,
     fun(_, _) -> ok end,
     [ {{Input,Expected}, fun({I, E}, _) ->
                                  {Description,
                                   fun() ->
                                           Actual = chef_object:depsolver_constraints(I),
                                           ?assertEqual(E, Actual)
                                   end}
                          end}
       || {Description, Input, Expected} <- [
                                             {"basic constraint as Erlang terms",
                                              {[{<<"foo">>, <<"= 1.0.0">>}]},
                                              [ {<<"foo">>, <<"1.0.0">>, '='} ]
                                             },

                                             {"basic constraint as JSON string",
                                              <<"{\"foo\": \"= 1.0.0\"}">>,
                                              [ {<<"foo">>, <<"1.0.0">>, '='} ]
                                             },

                                             {"basic constraint from an Environment",
                                              #chef_environment{
                                                serialized_object=zlib:gzip(
                                                                    <<"{\"cookbook_versions\":{\"foo\": \"= 1.0.0\"}}">>)},
                                              [ {<<"foo">>, <<"1.0.0">>, '='} ]
                                             }
                                            ]
     ]}.

make_org_prefix_id_test_() ->
    OrgId = make_org_id(),
    Inputs = [{OrgId, "some-node"}, {OrgId, <<"some-node">>},
              %% repeat on purpose
              {OrgId, <<"some-node">>}],
    NodeIds = [ chef_object:make_org_prefix_id(OrgGuid, NodeName)
                || {OrgGuid, NodeName} <- Inputs ],
    [{"node ids are unique",
      ?_assertEqual(length(NodeIds), length(lists:usort(NodeIds)))},

     {"node ids have the right prefix", generator,
      fun() ->
              <<_:20/binary, Expect:12/binary>> = OrgId,
              Prefixes = [Got || <<Got:12/binary, _:20/binary>> <- NodeIds],
              ?assertEqual(length(NodeIds), length(Prefixes)),
              [ ?_assertEqual(Expect, G) || G <- Prefixes ]
      end},

     {"node ids have the right length", generator,
      fun() ->
              [ ?_assertEqual(32, size(Id)) || Id <- NodeIds ]
      end}
    ].

make_org_id() ->
    <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = crypto:rand_bytes(16),
    Fmt = "~8.16.0b~4.16.0b~4.16.0b~2.16.0b~2.16.0b~12.16.0b",
    iolist_to_binary((io_lib:format(Fmt, [TL, TM, THV, CSR, CSL, N]))).
