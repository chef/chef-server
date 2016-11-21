%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Christopher Maier <cm@chef.io>
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


-module(chef_object_base_tests).

-include_lib("eunit/include/eunit.hrl").
-include("chef_types.hrl").

set_created_test_() ->
    ActorId = <<"12121212121212121212121212121212">>,
    [?_assertMatch(<<_Year:4/binary, "-", _Month:2/binary, "-", _Day:2/binary,
                     " ", _H:2/binary, ":", _M:2/binary, ":", _S:2/binary>>,
                   (chef_object_base:set_created(#chef_node{}, ActorId))#chef_node.created_at),
     fun() ->
             Obj = chef_object_base:set_created(#chef_role{}, ActorId),
             ?assertMatch(<<_Year:4/binary, "-", _Month:2/binary, "-", _Day:2/binary,
                            " ", _H:2/binary, ":", _M:2/binary, ":", _S:2/binary>>,
                          Obj#chef_role.created_at),
             ?assertEqual(Obj#chef_role.created_at, Obj#chef_role.updated_at),
             ?assertEqual(ActorId, Obj#chef_role.last_updated_by)
     end].

depsolver_constraints_test_() ->
    {foreachx,
     fun(_) -> ok end,
     fun(_, _) -> ok end,
     [ {{Input,Expected}, fun({I, E}, _) ->
                                  {Description,
                                   fun() ->
                                           Actual = chef_object_base:depsolver_constraints(I),
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
    NodeIds = [ chef_object_base:make_org_prefix_id(OrgGuid, NodeName)
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

deduplicate_run_list_test_() ->
    [{Message,
      fun() ->
              ?assertEqual(Expected,
                           chef_object_base:deduplicate_run_list(Input))
      end}
     || {Message, Input, Expected} <- [
                                       {"'deduplicates' an empty list", [], []},
                                       {"Leaves a list with no duplicates alone",
                                        ["foo", "bar", "baz"],
                                        ["foo", "bar", "baz"]},
                                       {"Deduplicates a list that happens to be sorted already",
                                        ["a", "a", "b", "a", "c", "c", "d"],
                                        ["a","b","c","d"]},
                                       {"Deduplicates an unsorted list, and retains its ordering",
                                        ["d", "c", "c", "a", "b", "b", "a"],
                                        ["d", "c", "a", "b"]}
                                      ]
    ].

normalize_item_test_() ->
    [{Message,
     fun() ->
             ?assertEqual(Normalized,
                         chef_object_base:normalize_item(Input))
     end}
     || {Message, Input, Normalized} <- [
                                         {"Explicit recipes are unchanged",
                                          <<"recipe[foo]">>,
                                          <<"recipe[foo]">>},
                                         {"Roles are unchanged",
                                          <<"role[foo]">>,
                                          <<"role[foo]">>},
                                         {"Bare cookbooks are tagged as recipes",
                                          <<"foo">>,
                                          <<"recipe[foo]">>},
                                         {"Cookbook-qualified recipes are tagged as recipes",
                                         <<"foo::bar">>,
                                          <<"recipe[foo::bar]">>},
                                         {"Versioned recipes are tagged as recipes",
                                          <<"foo::bar@1.0.0">>,
                                          <<"recipe[foo::bar@1.0.0]">>},
                                         {"Actually, any binary, whether it is a valid cookbook/recipe or not, is tagged as a recipe; we currently assume valid run list items as input",
                                         <<"23?8^3$$$%-not-valid-input">>,
                                          <<"recipe[23?8^3$$$%-not-valid-input]">>}
                                        ]
    ].

normalize_run_list_test_() ->
    [{Message,
      fun() ->
              ?assertEqual(Normalized,
                           chef_object_base:normalize_run_list(Input))
      end}
     || {Message, Input, Normalized} <- [
                                         {"Normalizes an empty run list", [], []},
                                         {"Does nothing to an already normalized list",
                                          [<<"recipe[foo]">>, <<"role[web]">>, <<"recipe[bar::baz]">>],
                                          [<<"recipe[foo]">>, <<"role[web]">>, <<"recipe[bar::baz]">>]},
                                         {"Normalizes bare cookbooks to recipes",
                                          [<<"foo">>, <<"bar">>, <<"baz">>],
                                          [<<"recipe[foo]">>, <<"recipe[bar]">>, <<"recipe[baz]">>]},
                                         {"Normalizes a mix of run list items",
                                          [<<"foo">>, <<"recipe[bar]">>, <<"baz::quux">>, <<"role[server]">>],
                                          [<<"recipe[foo]">>, <<"recipe[bar]">>, <<"recipe[baz::quux]">>, <<"role[server]">>]},
                                         {"Removes duplicates after normalization",
                                          [<<"foo">>, <<"recipe[foo]">>],
                                          [<<"recipe[foo]">>]}
                                        ]
    ].

%% Calling out this behavior on its own in case we decide to change it in the future
semantic_duplication_test_() ->
    [{"Semantic duplicates in a node run list are preserved",
      fun() ->
              Input = [<<"foo">>, <<"foo::default">>],
              Normalized = [<<"recipe[foo]">>, <<"recipe[foo::default]">>],
              ?assertEqual(Normalized,
                           chef_object_base:normalize_run_list(Input))
      end}
    ].

public_key_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("spki_public.pem"),
    Bin.

cert_data() ->
    {ok, Bin} = chef_objects_test_utils:read_file("cert.pem"),
    Bin.

set_key_pair_test_() ->
    Ejson = {[]},
    PrivateKey = <<"private">>,
    DataForType = fun(key) ->
                          public_key_data();
                     (cert) ->
                          cert_data()
                  end,
    KeyForType = fun(key) ->
                         <<"public_key">>;
                    (cert) ->
                         <<"certificate">>
                 end,
    NotKeyForType = fun(key) ->
                            <<"certificate">>;
                       (cert) ->
                            <<"public_key">>
                    end,
    Tests = [
             begin
                 Got = chef_object_base:set_key_pair(Ejson,
                                                {public_key, DataForType(Type)},
                                                {private_key, PrivateKey}),
                 [?_assertEqual(PrivateKey, ej:get({<<"private_key">>}, Got)),
                  ?_assertEqual(DataForType(Type), ej:get({KeyForType(Type)}, Got)),
                  ?_assertEqual(undefined, ej:get({NotKeyForType(Type)}, Got))]
             end
             || Type <- [key, cert] ],
    lists:flatten(Tests).

