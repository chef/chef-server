%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
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


-module(chef_depsolver_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").

%% Contains all allowed variants of run list items
good_runlist() ->
    {[ {<<"run_list">>, [<<"ntp">>, <<"recipe[ntp]">>, <<"recipe[apache2::default]">>, <<"role[foo]">>]} ]}.

empty_runlist() ->
    {[ {<<"run_list">>, []} ]}.

validate_body_test_() ->
    [
     {"Validate that a good run_list is ok",
      fun() ->
              ?assertEqual({ok, good_runlist()}, chef_depsolver:validate_body(good_runlist()))
      end},
     {"Validate that a bogus run list is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, empty_runlist(),
                         [<<"recipe[foo]">>, <<"fake[not_good]">>]),
              ?assertThrow(#ej_invalid{type=array_elt, key= <<"run_list">>},
                           chef_depsolver:validate_body(R))
      end},
     {"Validate that a run list that is not a list is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, empty_runlist(), 12),
              ?assertThrow(#ej_invalid{type=json_type, key= <<"run_list">>,
                                      found_type=number, expected_type=array},
                           chef_depsolver:validate_body(R))
      end},
     {"Validate that a run list with wrong type is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, empty_runlist(), [12]),
              ?assertThrow(#ej_invalid{type=array_elt, key= <<"run_list">>,
                                       found_type=number, expected_type=string},
                           chef_depsolver:validate_body(R))
      end}
    ].

set_default_values_test_() ->
    [
     {"Validate that a role with a missing attributes get appropriate defaults set",
      fun() ->
              WithDefaults = chef_depsolver:set_default_values({[]}),
              ?assertEqual([], ej:get({<<"run_list">>}, WithDefaults))
      end},
     {"Validate that fields with existing values are not overwritten by defaults",
      fun() ->
              Body = {[{<<"run_list">>, <<"recipe[foo]">>}]},
              WithDefaults = chef_depsolver:set_default_values(Body),
              ?assertEqual(<<"recipe[foo]">>, ej:get({<<"run_list">>}, WithDefaults))
      end}
    ].

parse_binary_json_test_() ->
    [
        {"Good json gets parsed properly",
         fun() ->
                 Bin = "{\"run_list\": []}",
                 ?assertEqual({ok, empty_runlist()}, chef_depsolver:parse_binary_json(Bin))
         end},
        {"Non json throws error",
         fun() ->
                 Bin = "this_is_not_json",
                 ?assertThrow({invalid_json, _},
                              chef_depsolver:parse_binary_json(Bin))
         end},
        {"Bad json throws error",
         fun() ->
                 Bin = "{\"run_list\":",
                 ?assertThrow({invalid_json, _},
                              chef_depsolver:parse_binary_json(Bin))
         end}
    ].

cookbook(Name, Version) ->
    {Name, [{Version, []}]}.
cookbook(Name, Version, Dep) when is_list(Dep) ->
    {Name, [{Version, Dep }] };
cookbook(Name, Version, Dep = {_Name, _Version}) ->
    {Name, [{Version, [ Dep ] }] };
cookbook(Name, Version, Dep = {_Name, _Version, _Reln}) ->
    {Name, [{Version, [ Dep ] }] }.

depsolver_dep_empty_deps_test() ->
    World = [ ],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, []),
    ?assertEqual({ok, []}, Ret).

depsolver_dep_empty_world_test() ->
    World = [ ],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    ?assertEqual({error, {unreachable_package, <<"foo">>}}, Ret).

depsolver_dep_no_version_test() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>)],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [{<<"foo">>, <<"2.0.0">>}]),
    Detail = [{[{[{<<"foo">>, {2,0,0}}],
                 [{<<"foo">>, {2,0,0}}]}],
               []
              }],
    ?assertEqual({error, Detail}, Ret).

%% Some tests which mimic the pedant tests for the depsolver endpoint

%%
%% We don't have bar cookbook
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar, which does not exist, due to run list item (foo >= 0.0.0).
%%            Run list items that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":["bar"],"
%% "most_constrained_cookbooks":[]}"
%%
depsolver_dep_doesnt_exist_test() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>)],
    Constraints = [ cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, gt}) ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    ?assertEqual({error, {unreachable_package, <<"bar">>}}, Ret).

%%
%% We have v 2.0.0 of bar but want > 2.0.0
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar due to run list item (foo >= 0.0.0).
%%            Run list items that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":[],
%% "most_constrained_cookbooks":["bar 2.0.0 -> []"]
%%
depsolver_dep_not_new_enough_test() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>),
              cookbook(<<"bar">>, <<"2.0.0">>)],
    Constraints = [cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, gt}) ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    %% TODO: Should this have bar in bad ??
    Detail = [{[{[<<"foo">>],
                 [{<<"foo">>, {1,2,3}}]}],
               [{{<<"foo">>, {1,2,3}}, [{<<"bar">>, {2,0,0}, gt}]}]
              }],
    ?assertEqual({error, Detail}, Ret).

%%
%% circular deps are bad
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar due to run list item (foo >= 0.0.0).
%%            Run list items that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":[],
%% "most_constrained_cookbooks:["bar = 2.0.0 -> [(foo > 3.0.0)]"]
%%
depsolver_impossible_dependency_test() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, { <<"bar">>, <<"2.0.0">>, gt}),
             cookbook(<<"bar">>, <<"2.0.0">>, { <<"foo">>, <<"3.0.0">>, gt})],
    Ret = chef_depsolver:solve_dependencies(World, [], [<<"foo">>]),
    Detail = [{[{[<<"foo">>],
                 [{<<"foo">>, {1,2,3}}]}],
               [{{<<"foo">>, {1,2,3}}, [{<<"bar">>, {2,0,0}, gt}]}]
              }],
    ?assertEqual({error, Detail}, Ret).

%% A more complex test.
%% World:
%% foo@1.2.3 -> bar@1.0.0 -> baz@1.0.0
%%     |
%%     -------> buzz@1.0.0 -> baz > 1.2.0
%% buzz@2.0.0 -> baz@1.0.0
%% ack@1.0.0 -> foobar @1.0.0
%% baz@1.0.0
%% baz@2.0.0
%%
%% solve(foo@1.2.3, buzz)
%% Fail since buzz@2.0.0 and foo@1.2.3 collide over baz
depsolver_complex_dependency_test() ->
    World = [cookbook(<<"foo">>, <<"1.2.3">>, [{ <<"bar">>, <<"1.0.0">>},
                                               { <<"buzz">>, <<"1.0.0">>}]),
             cookbook(<<"bar">>, <<"1.0.0">>, { <<"baz">>, <<"1.0.0">>}),
             cookbook(<<"buzz">>, <<"1.0.0">>, {<<"baz">>, <<"1.2.0">>, gt}),
             cookbook(<<"buzz">>, <<"2.0.0">>, {<<"baz">>, <<"1.0.0">>}),
             cookbook(<<"ack">>, <<"1.0.0">>, {<<"foobar">>, <<"1.0.0">>}),
             cookbook(<<"baz">>, <<"1.0.0">>),
             cookbook(<<"baz">>, <<"2.0.0">>)
            ],
    Ret = chef_depsolver:solve_dependencies(World, [], [<<"foo">>, <<"buzz">>]),
    Expected = [{{<<"buzz">>,{1,0,0}},[{<<"baz">>,{1,2,0},gt}]}],
    %% Check the culprits
    {error, [{_Paths, Culprits}] } = Ret,
    ?assertEqual(Expected, Culprits).

