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
-include_lib("chef_objects/include/chef_osc_defaults.hrl").
-compile([export_all]).

-define(NEEDED_APPS, [depsolver, folsom]).

all_test_() ->
  {foreach,
    fun() ->
        error_logger:delete_report_handler(error_logger_tty_h),
        [ application:start(App) || App <- ?NEEDED_APPS ]
    end,
    fun(_) ->
            [ application:stop(App) || App <- lists:reverse(?NEEDED_APPS) ]
    end,
    [
        {?MODULE, depsolver_dep_empty_world},
        {?MODULE, depsolver_dep_no_version},
        {?MODULE, depsolver_dep_doesnt_exist},
        {?MODULE, depsolver_dep_not_new_enough},
        {?MODULE, depsolver_impossible_dependency},
        {generator, ?MODULE, depsolver_environment_respected},
        {?MODULE, depsolver_impossible_dependency_via_environment},
        {?MODULE, depsolver_complex_dependency}
  ]
}.
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
              ?assertThrow(#ej_invalid{},
                           chef_depsolver:validate_body(R))
      end},
     {"Validate that a run list that is not a list is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, empty_runlist(), 12),
              ?assertThrow(#ej_invalid{},
                           chef_depsolver:validate_body(R))
      end},
     {"Validate that a run list with wrong type is rejected",
      fun() ->
              R = ej:set({<<"run_list">>}, empty_runlist(), [12]),
              ?assertThrow(#ej_invalid{},
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
                 ?assertThrow({error, _},
                              chef_depsolver:parse_binary_json(Bin))
         end},
        {"Bad json throws error",
         fun() ->
                 Bin = "{\"run_list\":",
                 ?assertThrow({error, _},
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

depsolver_dep_empty_deps() ->
    World = [ ],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, []),
    ?assertEqual({ok, []}, Ret).

depsolver_dep_empty_world() ->
    World = [ ],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    ?assertEqual({error, {unreachable_package, <<"foo">>}}, Ret).

depsolver_dep_no_version() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>)],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [{<<"foo">>, <<"2.0.0">>}]),
    Detail = [{[{[{<<"foo">>, {{2,0,0}, {[], []}}}],
                 [{<<"foo">>, {{2,0,0}, {[], []}}}]}],
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
depsolver_dep_doesnt_exist() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, gt})],
    Constraints = [],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    ?assertEqual({error,
            [{[{[<<102,111,111>>],[{<<102,111,111>>,{{1,2,3},{[],[]}}}]}],[{{<<102,111,111>>,{{1,2,3},{[],[]}}},[{<<98,97,114>>,{{2,0,0},{[],[]}},gt}]}]}]}
        , Ret).

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
depsolver_dep_not_new_enough() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, '>'}),
              cookbook(<<"bar">>, <<"2.0.0">>)],
    Constraints = [{<<"foo">>, <<"1.2.3">>, '='}],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    %% TODO: Should this have bar in bad ??
    Detail = [
              {[{[<<"foo">>], [{<<"foo">>, {{1, 2, 3}, {[], []}}}]}],
                  [{{<<"foo">>, {{1, 2, 3}, {[], []}}},
                    [{<<"bar">>, {{2, 0, 0}, {[], []}}, '>'}]}]}
             ],
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
depsolver_impossible_dependency() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, { <<"bar">>, <<"2.0.0">>, gt}),
             cookbook(<<"bar">>, <<"2.0.0">>, { <<"foo">>, <<"3.0.0">>, gt})],
    Ret = chef_depsolver:solve_dependencies(World, [], [<<"foo">>]),
    Detail = [{[{[<<"foo">>],
                 [{<<"foo">>, {{1,2,3}, {[], []}}}]}],
               [{{<<"foo">>, {{1,2,3}, {[], []}}},
                 [{<<"bar">>, {{2,0,0}, {[], []}}, gt}]}]
              }],
    ?assertEqual({error, Detail}, Ret).

depsolver_environment_respected() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, gt}),
              cookbook(<<"foo">>, <<"1.0.0">>, {<<"bar">>, <<"1.0.0">>, '='}),
              cookbook(<<"bar">>, <<"1.0.0">>),
              cookbook(<<"bar">>, <<"3.0.0">>) ],

    Env100 = make_env(<<"myenv">>, {[{<<"bar">>, <<"= 1.0.0">>}]}),
    Constraints100 = chef_object:depsolver_constraints(Env100),

    Env123 = make_env(<<"myenv">>, {[{<<"bar">>, <<"> 1.1.0">>}]}),
    Constraints123 = chef_object:depsolver_constraints(Env123),

    Expect100 = {ok, [{<<"bar">>, {1, 0, 0}}, {<<"foo">>, {1, 0, 0}}]},
    Expect123 = {ok, [{<<"bar">>, {3, 0, 0}}, {<<"foo">>, {1, 2, 3}}]},

    Tests = [ {Constraints100, Expect100},
              {Constraints123, Expect123} ],
    [ ?_assertEqual(Expect, chef_depsolver:solve_dependencies(World, Cons,
                                                              [<<"foo">>]))
      || {Cons, Expect} <- Tests ].

depsolver_impossible_dependency_via_environment() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, { <<"bar">>, <<"2.0.0">>, gt}),
              cookbook(<<"bar">>, <<"1.0.0">>),
              cookbook(<<"bar">>, <<"3.0.0">>) ],
    CookbookCons = {[{<<"bar">>, <<"= 1.0.0">>}]},
    Env = make_env(<<"myenv">>, CookbookCons),
    Constraints = chef_object:depsolver_constraints(Env),
    %% without constraints in env, ok
    ?assertMatch({ok, _}, chef_depsolver:solve_dependencies(World, [], [<<"foo">>])),
    %% with the constraints, foo can't be satisfied
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    Expect = {error, [{[{[<<"foo">>], [{<<"foo">>, {{1, 2, 3}, {[], []}}}]}],
                      [{{<<"foo">>, {{1, 2, 3}, {[], []}}}, [{<<"bar">>, {{2, 0, 0}, {[], []}}, gt}]}]}]},
    ?assertEqual(Expect, Ret).

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
depsolver_complex_dependency() ->
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
    Expected = [{{<<"buzz">>,{{1,0,0}, {[], []}}},[{<<"baz">>,{{1,2,0}, {[], []}},gt}]}],
    %% Check the culprits
    {error, [{_Paths, Culprits}] } = Ret,
    ?assertEqual(Expected, Culprits),
    %% verify that an unrelated set of constraints doesn't change anything
    Cons = [{<<"someting">>, <<"1.0.0">>, '='}],
    ?assertEqual(Ret, chef_depsolver:solve_dependencies(World, Cons, [<<"foo">>, <<"buzz">>])).

make_env(Name, Deps) ->
    Ejson0 = {[
              {<<"name">>, Name},
              {<<"description">>, <<"test env">>},
              {<<"cookbook_versions">>, Deps}
             ]},
    Json = chef_json:encode(Ejson0),
    {ok, Ejson} = chef_environment:parse_binary_json(Json),
    chef_object:new_record(chef_environment, ?OSC_ORG_ID, unset, Ejson).
