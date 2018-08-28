%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author James Casey <james@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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
-include("chef_osc_defaults.hrl").
-include("chef_types.hrl").
-compile([export_all]).

-define(NEEDED_APPS, [ pooler ]).

all_test_() ->
  {foreach,
   fun() ->
           error_logger:delete_report_handler(error_logger_tty_h),
           [ ok = application:start(App) || App <- ?NEEDED_APPS ],
           PoolConfig = [{name, chef_depsolver},
                         {max_count, 1},
                         {init_count, 1},
                         {start_mfa, {chef_depsolver_worker, start_link, []}}],
           pooler:new_pool(PoolConfig)
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
    {?MODULE, depsolver_complex_dependency},
    {?MODULE, depsolver_first},
    {?MODULE, depsolver_second},
    {?MODULE, depsolver_third},
    {?MODULE, depsolver_fail},
    {?MODULE, depsolver_conflicting_passing},
    {?MODULE, depsolver_circular_dependencies},
    {?MODULE, depsolver_conflicting_failing},
    {?MODULE, depsolver_pessimistic_major_minor_patch},
    {?MODULE, depsolver_pessimistic_major_minor},
    {?MODULE, depsolver_missing},
    {?MODULE, depsolver_missing_via_culprit_search},
    {?MODULE, depsolver_binary},
    {?MODULE, depsolver_no_workers},
    {?MODULE, depsolver_negative_version}
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
    ?assertEqual({error,invalid_constraints,
                  [{non_existent_cookbooks,[<<"foo">>]},
                   {constraints_not_met,[]}]}, Ret).

depsolver_dep_no_version() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>)],
    Constraints = [ ],
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [{<<"foo">>, <<"2.0.0">>}]),
    Error = {error,invalid_constraints,
             [{non_existent_cookbooks,[]},
              {constraints_not_met,[<<"(foo = 2.0.0)">>]}]},
    ?assertEqual(Error, Ret).

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
    Error = {error,no_solution,
             [{message,<<"Unable to satisfy constraints on package bar, which does not exist, due to solution constraint (foo >= 0.0.0). Solution constraints that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]">>},
              {unsatisfiable_run_list_item,<<"(foo >= 0.0.0)">>},
              {non_existent_cookbooks,[<<"bar">>]},
              {most_constrained_cookbooks,[]}]},
    ?assertEqual(Error, Ret).

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
    Error = {error,no_solution,
             [{message,<<"Unable to satisfy constraints on package bar due to solution constraint (foo >= 0.0.0). Solution constraints that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]">>},
              {unsatisfiable_run_list_item,<<"(foo >= 0.0.0)">>},
              {non_existent_cookbooks,[]},
              {most_constrained_cookbooks,[<<"bar = 2.0.0 -> []">>]}]},
    ?assertEqual(Error, Ret).

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
    Error = {error,no_solution,
             [{message,<<"Unable to satisfy constraints on package bar due to solution constraint (foo >= 0.0.0). Solution constraints that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]">>},
              {unsatisfiable_run_list_item,<<"(foo >= 0.0.0)">>},
              {non_existent_cookbooks,[]},
              {most_constrained_cookbooks,[<<"bar = 2.0.0 -> [(foo > 3.0.0)]">>]}]},
        ?assertEqual(Error, Ret).

depsolver_environment_respected() ->
    World = [ cookbook(<<"foo">>, <<"1.2.3">>, {<<"bar">>, <<"2.0.0">>, gt}),
              cookbook(<<"foo">>, <<"1.0.0">>, {<<"bar">>, <<"1.0.0">>, '='}),
              cookbook(<<"bar">>, <<"1.0.0">>),
              cookbook(<<"bar">>, <<"3.0.0">>) ],

    Env100 = make_env(<<"myenv">>, {[{<<"bar">>, <<"= 1.0.0">>}]}),
    Constraints100 = chef_object_base:depsolver_constraints(Env100),

    Env123 = make_env(<<"myenv">>, {[{<<"bar">>, <<"> 1.1.0">>}]}),
    Constraints123 = chef_object_base:depsolver_constraints(Env123),

    Expect100 = {ok, [{<<"foo">>, {1, 0, 0}}, {<<"bar">>, {1, 0, 0}}]},
    Expect123 = {ok, [{<<"foo">>, {1, 2, 3}}, {<<"bar">>, {3, 0, 0}}]},

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
    Constraints = chef_object_base:depsolver_constraints(Env),
    %% without constraints in env, ok
    ?assertMatch({ok, _}, chef_depsolver:solve_dependencies(World, [], [<<"foo">>])),
    %% with the constraints, foo can't be satisfied
    Ret = chef_depsolver:solve_dependencies(World, Constraints, [<<"foo">>]),
    Error = {error,no_solution,
             [{message,<<"Unable to satisfy constraints on package bar due to solution constraint (foo >= 0.0.0). Solution constraints that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]">>},
              {unsatisfiable_run_list_item,<<"(foo >= 0.0.0)">>},
              {non_existent_cookbooks,[]},
              {most_constrained_cookbooks,[<<"bar = 1.0.0 -> []">>]}]},
    ?assertEqual(Error, Ret).

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

    _Error = {error,no_solution,
              [{message,<<"Unable to satisfy constraints on package baz due to solution constraint (foo >= 0.0.0). Solution constraints that may result in a constraint on baz: [(foo = 1.2.3) -> (bar = 1.0.0) -> (baz = 1.0.0)], [(foo = 1.2.3) -> (buzz = 1.0.0) -> (baz > 1.2.0)], [(buzz = 1.0.0) -> (baz > 1.2.0)], [(buzz = 2.0.0) -> (baz = 1.0.0)]">>},
               {unsatisfiable_run_list_item,<<"(foo >= 0.0.0)">>},
               {non_existent_cookbooks,[]},
               {most_constrained_cookbooks,[<<"baz = 1.0.0 -> []">>]}]},

    %% %% Check the culprits
    %% {error, [{_Paths, Culprits}] } = Ret,
    %% ?assertEqual(Expected, Culprits),

    %% verify that an unrelated set of constraints doesn't change anything
    Cons = [{<<"someting">>, <<"1.0.0">>, '='}],
    ?assertEqual(Ret, chef_depsolver:solve_dependencies(World, Cons, [<<"foo">>, <<"buzz">>])).

depsolver_first() ->
    World = [{<<"app1">>, [{<<"0.1">>, [{<<"app2">>, <<"0.2.33">>},
                                        {<<"app3">>, <<"0.2">>, '>='}]},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]},
             {<<"app2">>, [{<<"0.1">>, []},
                           {<<"0.2.33">>,[{<<"app3">>, <<"0.3">>}]},
                           {<<"0.3">>, []}]},
             {<<"app3">>, [{<<"0.1">>, []},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]}],
    RunList = [{<<"app1">>, <<"0.1">>}],
    Expected = {ok,[{<<"app1">>,{0,1,0}},
                    {<<"app2">>,{0,2,33}},
                    {<<"app3">>,{0,3,0}}]},
    Result = chef_depsolver:solve_dependencies(World, [], RunList),
    ?assertEqual(Expected, Result).

depsolver_second() ->
    World = [{<<"app1">>, [{<<"0.1">>, [{<<"app2">>, <<"0.1">>, '>='},
                                        {<<"app4">>, <<"0.2">>},
                                        {<<"app3">>, <<"0.2">>, '>='}]},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]},
             {<<"app2">>, [{<<"0.1">>, [{<<"app3">>, <<"0.2">>, gte}]},
                           {<<"0.2">>, [{<<"app3">>, <<"0.2">>, gte}]},
                           {<<"0.3">>, [{<<"app3">>, <<"0.2">>, '>='}]}]},
             {<<"app3">>, [{<<"0.1">>, [{<<"app4">>, <<"0.2">>, '>='}]},
                           {<<"0.2">>, [{<<"app4">>, <<"0.2">>}]},
                           {<<"0.3">>, []}]},
             {<<"app4">>, [{<<"0.1">>, []},
                           {<<"0.2">>, [{<<"app2">>, <<"0.2">>, gte},
                                        {<<"app3">>, <<"0.3">>}]},
                           {<<"0.3">>, []}]}],
    RunList = [{<<"app1">>, <<"0.1">>},
               {<<"app2">>, <<"0.3">>}],
    Expected = {ok, [{<<"app1">>,{0,1,0}},
                     {<<"app2">>,{0,3,0}},
                     {<<"app3">>,{0,3,0}},
                     {<<"app4">>,{0,2,0}}]},
    Result = chef_depsolver:solve_dependencies(World, [], RunList),
    ?assertEqual(Expected, Result).

depsolver_third() ->
    Pkg1Deps = [{<<"app2">>, <<"0.1.0">>, '>='},
                {<<"app3">>, <<"0.1.1">>, gte},
                {<<"app3">>, <<"0.1.5">>, lte}], %% adding gte and lte to replace between
    Pkg2Deps = [{<<"app4">>, <<"5.0.0">>, gte}],
    Pkg3Deps = [{<<"app5">>, <<"2.0.0">>, '>='}],
    Pkg4Deps = [<<"app5">>],
    World = [{<<"app1">>, [{<<"0.1.0">>, Pkg1Deps},
                           {<<"0.2">>, Pkg1Deps},
                           {<<"3.0">>, Pkg1Deps}]},
             {<<"app2">>, [{<<"0.0.1">>, Pkg2Deps},
                           {<<"0.1">>, Pkg2Deps},
                           {<<"1.0">>, Pkg2Deps},
                           {<<"3.0">>, Pkg2Deps}]},
             {<<"app3">>, [{<<"0.1.0">>, Pkg3Deps},
                           {<<"0.1.3">>, Pkg3Deps},
                           {<<"2.0.0">>, Pkg3Deps},
                           {<<"3.0.0">>, Pkg3Deps},
                           {<<"4.0.0">>, Pkg3Deps}]},
             {<<"app4">>, [{<<"0.1.0">>, Pkg4Deps},
                           {<<"0.3.0">>, Pkg4Deps},
                           {<<"5.0.0">>, Pkg4Deps},
                           {<<"6.0.0">>, Pkg4Deps}]},
             {<<"app5">>, [{<<"0.1.0">>, []},
                           {<<"0.3.0">>, []},
                           {<<"2.0.0">>, []},
                           {<<"6.0.0">>, []}]}],

    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{3,0,0}},
                       {<<"app4">>,{6,0,0}},
                       {<<"app5">>,{6,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World, [], [{<<"app1">>, <<"3.0">>}])),

    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{3,0,0}},
                       {<<"app4">>,{6,0,0}},
                       {<<"app5">>,{6,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World, [], [<<"app1">>])).

depsolver_fail() ->
    World = [{<<"app1">>, [{<<"0.1">>, [{<<"app2">>, <<"0.2">>},
                                        {<<"app3">>, <<"0.2">>, gte}]},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]},
             {<<"app2">>, [{<<"0.1">>, []},
                           {<<"0.2">>,[{<<"app3">>, <<"0.1">>}]},
                           {<<"0.3">>, []}]},
             {<<"app3">>, [{<<"0.1">>, []},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]}],

    Result = chef_depsolver:solve_dependencies(World, [], [{<<"app1">>, <<"0.1">>}]),
    ?assertMatch({error, no_solution, _}, Result).

depsolver_conflicting_passing() ->
    Pkg1Deps = [{<<"app2">>, <<"0.1.0">>, '>='},
                {<<"app5">>, <<"2.0.0">>},
                {<<"app4">>, <<"0.3.0">>, gte},
                {<<"app4">>, <<"5.0.0">>, lte},
                {<<"app3">>, <<"0.1.1">>, gte},
                {<<"app3">>, <<"0.1.5">>, lte}],
    Pkg2Deps = [{<<"app4">>, <<"3.0.0">>, gte}],
    Pkg3Deps = [{<<"app5">>, <<"2.0.0">>, '>='}],

    World = [{<<"app1">>, [{<<"0.1.0">>, Pkg1Deps},
                           {<<"0.2">>, Pkg1Deps},
                           {<<"3.0">>, Pkg1Deps}]},
             {<<"app2">>, [{<<"0.0.1">>, Pkg2Deps},
                           {<<"0.1">>, Pkg2Deps},
                           {<<"1.0">>, Pkg2Deps},
                           {<<"3.0">>, Pkg2Deps}]},
             {<<"app3">>, [{<<"0.1.0">>, Pkg3Deps},
                           {<<"0.1.3">>, Pkg3Deps},
                           {<<"2.0.0">>, Pkg3Deps},
                           {<<"3.0.0">>, Pkg3Deps},
                           {<<"4.0.0">>, Pkg3Deps}]},
             {<<"app4">>, [{<<"0.1.0">>, [{<<"app5">>, <<"0.1.0">>}]},
                           {<<"0.3.0">>, [{<<"app5">>, <<"0.3.0">>}]},
                           {<<"5.0.0">>, [{<<"app5">>, <<"2.0.0">>}]},
                           {<<"6.0.0">>, [{<<"app5">>, <<"6.0.0">>}]}]},
             {<<"app5">>, [{<<"0.1.0">>, []},
                           {<<"0.3.0">>, []},
                           {<<"2.0.0">>, []},
                           {<<"6.0.0">>, []}]}],

    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{3,0,0}},
                       {<<"app4">>,{5,0,0}},
                       {<<"app5">>,{2,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World,
                                                   [],
                                                   [{<<"app1">>, <<"3.0">>}])),

    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{3,0,0}},
                       {<<"app4">>,{5,0,0}},
                       {<<"app5">>,{2,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World,
                                                   [],
%%                                                   [<<"app5">>, <<"app2">>, <<"app1">>])).
                                                   [<<"app1">>, <<"app2">>, <<"app5">>])).

depsolver_circular_dependencies() ->
    World = [{<<"app1">>, [{<<"0.1.0">>, [<<"app2">>]}]},
             {<<"app2">>, [{<<"0.0.1">>, [<<"app1">>]}]}],

    ?assertEqual({ok, [{<<"app1">>,{0,1,0}},
                       {<<"app2">>,{0,0,1}}]},
                 chef_depsolver:solve_dependencies(World,
                                                   [],
                                                   [{<<"app1">>, <<"0.1.0">>}])).

depsolver_conflicting_failing() ->
    Pkg1Deps = [<<"app2">>,
                {<<"app5">>, <<"2.0.0">>, '='},
                {<<"app4">>, <<"0.3.0">>, gte},
                {<<"app4">>, <<"5.0.0">>, lte}],
    Pkg2Deps = [{<<"app4">>, <<"5.0.0">>, gte}],
    Pkg3Deps = [{<<"app5">>, <<"6.0.0">>}],


    World = [{<<"app1">>, [{<<"3.0">>, Pkg1Deps}]},
             {<<"app2">>, [{<<"0.0.1">>, Pkg2Deps}]},
             {<<"app3">>, [{<<"0.1.0">>, Pkg3Deps}]},
             {<<"app4">>, [{<<"5.0.0">>, [{<<"app5">>, <<"2.0.0">>}]}]},
             {<<"app5">>, [{<<"2.0.0">>, []},
                           {<<"6.0.0">>, []}]}],
    Result = chef_depsolver:solve_dependencies(World, [], [<<"app1">>, <<"app3">>]),
    ?assertMatch({error, no_solution, _}, Result).

depsolver_pessimistic_major_minor_patch() ->

    Pkg1Deps = [{<<"app2">>, <<"2.1.1">>, '~>'},
                {<<"app3">>, <<"0.1.1">>, gte},
                {<<"app3">>, <<"0.1.5">>, lte}],

    Pkg2Deps = [{<<"app4">>, <<"5.0.0">>, gte}],
    Pkg3Deps = [{<<"app5">>, <<"2.0.0">>, '>='}],
    Pkg4Deps = [<<"app5">>],

    World = [{<<"app1">>, [{<<"0.1.0">>, Pkg1Deps},
                           {<<"0.2">>, Pkg1Deps},
                           {<<"3.0">>, Pkg1Deps}]},
             {<<"app2">>, [{<<"0.0.1">>, Pkg2Deps},
                           {<<"0.1">>, Pkg2Deps},
                           {<<"1.0">>, Pkg2Deps},
                           {<<"2.1.5">>, Pkg2Deps},
                           {<<"2.2">>, Pkg2Deps},
                           {<<"3.0">>, Pkg2Deps}]},
             {<<"app3">>, [{<<"0.1.0">>, Pkg3Deps},
                           {<<"0.1.3">>, Pkg3Deps},
                           {<<"2.0.0">>, Pkg3Deps},
                           {<<"3.0.0">>, Pkg3Deps},
                           {<<"4.0.0">>, Pkg3Deps}]},
             {<<"app4">>, [{<<"0.1.0">>, Pkg4Deps},
                           {<<"0.3.0">>, Pkg4Deps},
                           {<<"5.0.0">>, Pkg4Deps},
                           {<<"6.0.0">>, Pkg4Deps}]},
             {<<"app5">>, [{<<"0.1.0">>, []},
                           {<<"0.3.0">>, []},
                           {<<"2.0.0">>, []},
                           {<<"6.0.0">>, []}]}],
    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{2,1,5}},
                       {<<"app4">>,{6,0,0}},
                       {<<"app5">>,{6,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World, [], [{<<"app1">>, <<"3.0">>}])).

depsolver_pessimistic_major_minor() ->

    Pkg1Deps = [{<<"app2">>, <<"2.1">>, '~>'},
                {<<"app3">>, <<"0.1.1">>, gte},
                {<<"app3">>, <<"0.1.5">>, lte}],
    Pkg2Deps = [{<<"app4">>, <<"5.0.0">>, gte}],
    Pkg3Deps = [{<<"app5">>, <<"2.0.0">>, '>='}],
    Pkg4Deps = [<<"app5">>],

    World = [{<<"app1">>, [{<<"0.1.0">>, Pkg1Deps},
                           {<<"0.2">>, Pkg1Deps},
                           {<<"3.0">>, Pkg1Deps}]},
             {<<"app2">>, [{<<"0.0.1">>, Pkg2Deps},
                           {<<"0.1">>, Pkg2Deps},
                           {<<"1.0">>, Pkg2Deps},
                           {<<"2.1.5">>, Pkg2Deps},
                           {<<"2.2">>, Pkg2Deps},
                           {<<"3.0">>, Pkg2Deps}]},
             {<<"app3">>, [{<<"0.1.0">>, Pkg3Deps},
                           {<<"0.1.3">>, Pkg3Deps},
                           {<<"2.0.0">>, Pkg3Deps},
                           {<<"3.0.0">>, Pkg3Deps},
                           {<<"4.0.0">>, Pkg3Deps}]},
             {<<"app4">>, [{<<"0.1.0">>, Pkg4Deps},
                           {<<"0.3.0">>, Pkg4Deps},
                           {<<"5.0.0">>, Pkg4Deps},
                           {<<"6.0.0">>, Pkg4Deps}]},
             {<<"app5">>, [{<<"0.1.0">>, []},
                           {<<"0.3.0">>, []},
                           {<<"2.0.0">>, []},
                           {<<"6.0.0">>, []}]}],
    ?assertEqual({ok, [{<<"app1">>,{3,0,0}},
                       {<<"app2">>,{2,2,0}},
                       {<<"app4">>,{6,0,0}},
                       {<<"app5">>,{6,0,0}},
                       {<<"app3">>,{0,1,3}}]},
                 chef_depsolver:solve_dependencies(World, [], [{<<"app1">>, <<"3.0">>}])).

depsolver_missing() ->
    World = [{<<"app1">>, [{<<"0.1">>, [{<<"app2">>, <<"0.2">>},
                                    {<<"app3">>, <<"0.2">>, '>='},
                                    {<<"app4">>, <<"0.2">>, '='}]},
                           {<<"0.2">>, [{<<"app4">>, <<"0.2">>}]},
                           {<<"0.3">>, [{<<"app4">>, <<"0.2">>, '='}]}]},
             {<<"app2">>, [{<<"0.1">>, []},
                           {<<"0.2">>,[{<<"app3">>, <<"0.3">>}]},
                           {<<"0.3">>, []}]},
             {<<"app3">>, [{<<"0.1">>, []},
                           {<<"0.2">>, []},
                           {<<"0.3">>, []}]}],

    Ret1 = chef_depsolver:solve_dependencies(World,
                                             [],
                                             [{<<"app4">>, <<"0.1">>}, {<<"app3">>, <<"0.1">>}]),
    ?assertMatch({error,invalid_constraints,
                  [{non_existent_cookbooks,[<<"app4">>]},
                   {constraints_not_met,[]}]}, Ret1),

    Ret2 = chef_depsolver:solve_dependencies(World, [], [{<<"app1">>, <<"0.1">>}]),
    ?assertMatch({error,no_solution,_},Ret2).


depsolver_missing_via_culprit_search() ->
    World = [{<<"app1">>,[{<<"1.1.0">>,[]}]},
             {<<"app2">>,[{<<"0.0.1">>,[{<<"app1::oops">>,<<"0.0.0">>,'>='}]} ]} ],
    Result = chef_depsolver:solve_dependencies(World, [], [<<"app1">>,<<"app2">>]),
    ?assertMatch({error,no_solution,_}, Result).

%% This test from the depsolver library may no longer be necessary. It seems to be
%% testing that you can pass binary data to depsolver and it performs the same.
%% I've modified all of the other test to pass data as binary so that they work
%% when converted to Ruby.
depsolver_binary() ->
    World = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
             {<<"bar">>, [{<<"2.0.0">>, [{<<"foo">>, <<"3.0.0">>, gt}]}]}],

    Result = chef_depsolver:solve_dependencies(World, [], [<<"foo">>]),
    ?assertMatch({error, no_solution, _}, Result).

depsolver_no_workers() ->
    World = [{<<"app1">>, [{<<"1.1.0">>}], []}],
    RunList = [<<"app1">>],
    _Steal = pooler:take_member(chef_depsolver),
    ?assertEqual({error, no_depsolver_workers},
                 chef_depsolver:solve_dependencies(World, [], RunList)).

depsolver_negative_version() ->
    World = [{<<"app1">>, [{<<"1.1.-42">>}], []}],
    RunList = [<<"app1">>],
    ?assertMatch({error, exception, _Message, _Backtrace},
                 chef_depsolver:solve_dependencies(World, [], RunList)).

make_env(Name, Deps) ->
    Ejson0 = {[
              {<<"name">>, Name},
              {<<"description">>, <<"test env">>},
              {<<"cookbook_versions">>, Deps}
             ]},
    Json = chef_json:encode(Ejson0),
    {ok, Ejson} = chef_environment:parse_binary_json(Json),
    chef_environment:new_record(?API_MIN_VER, ?OSC_ORG_ID, unset, Ejson).
