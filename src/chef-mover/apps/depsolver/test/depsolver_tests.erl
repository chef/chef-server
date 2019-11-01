%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
%%-------------------------------------------------------------------
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
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%-------------------------------------------------------------------
-module(depsolver_tests).

-include_lib("eunit/include/eunit.hrl").

%%===========================================================================
%% Tests
%%============================================================================
all_test_() ->
  {foreach,
    fun() ->
        error_logger:delete_report_handler(error_logger_tty_h),
        application:start(depsolver)
    end,
    fun(_) -> application:stop(depsolver) end,
    [
      {?MODULE, first},
      {?MODULE, second},
      {?MODULE, third},
      {?MODULE, fail},
      {?MODULE, conflicting_passing},
      {?MODULE, circular_dependencies},
      {?MODULE, conflicting_failing},
      {?MODULE, pessimistic_major_minor_patch},
      {?MODULE, pessimistic_major_minor},
      {?MODULE, filter_packages_with_deps},
      {?MODULE, filter_versions},
      {?MODULE, missing},
      {?MODULE, binary},
      {?MODULE, doesnt_exist},
      {?MODULE, not_new_enough},
      {?MODULE, impossible_dependency},
      {?MODULE, integration},
      {?MODULE, missing_via_culprit_search},
      {generator, ?MODULE, format},
      {generator, ?MODULE, missing2}
  ]
}.

%% event_logging_test_() ->
%%   {foreach,
%%     fun() ->
%%             error_logger:delete_report_handler(error_logger_tty_h),
%%             application:start(depsolver),
%%             depsolver_event_logger:add_handler([{root_dir, "/tmp/depsolver-test"},
%%                                                 {max_files, 4}])
%%     end,
%%     fun(_) ->
%%             application:stop(depsolver),
%%             %% ugly, hack
%%             os:cmd("rm -rf /tmp/depsolver-test")
%%     end,
%%     [
%%      {"event logs are captured",
%%       fun() ->
%%               %% Six ok results, should only record a total of four
%%               [ first() || _I <- lists:seq(1, 6) ],

%%               %% Three error results
%%               [ fail() || _I <- lists:seq(1, 3) ],

%%               %% 21 unreachable
%%               Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2"}]}]}]),
%%               [ depsolver:solve(Dom0, [{app4, "8"}]) || _I <- lists:seq(1, 21) ],

%%               OkExpect = [ "/tmp/depsolver-test/ok/" ++ integer_to_list(I) || I <- lists:seq(1, 4) ],
%%               ?assertEqual(OkExpect, filelib:wildcard("/tmp/depsolver-test/ok/*")),

%%               ErrorExpect = [ "/tmp/depsolver-test/error/" ++ integer_to_list(I) || I <- lists:seq(1, 3) ],
%%               ?assertEqual(ErrorExpect, filelib:wildcard("/tmp/depsolver-test/error/*")),

%%               UnreachableExpect = [ "/tmp/depsolver-test/unreachable/" ++ integer_to_list(I)
%%                                     || I <- lists:seq(1, 4) ],
%%               ?assertEqual(UnreachableExpect, filelib:wildcard("/tmp/depsolver-test/unreachable/*")),

%%               %% we don't have a timeout test at present
%%               ?assertEqual([], filelib:wildcard("/tmp/depsolver-test/timeout/*"))
%%       end},

%%      {"removing handler prevent event log capture",
%%       fun() ->
%%               depsolver_event_logger:remove_handler(),
%%               %% Six ok results
%%               [ first() || _I <- lists:seq(1, 6) ],
%%               ?assertEqual([], filelib:wildcard("/tmp/depsolver-test/ok/*"))
%%       end}
%%     ]
%%   }.

first() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2+build.33"},
                                                                           {app3, "0.2", '>='}]},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]},
                                                          {app2, [{"0.1", []},
                                                                  {"0.2+build.33",[{app3, "0.3"}]},
                                                                  {"0.3", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]}]),


    case depsolver:solve(Dom0, [{app1, "0.1"}]) of
        {ok,[{app3,{{0,3},{[],[]}}},
             {app2,{{0,2},{[],[<<"build">>,33]}}},
             {app1,{{0,1},{[],[]}}}]} ->
            ok;
        E ->
            erlang:throw({invalid_result, E})
    end.

second() ->

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.1", '>='},
                                                       {app4, "0.2"},
                                                       {app3, "0.2", '>='}]},
                                              {"0.2", []},
                                              {"0.3", []}]},
                                      {app2, [{"0.1", [{app3, "0.2", gte}]},
                                              {"0.2", [{app3, "0.2", gte}]},
                                              {"0.3", [{app3, "0.2", '>='}]}]},
                                      {app3, [{"0.1", [{app4, "0.2", '>='}]},
                                              {"0.2", [{app4, "0.2"}]},
                                              {"0.3", []}]},
                                      {app4, [{"0.1", []},
                                              {"0.2", [{app2, "0.2", gte},
                                                       {app3, "0.3"}]},
                                              {"0.3", []}]}]),

    X = depsolver:solve(Dom0, [{app1, "0.1"},
                     {app2, "0.3"}]),

    ?assertMatch({ok, [{app3,{{0,3},{[],[]}}},
                       {app2,{{0,3},{[],[]}}},
                       {app4,{{0,2},{[],[]}}},
                       {app1,{{0,1},{[],[]}}}]},
                 X).

third() ->

    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),

    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),


    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [app1])).

fail() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(),
                                  [{app1, [{"0.1", [{app2, "0.2"},
                                                    {app3, "0.2", gte}]},
                                           {"0.2", []},
                                           {"0.3", []}]},
                                   {app2, [{"0.1", []},
                                           {"0.2",[{app3, "0.1"}]},
                                           {"0.3", []}]},
                                   {app3, [{"0.1", []},
                                           {"0.2", []},
                                           {"0.3", []}]}]),

    Ret = depsolver:solve(Dom0, [{app1, "0.1"}]),
    %% We do this to make sure all errors can be formated.
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[{app1,{{0,1},{[],[]}}}],
                      [{app1,{{0,1},{[],[]}}},[[{app2,{{0,2},{[],[]}}}]]]}],
                    [{{app2,{{0,2},{[],[]}}},[{app3,{{0,1},{[],[]}}}]},
                     {{app1,{{0,1},{[],[]}}},[{app3,{{0,2},{[],[]}},gte}]}]}]},
                 Ret).

conflicting_passing() ->
    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "3.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", [{app5, "0.1.0"}]},
                                              {"0.3.0", [{app5, "0.3.0"}]},
                                              {"5.0.0", [{app5, "2.0.0"}]},
                                              {"6.0.0", [{app5, "6.0.0"}]}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),

    ?assertMatch({ok, [{app5,{{2,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{5,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),

    ?assertMatch({ok, [{app5,{{2,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{5,0,0},{[],[]}}},
                       {app2,{{3,0},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [app1, app2, app5])).



circular_dependencies() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", [app2]}]},
                                      {app2, [{"0.0.1", [app1]}]}]),

    ?assertMatch({ok, [{app1,{{0,1,0},{[],[]}}},{app2,{{0,0,1},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "0.1.0"}])).

conflicting_failing() ->
    Pkg1Deps = [app2,
                {app5, "2.0.0", '='},
                {app4, "0.3.0", "5.0.0", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "6.0.0"}],


    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps}]},
                                      {app4, [{"5.0.0", [{app5, "2.0.0"}]}]},
                                      {app5, [{"2.0.0", []},
                                              {"6.0.0", []}]}]),
    Ret = depsolver:solve(Dom0, [app1, app3]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                   [{[{[app1],
                       [{app1,{{3,0},{[],[]}}},
                        [[{app4,{{5,0,0},{[],[]}}}],
                         [{app2,{{0,0,1},{[],[]}}},[[{app4,{{5,0,0},{[],[]}}}]]]]]},
                      {[app3],
                       [{app3,{{0,1,0},{[],[]}}},[[{app5,{{6,0,0},{[],[]}}}]]]}],
                     [{{app4,{{5,0,0},{[],[]}}},[{app5,{{2,0,0},{[],[]}}}]},
                      {{app1,{{3,0},{[],[]}}},[{app5,{{2,0,0},{[],[]}},'='}]}]}]},
                 Ret).


pessimistic_major_minor_patch() ->

    Pkg1Deps = [{app2, "2.1.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"2.1.5", Pkg2Deps},
                                              {"2.2", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),
    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{2,1,5},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

pessimistic_major_minor() ->

    Pkg1Deps = [{app2, "2.1", '~>'},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

     Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                              {"0.2", Pkg1Deps},
                                              {"3.0", Pkg1Deps}]},
                                      {app2, [{"0.0.1", Pkg2Deps},
                                              {"0.1", Pkg2Deps},
                                              {"1.0", Pkg2Deps},
                                              {"2.1.5", Pkg2Deps},
                                              {"2.2", Pkg2Deps},
                                              {"3.0", Pkg2Deps}]},
                                      {app3, [{"0.1.0", Pkg3Deps},
                                              {"0.1.3", Pkg3Deps},
                                              {"2.0.0", Pkg3Deps},
                                              {"3.0.0", Pkg3Deps},
                                              {"4.0.0", Pkg3Deps}]},
                                      {app4, [{"0.1.0", Pkg4Deps},
                                              {"0.3.0", Pkg4Deps},
                                              {"5.0.0", Pkg4Deps},
                                              {"6.0.0", Pkg4Deps}]},
                                      {app5, [{"0.1.0", []},
                                              {"0.3.0", []},
                                              {"2.0.0", []},
                                              {"6.0.0", []}]}]),
    ?assertMatch({ok, [{app5,{{6,0,0},{[],[]}}},
                       {app3,{{0,1,3},{[],[]}}},
                       {app4,{{6,0,0},{[],[]}}},
                       {app2,{{2,2},{[],[]}}},
                       {app1,{{3,0},{[],[]}}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

filter_packages_with_deps() ->
    Packages = [{app1, [{"0.1", [{app2, "0.2"},
                                 {app3, "0.2", '>='},
                                 {app4, "0.2", '='}]},
                        {"0.2", [{app4, "0.2"}]},
                        {"0.3", [{app4, "0.2", '='}]}]},
                {app2, [{"0.1", []},
                        {"0.2",[{app3, "0.3"}]},
                        {"0.3", []}]},
                {app3, [{"0.1", []},
                        {"0.2", []},
                        {"0.3", []}]}],

    %% constrain app1 and app3
    Cons = [{app1, "0.1", '='},
            {app3, "0.1", '>'}],
    ?assertEqual({ok, [{app1, [{"0.1", [{app2, "0.2"},
                                        {app3, "0.2", '>='},
                                        {app4, "0.2", '='}]}]},
                       {app2, [{"0.1", []},
                               {"0.2",[{app3, "0.3"}]},
                               {"0.3", []}]},
                       {app3, [{"0.2", []},
                               {"0.3", []}]}]},
                 depsolver:filter_packages_with_deps(Packages, Cons)),
    %% a constraint that doesn't matter
    ?assertEqual({ok, Packages},
                 depsolver:filter_packages_with_deps(Packages, [{appX, "4.0"}])),
    %% no constraints
    ?assertEqual({ok, Packages},
                 depsolver:filter_packages_with_deps(Packages, [])),

    %% remove everything constraints
    ?assertEqual({ok, []},
                 depsolver:filter_packages_with_deps(Packages,
                                                     [{app1, "40.0"},
                                                      {app2, "40.0"},
                                                      {app3, "40.0"}])),

    Ret = depsolver:filter_packages_with_deps(Packages, [{<<"ick">>, "1.0.0", '~~~~'}]),
    Expect = {error, {invalid_constraints, [{<<"ick">>, {{1, 0, 0}, {[], []}}, '~~~~'}]}},
    ?assertEqual(Expect, Ret).

filter_versions() ->

    Cons = [{app2, "2.1", '~>'},
            {app3, "0.1.1", "0.1.5", between},
            {app4, "5.0.0", gte},
            {app5, "2.0.0", '>='},
            app5],

    Packages = [{app1, "0.1.0"},
                {app1, "0.2"},
                {app1, "0.2"},
                {app1, "3.0"},
                {app2, "0.0.1"},
                {app2, "0.1"},
                {app2, "1.0"},
                {app2, "2.1.5"},
                {app2, "2.2"},
                {app2, "3.0"},
                {app3, "0.1.0"},
                {app3, "0.1.3"},
                {app3, "2.0.0"},
                {app3, "3.0.0"},
                {app3, "4.0.0"},
                {app4, "0.1.0"},
                {app4, "0.3.0"},
                {app4, "5.0.0"},
                {app4, "6.0.0"},
                {app5, "0.1.0"},
                {app5, "0.3.0"},
                {app5, "2.0.0"},
                {app5, "6.0.0"}],

    ?assertMatch({ok, [{app1,"0.1.0"},
                       {app1,"0.2"},
                       {app1,"0.2"},
                       {app1,"3.0"},
                       {app2,"2.1.5"},
                       {app2,"2.2"},
                       {app3,"0.1.3"},
                       {app4,"5.0.0"},
                       {app4,"6.0.0"},
                       {app5,"2.0.0"},
                       {app5,"6.0.0"}]},
                 depsolver:filter_packages(Packages, Cons)),

    Ret = depsolver:filter_packages(Packages,
                                    [{"foo", "1.0.0", '~~~~'} | Cons]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error, {invalid_constraints, [{<<"foo">>,{{1,0,0},{[],[]}},'~~~~'}]}}, Ret).


-spec missing() -> ok.
missing() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                             {app3, "0.2", '>='},
                                                             {app4, "0.2", '='}]},
                                                                  {"0.2", [{app4, "0.2"}]},
                                                                  {"0.3", [{app4, "0.2", '='}]}]},
                                                          {app2, [{"0.1", []},
                                                                  {"0.2",[{app3, "0.3"}]},
                                                                  {"0.3", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]}]),
    Ret1 = depsolver:solve(Dom0, [{app4, "0.1"}, {app3, "0.1"}]),
    _ = depsolver:format_error(Ret1),
    ?assertMatch({error,{unreachable_package,app4}}, Ret1),

    Ret2 = depsolver:solve(Dom0, [{app1, "0.1"}]),
    _ = depsolver:format_error(Ret2),
    ?assertMatch({error,_},
                 Ret2).

missing_via_culprit_search() ->
    World = [{<<"app1">>,[{"1.1.0",[]}]},
             {<<"app2">>,[{"0.0.1",[{<<"app1::oops">>,<<"0.0.0">>,'>='}]} ]} ],
    Dom0 = depsolver:add_packages(depsolver:new_graph(), World),
    Ret1 = depsolver:solve(Dom0, [<<"app1">>,<<"app2">>]),
    _ = depsolver:format_error(Ret1),
    ?assertMatch({error,{unreachable_package,<<"app1::oops">>}}, Ret1).

binary() ->

    World = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
             {<<"bar">>, [{<<"2.0.0">>, [{<<"foo">>, <<"3.0.0">>, gt}]}]}],
    Ret = depsolver:solve(depsolver:add_packages(depsolver:new_graph(),
                                               World),
                        [<<"foo">>]),

    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

%%
%% We don't have bar cookbook
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar, which does not
%%   exist, due to run list item (foo >= 0.0.0).  Run list items that may result
%%   in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
%%   "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":["bar"],"
%% "most_constrained_cookbooks":[]}"
%%
doesnt_exist() ->
    Constraints = [{<<"foo">>,[{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]}],
    World = depsolver:add_packages(depsolver:new_graph(), Constraints),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,_}, Ret).

%%
%% We have v 2.0.0 of bar but want > 2.0.0
%%
%% Ruby gives
%% "message":"Unable to satisfy constraints on cookbook bar due to run list item
%% (foo >= 0.0.0).  Run list items that may result in a constraint on bar: [(foo
%% = 1.2.3) -> (bar > 2.0.0)]",
%% "unsatisfiable_run_list_item":"(foo >= 0.0.0)",
%% "non_existent_cookbooks":[],
%% "most_constrained_cookbooks":["bar 2.0.0 -> []"]
%%
not_new_enough() ->

    Constraints = [{<<"foo">>, [{<<"1.2.3">>, [{<<"bar">>, <<"2.0.0">>, gt}]}]},
                   {<<"bar">>, [{<<"2.0.0">>, []}]}],
    World = depsolver:add_packages(depsolver:new_graph(), Constraints),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

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
impossible_dependency() ->
    World = depsolver:add_packages(depsolver:new_graph(),
                                   [{<<"foo">>, [{<<"1.2.3">>,[{ <<"bar">>, <<"2.0.0">>, gt}]}]},
                                    {<<"bar">>, [{<<"2.0.0">>, [{ <<"foo">>, <<"3.0.0">>, gt}]}]}]),
    Ret = depsolver:solve(World, [<<"foo">>]),
    _ = depsolver:format_error(Ret),
    ?assertMatch({error,
                  [{[{[<<"foo">>],[{<<"foo">>,{{1,2,3},{[],[]}}}]}],
                    [{{<<"foo">>,{{1,2,3},{[],[]}}},
                      [{<<"bar">>,{{2,0,0},{[],[]}},gt}]}]}]}, Ret).

%%
%% Formatting tests
%%
format() ->
      [{"format_version returns iolist",
        [?_assertEqual(["1", [], []], depsolver:format_version({1, {[],[]}})),
         ?_assertEqual(["1", ".", "2", ".", "34", [], []], depsolver:format_version({{1,2,34},{[],[]}}))
        ]
       },
       {"format_version",
        [equal_bin_string(<<"1">>, depsolver:format_version({1, {[],[]}})),
         equal_bin_string(<<"1.2">>, depsolver:format_version({{1,2}, {[],[]}})),
         equal_bin_string(<<"1.2.2">>, depsolver:format_version({{1,2,2}, {[],[]}})),
         equal_bin_string(<<"1.99.2">>, depsolver:format_version({{1,99,2}, {[],[]}})),
         equal_bin_string(<<"1.99.2-alpha">>, depsolver:format_version({{1,99,2}, {["alpha"],[]}})),
         equal_bin_string(<<"1.99.2-alpha.1">>, depsolver:format_version({{1,99,2}, {["alpha",1], []}})),
         equal_bin_string(<<"1.99.2+build.1.a36">>,
                          depsolver:format_version({{1,99,2}, {[], ["build", 1, "a36"]}})),
         equal_bin_string(<<"1.99.2-alpha.1+build.1.a36">>,
                          depsolver:format_version({{1,99,2}, {["alpha", 1], ["build", 1, "a36"]}})),
         equal_bin_string(<<"1">>, depsolver:format_version({1, {[],[]}}))]
       },
       {"format constraint",
        [equal_bin_string(<<"foo">>, depsolver:format_constraint(<<"foo">>)),
         equal_bin_string(<<"foo">>, depsolver:format_constraint(foo)),
         equal_bin_string(<<"(foo = 1.2.0)">>, depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}})),
         equal_bin_string(<<"(foo = 1.2.0)">>, depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '='})),
         equal_bin_string(<<"(foo > 1.2.0)">>,
                          depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, '>'})),
         equal_bin_string(<<"(foo > 1.2.0)">>,
                          depsolver:format_constraint({<<"foo">>, {{1,2,0}, {[], []}}, gt})),
         equal_bin_string(<<"(foo between 1.2.0 and 1.3.0)">>,
                          depsolver:format_constraint({<<"foo">>,{{1,2,0}, {[], []}},
                                                       {{1,3,0}, {[], []}}, between})),
         equal_bin_string(<<"(foo > 1.2.0-alpha.1+build.36)">>,
                          depsolver:format_constraint({<<"foo">>,
                                                       {{1,2,0}, {["alpha", 1], ["build", 36]}}, gt}))
        ]
       },
       {"format roots",
        [equal_bin_string(<<"(bar = 1.2.0)">>,
                          depsolver:format_roots([ [{<<"bar">>, {{1,2,0},{[],[]}}}] ])),
         equal_bin_string(<<"(bar = 1.2.0), foo">>,
                          depsolver:format_roots([[<<"foo">>,
                                                   {<<"bar">>, {{1,2,0},{[],[]}}}]])),
         equal_bin_string(<<"(bar = 1.2.0), foo">>,
                          depsolver:format_roots([[<<"foo">>], [{<<"bar">>, {{1,2,0},{[],[]}}}]]))
        ]
       }
      ].


integration() ->
  Arg1 = {depsolver, {26, {<<"users">>, [{{{1,0,0},{[],[]}},[]}], {<<"openssl">>, [{{{1,0,0},{[],[]}},[]}, {{{0,1,0},{[],[]}},[]}], {<<"macbook">>, [{{{0,1,0},{[],[]}},[]}, {{{0,0,0},{[],[]}},[]}], {<<"java_sun">>, [{{{0,10,0},{[],[]}}, [{<<"java">>, {{0,0,0},{[],[]}}, '>='}]}], {<<"dbapp">>, [{{{0,1,0},{[],[]}},[]}], {<<"build-essential">>, [{{{1,0,2},{[],[]}},[]}], {<<"aws">>, [{{{0,9,0},{[],[]}},[]}], {<<"apt">>, [{{{1,10,0},{[],[]}},[]}], {<<"apache2">>, [{{{1,6,2},{[],[]}},[]}], nil,nil}, nil}, nil}, {<<"chef_handler">>, [{{{1,0,6},{[],[]}},[]}], nil,nil}}, {<<"edb_demo">>, [{{{0,0,1},{[],[]}},[]}], {<<"dbserver">>, [{{{0,1,0},{[],[]}},[]}], nil,nil}, {<<"java">>, [{{{1,1,0},{[],[]}}, [{<<"apt">>, {{0,0,0},{[],[]}}, '>='}]}], nil,nil}}}, {<<"jpackage">>, [{{{0,10,0},{[],[]}}, [{<<"java">>, {{0,0,0},{[],[]}}, '>='}]}], nil,nil}}, {<<"networking_basic">>, [{{{0,0,5},{[],[]}},[]}], {<<"mysql">>, [{{{1,2,6},{[],[]}}, [{<<"windows">>, {{0,0,0},{[],[]}}, '>='}, {<<"openssl">>, {{0,0,0},{[],[]}}, '>='}]}, {{{1,0,2},{[],[]}}, [{<<"openssl">>, {{0,0,0},{[],[]}}, '>='}]}, {{{0,24,4},{[],[]}}, [{<<"openssl">>, {{0,0,0},{[],[]}}, '>='}]}], nil,nil}, {<<"ntp">>, [{{{1,0,0},{[],[]}},[]}], nil,nil}}}, {<<"testcb">>, [{{{0,1,1},{[],[]}},[]}, {{{0,1,0},{[],[]}}, [{<<"deptest">>, {{0,0,0},{[],[]}}, '>='}]}], {<<"runit">>, [{{{0,14,2},{[],[]}},[]}], {<<"php">>, [{{{1,0,2},{[],[]}}, [{<<"xml">>, {{0,0,0},{[],[]}}, '>='}, {<<"mysql">>, {{0,0,0},{[],[]}}, '>='}, {<<"build-essential">>, {{0,0,0},{[],[]}}, '>='}]}, {{{0,9,1},{[],[]}}, [{<<"apache2">>, {{0,0,0},{[],[]}}, '>='}]}], nil,nil}, {<<"test123">>, [{{{0,0,1},{[],[]}},[]}], nil,nil}}, {<<"tomcat">>, [{{{0,10,3},{[],[]}}, [{<<"java">>, {{0,0,0},{[],[]}}, '>='}, {<<"jpackage">>, {{0,0,0},{[],[]}}, '>='}]}], {<<"testcookbook">>, [{{{0,0,1},{[],[]}},[]}], nil,nil}, nil}}}, {<<"wordpress">>, [{{{0,8,8},{[],[]}}, [{<<"php">>, {{0,0,0},{[],[]}}, '>='}, {<<"mysql">>, {{1,0,5},{[],[]}}, '>='}, {<<"openssl">>, {{0,0,0},{[],[]}}, '>='}, {<<"apache2">>, {{0,99,4},{[],[]}}, '>='}]}], {<<"windows">>, [{{{1,3,0},{[],[]}}, [{<<"chef_handler">>, {{0,0,0},{[],[]}}, '>='}]}], nil,nil}, {<<"xml">>, [{{{1,0,2},{[],[]}},[]}], nil,nil}}}}},
  Arg2 = [<<"testcb">>],
  ?assertMatch({ok, _}, depsolver:solve(Arg1,Arg2)).

missing2() ->
    %% noapp is missing, but referenced.
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [
                                                                  %% direct dep on noapp
                                                                  {"0.1", [{noapp, "0.1", '>='}]},
                                                                  %% exact dep on app2 which depends on noapp
                                                                  {"0.2", [{app2, "0.1"}]},
                                                                  %% will take any version of app2
                                                                  {"0.3", [{app2, "0.1", '>='}]},
                                                                  {"0.4", [{app2, "0.3", '<='}]}
                                                                 ]},
                                                          {app2, [{"0.1", [{noapp, "0.1"}]},
                                                                  {"0.2",[]},
                                                                  {"0.3", [{app3, "0.2"}]},
                                                                  {"0.4", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", [{noapp, "0.1"}]},
                                                                  {"0.3", []}]},
                                                          {app4, [{"0.1", [{app3, "100"}]}]}
                                                         ]),

    [
     %% should fail because direct dep not found
     ?_assertMatch({error, _},
                 depsolver:solve(Dom0, [{app1, "0.1"}])),

     %% should fail because dep of dep not found
     ?_assertMatch({error, _},
                  depsolver:solve(Dom0, [{app1, "0.2"}])),

     %% should fail because dep of dep not found
     ?_assertMatch({error, _},
                  depsolver:solve(Dom0, [{app1, "0.3"}, {non_existent, "0.0.0"}])),

     %% should fail, pkg exists, but not at version
     ?_assertMatch({error, _},
                  depsolver:solve(Dom0, [{app4, "0.1"}])),

     %% should end up with app1 0.3, app2 0.4
     ?_assertEqual({ok,[{app2,{{0,4},{[],[]}}},{app1,{{0,3},{[],[]}}}]},
                  depsolver:solve(Dom0, [{app1, "0.3"}])),

     %% Since latest version of app2 is unreachable due to missing dep, we
     %% expect app1 0.4 with app2 0.2.
	 %% Current implementation fails on first missing package.  This is a
	 %% compromise solution until depsolver can be enhanced to handle this
	 %% better.
     ?_assertEqual({ok,[{app2,{{0,2},{[],[]}}},{app1,{{0,4},{[],[]}}}]},
	 %?_assertEqual({error, {unreachable_package, noapp}},
                   depsolver:solve(Dom0, [{app1, "0.4"}])),

     %% should end up with app1 0.3, app2 0.4
     ?_assertEqual({ok,[{app2,{{0,2},{[],[]}}},{app1,{{0,4},{[],[]}}}]},
     %?_assertEqual({ok,[{app2,{{0,4},{[],[]}}},{app1,{{0,3},{[],[]}}}]},
                  depsolver:solve(Dom0, [{app1, "0.4", '<='}]))
    ].
%%
%% Internal functions
%%
equal_bin_string(Expected, Got) ->
  ?_assertEqual(Expected, erlang:iolist_to_binary(Got)).
