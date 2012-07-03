%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(depsolver_tests).

-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Tests
%%============================================================================

first_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                                           {app3, "0.2", '>='}]},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]},
                                                          {app2, [{"0.1", []},
                                                                  {"0.2",[{app3, "0.3"}]},
                                                                  {"0.3", []}]},
                                                          {app3, [{"0.1", []},
                                                                  {"0.2", []},
                                                                  {"0.3", []}]}]),


    case depsolver:solve(Dom0, [{app1, "0.1"}]) of
        {ok, [{app3,{0,3}},
              {app2,{0,2}},
              {app1,{0,1}}]} ->
            ok;
        E ->
            erlang:throw({invalid_result, E})
    end.

second_test() ->

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

    ?assertMatch({ok, [{app3,{0,3}},
                       {app2,{0,3}},
                       {app4,{0,2}},
                       {app1,{0,1}}]},
                 X).

third_test() ->

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

    ?assertMatch({ok, [{app5,{6,0,0}},
                       {app3,{0,1,3}},
                       {app4,{6,0,0}},
                       {app2,{3,0}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),


    ?assertMatch({ok, [{app5,{6,0,0}},
                       {app3,{0,1,3}},
                       {app4,{6,0,0}},
                       {app2,{3,0}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [app1])).

fail_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                       {app3, "0.2", gte}]},
                                              {"0.2", []},
                                              {"0.3", []}]},
                                      {app2, [{"0.1", []},
                                              {"0.2",[{app3, "0.1"}]},
                                              {"0.3", []}]},
                                      {app3, [{"0.1", []},
                                              {"0.2", []},
                                              {"0.3", []}]}]),


    ?assertMatch({error, {unable_to_solve, {app1,{0,1}},
                          {[], [], [{app1, {0,1}}]}}},
                 depsolver:solve(Dom0, [{app1, {0,1}}])).

conflicting_passing_test() ->
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

    ?assertMatch({ok, [{app5,{2,0,0}},
                       {app3,{0,1,3}},
                       {app4,{5,0,0}},
                       {app2,{3,0}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])),

    ?assertMatch({ok, [{app5,{2,0,0}},
                       {app3,{0,1,3}},
                       {app4,{5,0,0}},
                       {app2,{3,0}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [app1, app2, app5])).



circular_dependencies_test() ->
    Dom0 = depsolver:add_packages(depsolver:new_graph(), [{app1, [{"0.1.0", [app2]}]},
                                      {app2, [{"0.0.1", [app1]}]}]),

    ?assertMatch({ok, [{app1,{0,1,0}},{app2,{0,0,1}}]},
                 depsolver:solve(Dom0, [{app1, "0.1.0"}])).

conflicting_failing_test() ->
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

    ?assertMatch({error, {unable_to_solve,app3,{[],[],[app1,app3]}}},
                 depsolver:solve(Dom0, [app1, app3])).


pessimistic_major_minor_patch_test() ->

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
    ?assertMatch({ok, [{app5,{6,0,0}},
                       {app3,{0,1,3}},
                       {app4,{6,0,0}},
                       {app2,{2,1,5}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

pessimistic_major_minor_test() ->

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
    ?assertMatch({ok, [{app5,{6,0,0}},
                       {app3,{0,1,3}},
                       {app4,{6,0,0}},
                       {app2,{2,2}},
                       {app1,{3,0}}]},
                 depsolver:solve(Dom0, [{app1, "3.0"}])).

filter_versions_test() ->

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
    ?assertMatch({error, {invalid_constraints, [{<<"foo">>,{1,0,0},'~~~~'}]}},
                 depsolver:filter_packages(Packages,
                                           [{"foo", "1.0.0", '~~~~'} | Cons])).

-spec missing_test() -> ok.
missing_test() ->

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


    ?assertMatch({error, {unreachable_package,app4}},
                 depsolver:solve(Dom0, [{app4, "0.1"}, {app3, "0.1"}])),

    ?assertMatch({error, {unreachable_package,app4}},
                 depsolver:solve(Dom0, [{app1, "0.1"}])).
