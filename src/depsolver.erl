%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sx=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%% This is a dependency constraint solver. You add your 'world' the
%%% solver. That is the packages that exist, thier versions and their
%%% dependencies. Then give the system a set of targets and ask it to solve.
%%%
%%%  Lets say our world looks as follows
%%%
%%%      app1 that has versions "0.1"
%%%        depends on app3 any version greater then "0.2"
%%%       "0.2" with no dependencies
%%%       "0.3" with no dependencies
%%%
%%%      app2 that has versions "0.1" with no dependencies
%%%       "0.2" that depends on app3 exactly "0.3"
%%%       "0.3" with no dependencies
%%%
%%%      app3 that has versions
%%%       "0.1", "0.2" and "0.3" all with no dependencies
%%%
%%% we can add this world to the system all at once ass follows
%%%
%%%      Graph0 = depsolver:new_graph(),
%%%      Graph1 = depsolver:add_packages(
%%%             [{app1, [{"0.1", [{app2, "0.2"},
%%%                               {app3, "0.2", '>='}]},
%%%                               {"0.2", []},
%%%                               {"0.3", []}]},
%%%              {app2, [{"0.1", []},
%%%                       {"0.2",[{app3, "0.3"}]},
%%%                       {"0.3", []}]},
%%%              {app3, [{"0.1", []},
%%%                      {"0.2", []},
%%%                      {"0.3", []}]}]).
%%%
%%% We can also build it up incrementally using the other add_package and
%%% add_package_version functions.
%%%
%%% Finally, once we have built up the graph we can ask depsolver to solve the
%%% dependency constraints. That is to give us a list of valid dependencies by
%%% using the solve function. Lets say we want the app3 version "0.3" and all of
%%% its resolved dependencies. We could call solve as follows.
%%%
%%%    depsolver:solve(Graph1, [{app3, "0.3"}]).
%%%
%%% That will give us the completely resolved dependencies including app3
%%% itself. Lets be a little more flexible. Lets ask for a graph that is rooted
%%% in anything greater then or equal to app3 "0.3". We could do that by
%%%
%%%    depsolver:solve(Graph1, [{app3, "0.3", '>='}]).
%%%
%%% Of course, you can specify any number of goals at the top level.
%%% @end
%%%-------------------------------------------------------------------
-module(depsolver).

%% Public Api
-export([new_graph/0,
         solve/2,
         add_packages/2,
         add_package/3,
         add_package_version/3,
         add_package_version/4]).

%% Internally Exported API. This should *not* be used outside of the depsolver
%% application. You have been warned.
-export([dep_pkg/1,
         primitive_solve/2,
         is_version_within_constraint/2]).

-export_type([t/0,
              pkg/0,
              pkg_name/0,
              vsn/0,
              constraint/0,
              dependency_set/0]).

-export_type([constraints/0]).
%%============================================================================
%% type
%%============================================================================
-type dep_graph() :: gb_tree().
-opaque t() :: {?MODULE, dep_graph()}.
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: string() | atom().
-type vsn() :: string().
-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), gte}
                    | {pkg_name(), vsn(),'>='}
                    | {pkg_name(), vsn(), lte}
                    | {pkg_name(), vsn(), '<='}
                    | {pkg_name(), vsn(), gt}
                    | {pkg_name(), vsn(), '>'}
                    | {pkg_name(), vsn(), lt}
                    | {pkg_name(), vsn(), '<'}
                    | {pkg_name(), vsn(), '~>'}
                    | {pkg_name(), vsn(), vsn(), between}.

-type vsn_constraint() :: {vsn(), [constraint()]}.
-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.

%% Internal Types
-type constraints() :: [constraint()].

%%============================================================================
%% API
%%============================================================================
%% @doc create a new empty dependency graph
-spec new_graph() -> t().
new_graph() ->
    {?MODULE, gb_trees:empty()}.

%% @doc add a complete set of list of packages to the graph. Where the package
%% consists of the name and a list of versions and dependencies.
%%
%% ``` depsolver:add_packages(Graph,
%%               [{app1, [{"0.1", [{app2, "0.2"},
%%                                 {app3, "0.2", '>='}]},
%%                                 {"0.2", []},
%%                                 {"0.3", []}]},
%%                 {app2, [{"0.1", []},
%%                         {"0.2",[{app3, "0.3"}]},
%%                         {"0.3", []}]},
%%                 {app3, [{"0.1", []},
%%                         {"0.2", []},
%%                         {"0.3", []}]}])
%% '''
-spec add_packages(t(),[dependency_set()]) -> t().
add_packages(Dom0, Info)
  when is_list(Info) ->
    lists:foldl(fun({Pkg, VsnInfo}, Dom1) ->
                        add_package(Dom1, Pkg, VsnInfo)
                end, Dom0, Info).

%% @doc add a single package to the graph, where it consists of a package name
%% and its versions and thier dependencies.
%%  ```depsolver:add_package(Graph, app1, [{"0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]}]).
%% '''
-spec add_package(t(),pkg_name(),[vsn_constraint()]) -> t().
add_package(State, Pkg, Versions)
  when is_list(Versions) ->
    lists:foldl(fun({Vsn, Constraints}, Dom1) ->
                        add_package_version(Dom1, Pkg, Vsn, Constraints);
                   (Version, Dom1) ->
                        add_package_version(Dom1, Pkg, Version, [])
                end, State, Versions).

%% @doc add a set of dependencies to a specific package and version.
%% and its versions and thier dependencies.
%%  ```depsolver:add_package(Graph, app1, "0.1", [{app2, "0.2"},
%%                                              {app3, "0.2", '>='}]},
%%                                              {"0.2", []},
%%                                              {"0.3", []}]).
%% '''
-spec add_package_version(t(),pkg_name(),vsn(),[constraint()]) -> t().
add_package_version({?MODULE, Dom0}, Pkg, Vsn, PkgConstraints) ->
   Info2 =
        case gb_trees:lookup(Pkg, Dom0) of
            {value, Info0} ->
                case lists:keytake(Vsn, 1, Info0) of
                    {value, {Vsn, Constraints}, Info1} ->
                        [{Vsn, join_constraints(Constraints, PkgConstraints)}
                         | Info1];
                    false ->
                        [{Vsn,  PkgConstraints} | Info0]
                end;
            none ->
                [{Vsn, PkgConstraints}]
        end,
    {?MODULE, gb_trees:enter(Pkg, Info2, Dom0)}.

%% @doc add a package and version to the dependency graph with no dependency
%% constraints, dependency constraints can always be added after the fact.
%%
%% ```depsolver:add_package_version(Graph, app1, "0.1").'''
-spec add_package_version(t(),pkg_name(),vsn()) -> t().
add_package_version(State, Pkg, Vsn) ->
    add_package_version(State, Pkg, Vsn, []).

%% @doc Given a set of goals (in the form of constrains) find a set of packages
%% and versions that satisfy all constraints. If no solution can be found then
%% an exception is thrown.
%% ``` depsolver:solve(State, [{app1, "0.1", '>='}]).'''
-spec solve(t(),[constraint()]) -> {ok, [pkg()] | []} | {error, term()}.
solve({?MODULE, DepGraph0}, Goals)
  when erlang:length(Goals) > 0 ->
    case trim_unreachable_packages(DepGraph0, Goals) of
        Error = {error, _} ->
            Error;
        DepGraph1 ->
            case primitive_solve(DepGraph1, Goals) of
                fail ->
                    [FirstCons | Rest] = Goals,
                    {error, depsolver_culprit:search(DepGraph1, [FirstCons], Rest)};
                Solution ->
                    {ok, Solution}
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc given two lists of constraints join them in such a way that no
%% constraint is duplicated but the over all order of the constraints is
%% preserved. Order drives priority in this solver and is important for that
%% reason.
-spec join_constraints([constraint()],[constraint()]) -> [constraint()].
join_constraints(NewConstraints, ExistingConstraints) ->
    ECSet = sets:from_list(ExistingConstraints),
    FilteredNewConstraints = [NC || NC <- NewConstraints,
                                    not sets:is_element(NC, ECSet)],
    ExistingConstraints ++ FilteredNewConstraints.


%% @doc constraints is an associated list keeping track of all the constraints
%% that have been placed on a package
-spec new_constraints() -> constraints().
new_constraints() ->
    [].

%% @doc Given a dep graph and a set of goals this either solves the problem or
%% fails. This is basically the root solver of the system, the main difference
%% from the exported solve/2 function is the fact that this does not do the
%% culprit search.
-spec primitive_solve(dep_graph(),[constraint()]) ->
                             [pkg()] | 'fail'.
primitive_solve(State, PackageList)
  when erlang:length(PackageList) > 0 ->
    Constraints = lists:foldl(fun(Info, Acc) ->
                                      add_constraint(Acc, Info)
                              end, new_constraints(), PackageList),

    [Pkg | OtherPkgs] = lists:map(fun dep_pkg/1, PackageList),
    pkgs(State, [], dep_pkg(Pkg), Constraints, OtherPkgs).

%% @doc given a package spec return the 'name' portion of said spec
-spec dep_pkg(constraint()) -> pkg_name().
dep_pkg({Pkg, _Vsn}) ->
    Pkg;
dep_pkg({Pkg, _Vsn, _}) ->
    Pkg;
dep_pkg({Pkg, _Vsn1, _Vsn2, _}) ->
    Pkg;
dep_pkg(Pkg) when is_atom(Pkg) orelse is_list(Pkg) ->
    Pkg.

-spec is_valid_constraint(constraint()) -> boolean().
is_valid_constraint(Pkg) when is_atom(Pkg) orelse is_list(Pkg) ->
    true;
is_valid_constraint({_Pkg, Vsn}) when is_list(Vsn) ->
    true;
is_valid_constraint({_Pkg, _LVsn, gte}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '>='}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, lte}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '<='}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, gt}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '>'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, lt}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '<'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn1, _LVsn2, between}) ->
    true;
is_valid_constraint(_InvalidConstraint) ->
    false.

-spec add_constraint([constraint()],constraint()) -> [constraint()].
add_constraint(PkgsConstraints, PkgConstraint) ->
    case is_valid_constraint(PkgConstraint) of
        true -> ok;
        false -> erlang:throw({invalid_constraint, PkgConstraint})
    end,
    PkgName = dep_pkg(PkgConstraint),
    Constraints1 =
        case lists:keysearch(PkgName, 1, PkgsConstraints) of
            false ->
                [];
            {value, {PkgName, Constraints0}} ->
                Constraints0
        end,
    [{PkgName, [PkgConstraint | Constraints1]} |
     lists:keydelete(PkgName, 1, PkgsConstraints)].

%% @doc
%% Extend the currently active constraints correctly for the given constraints.
-spec extend_constraints(constraints(),constraints()) -> constraints().
extend_constraints(ExistingConstraints0, NewConstraints) ->
    lists:foldl(fun (Constraint, ExistingConstraints1) ->
                        add_constraint(ExistingConstraints1, Constraint)
                end,
                ExistingConstraints0, NewConstraints).

-spec is_version_within_constraint(vsn(),constraint()) -> boolean().
is_version_within_constraint(_Vsn, Pkg) when is_atom(Pkg) orelse is_list(Pkg) ->
    true;
is_version_within_constraint(Vsn, {_Pkg, Vsn}) when is_list(Vsn) ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, gte}) when Vsn >= LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>='}) when Vsn >= LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, lte}) when Vsn =< LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<='}) when Vsn =< LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, gt}) when Vsn > LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>'}) when Vsn > LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, lt}) when Vsn < LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<'}) when Vsn < LVsn ->
    true;
is_version_within_constraint(Vsn, {_Pkg, LVsn1, LVsn2, between})
  when Vsn >= LVsn1 andalso Vsn =< LVsn2 ->
    true;
is_version_within_constraint(_Vsn, _Pkg) ->
    false.

%% @doc
%% Get the currently active constraints that relate to the specified package
-spec get_constraints(constraints(),pkg_name()) -> constraints().
get_constraints(PkgsConstraints, PkgName) ->
    case lists:keysearch(PkgName, 1, PkgsConstraints) of
        false ->
            [];
        {value, {PkgName, Constraints}} ->
            Constraints
    end.

%% @doc
%% Given a package name get the list of all versions available for that package.
-spec get_versions(dep_graph(),pkg_name()) -> [vsn()].
get_versions(DepGraph, PkgName) ->
    case gb_trees:lookup(PkgName, DepGraph) of
        none ->
            [];
        {value, AllVsns} ->
            [Vsn || {Vsn, _} <- AllVsns]
    end.

%% @doc
%% make sure a given name/vsn meets all current constraints
-spec valid_version(pkg_name(),vsn(),constraints()) -> boolean().
valid_version(PkgName, Vsn, PkgConstraints) ->
    lists:all(fun (L) ->
                      is_version_within_constraint(Vsn, L)
              end,
              get_constraints(PkgConstraints, PkgName)).

%% @doc
%% Given a Package Name and a set of constraints get a list of package
%% versions that meet all constraints.
-spec constrained_package_versions(dep_graph(),pkg_name(),constraints()) ->
                                          [vsn()].
constrained_package_versions(State, PkgName, PkgConstraints) ->
    Versions = get_versions(State, PkgName),
    [Vsn || Vsn <- Versions, valid_version(PkgName, Vsn, PkgConstraints)].

%% Given a list of constraints filter said list such that only false (for things
%% that do not match a package and pkg are returned. Since at the end only pkg()
%% we should have a pure list of packages.
-spec filter_package_constraints([constraint()]) -> [false | pkg()].
filter_package_constraints(PkgConstraints) ->
    lists_some(fun (Pkg)
                     when is_atom(Pkg) ->
                       false;
                   ({_Pkg1, _Vsn} = PV) ->
                       PV;
                   ({_Pkg2, _Vsn, _R}) ->
                       false;
                   ({_Pkg2, _Vsn1, _Vsn2, _R}) ->
                       false
               end, PkgConstraints).

%% @doc all_pkgs is one of the set of mutually recursive functions (all_pkgs and
%% pkgs) that serve to walk the solution space of dependency.
-spec all_pkgs(dep_graph(),[pkg_name()],[pkg_name()],[constraint()]) ->
                      fail | [pkg()].
all_pkgs(_State, _, [], Constraints) ->
    PkgVsns =
        [filter_package_constraints(PkgConstraints) || {_Pkg, PkgConstraints} <- Constraints],

    %% PkgVsns should be a list of pkg() where all the constraints are correctly
    %% met. If not we fail the solution. If so we return those pkg()s
    case lists:all(fun({Pkg, Vsn}) ->
                           lists:all(fun(Constraint) ->
                                             is_version_within_constraint(Vsn, Constraint)
                                     end,  get_constraints(Constraints, Pkg))
                   end, PkgVsns) of
        true ->
            PkgVsns;
        false ->
            fail
    end;
all_pkgs(State, Visited, [Pkg | Pkgs], Constraints) ->
    case lists:member(Pkg, Visited) of
        true ->
            all_pkgs(State, Visited, Pkgs, Constraints);
        false ->
            pkgs(State, Visited, Pkg, Constraints, Pkgs)
    end.

%% @doc this is the key graph walker. Set of constraints it walks forward into
%% the solution space searching for a path that solves all dependencies.
-spec pkgs(dep_graph(),[pkg_name()], pkg_name(), [constraint()],
           [constraint()]) -> fail | [pkg()].
pkgs(DepGraph, Visited, Pkg, Constraints, OtherPkgs) ->
    F = fun (Vsn) ->
                Deps = get_dep_constraints(DepGraph, Pkg, Vsn),
                UConstraints = extend_constraints(Constraints, [{Pkg, Vsn}
                                                                | Deps]),
                DepPkgs =[dep_pkg(Dep) || Dep <- Deps],
                NewVisited = [Pkg | Visited],
                all_pkgs(DepGraph, NewVisited, DepPkgs ++ OtherPkgs, UConstraints)
        end,
    lists_some(F, constrained_package_versions(DepGraph, Pkg, Constraints), fail).


%% @doc This gathers the dependency constraints for a given package vsn from the
%% dependency graph.
-spec get_dep_constraints(dep_graph(), pkg_name(), vsn()) -> [constraint()].
get_dep_constraints(DepGraph, PkgName, Vsn) ->
    {Vsn, Constraints} = lists:keyfind(Vsn, 1,
                                       gb_trees:get(PkgName, DepGraph)),
    Constraints.

-type evaluator(A, Return, FailIndicator) ::
        fun((A) -> Return | FailIndicator).
-spec lists_some(evaluator(A, Return, false), [A]) -> Return | false.
%% @doc lists_some is the root of the system the actual backtracing search that
%% makes the dep solver posible. It a takes a function that checks whether the
%% 'problem' has been solved and an fail indicator. As long as the evaluator
%% returns the fail indicator processing continues. If the evaluator returns
%% anything but the fail indicator that indicates success.
lists_some(F, Args) ->
    lists_some(F, Args, false).

-spec lists_some(evaluator(A, Return, FailIndicator), [A], FailIndicator) ->
                       Return | FailIndicator.
lists_some(_, [], False) ->
    False;
lists_some(F, [H | T], False) ->
    case F(H) of
        False ->
            lists_some(F, T, False);
        Res ->
            Res
    end.

%% @doc given a graph and a set of top level goals return a graph that contains
%% only those top level packages and those packages that might be required by
%% those packages.
-spec trim_unreachable_packages(dep_graph(), [constraint()]) ->
                                       dep_graph() | {error, term()}.
trim_unreachable_packages(State, Goals) ->
    {_, NewState0} = new_graph(),
    lists:foldl(fun(_Pkg, Error={error, _}) ->
                        Error;
                   (Pkg, NewState1) ->
                        PkgName = dep_pkg(Pkg),
                        find_reachable_packages(State, NewState1, PkgName)
                end, NewState0, Goals).

%% @doc given a list of versions and the constraints for that version rewrite
%% the new graph to reflect the requirements of those versions.
-spec rewrite_vsns(dep_graph(), dep_graph(), [{vsn(), [constraint()]}]) ->
                          dep_graph() | {error, term()}.
rewrite_vsns(ExistingGraph, NewGraph0, Info) ->
    lists:foldl(fun(_, Error={error, _}) ->
                        Error;
                   ({_Vsn, Constraints}, NewGraph1) ->
                        lists:foldl(fun(_DepPkg, Error={error, _}) ->
                                            Error;
                                       (DepPkg, NewGraph2) ->
                                            DepPkgName = dep_pkg(DepPkg),
                                            find_reachable_packages(ExistingGraph,
                                                                    NewGraph2,
                                                                    DepPkgName)
                                    end, NewGraph1, Constraints)
                end, NewGraph0, Info).

%% @doc Rewrite the existing dep graph removing anything that is not reachable
%% required by the goals or any of its potential dependencies.
-spec find_reachable_packages(dep_graph(), dep_graph(), pkg_name()) ->
                                     dep_graph() | {error, term()}.
find_reachable_packages(_ExistingGraph, Error={error, _}, _PkgName) ->
    Error;
find_reachable_packages(ExistingGraph, NewGraph0, PkgName) ->
    case contains_package_version(NewGraph0, PkgName) of
        true ->
            NewGraph0;
        false ->
            case gb_trees:lookup(PkgName, ExistingGraph) of
                {value, Info} ->
                    NewGraph1 = gb_trees:insert(PkgName, Info, NewGraph0),
                    rewrite_vsns(ExistingGraph, NewGraph1, Info);
                none ->
                    {error, {unreachable_package, PkgName}}
            end
    end.

%% @doc
%%  Checks to see if a package name has been defined in the dependency graph
-spec contains_package_version(dep_graph(), pkg_name()) -> boolean().
contains_package_version(Dom0, PkgName) ->
    gb_trees:is_defined(PkgName, Dom0).

%%============================================================================
%% Tests
%%============================================================================
-ifndef(NO_TESTS).
-include_lib("eunit/include/eunit.hrl").

first_test() ->
    Dom0 = add_packages(new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                {app3, "0.2", '>='}]},
                                       {"0.2", []},
                                       {"0.3", []}]},
                               {app2, [{"0.1", []},
                                       {"0.2",[{app3, "0.3"}]},
                                       {"0.3", []}]},
                               {app3, [{"0.1", []},
                                       {"0.2", []},
                                       {"0.3", []}]}]),


    X = solve(Dom0, [{app1, "0.1"}]),
    ?assertMatch({ok, [{app3,"0.3"},
                       {app2,"0.2"},
                       {app1,"0.1"}]},
                  X).

second_test() ->

    Dom0 = add_packages(new_graph(), [{app1, [{"0.1", [{app2, "0.1", '>='},
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

    X = solve(Dom0, [{app1, "0.1"},
                     {app2, "0.3"}]),
    ?assertMatch({ok, [{app3,"0.3"},
                       {app2,"0.3"},
                       {app4,"0.2"},
                       {app1,"0.1"}]},
                 X).

third_test() ->

    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = add_packages(new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                       {"0.2", Pkg1Deps},
                                       {"3.0", Pkg1Deps}]},

                               {app2, [{"0.0.0.1", Pkg2Deps},
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
    ?assertMatch({ok, [{app5,"6.0.0"},
                       {app3,"0.1.3"},
                       {app4,"6.0.0"},
                       {app2,"3.0"},
                       {app1,"3.0"}]},
                 solve(Dom0, [{app1, "3.0"}])),


    ?assertMatch({ok, [{app5,"6.0.0"},
                       {app3,"0.1.3"},
                       {app4,"6.0.0"},
                       {app2,"3.0"},
                       {app1,"3.0"}]},
                 solve(Dom0, [app1, app2, app5])).

fail_test() ->
    Dom0 = add_packages(new_graph(), [{app1, [{"0.1", [{app2, "0.2"},
                                                {app3, "0.2", gte}]},
                                       {"0.2", []},
                                       {"0.3", []}]},
                               {app2, [{"0.1", []},
                                       {"0.2",[{app3, "0.1"}]},
                                       {"0.3", []}]},
                               {app3, [{"0.1", []},
                                       {"0.2", []},
                                       {"0.3", []}]}]),


    ?assertMatch({error, {unable_to_solve, {app1,"0.1"},
                          {[], [], [{app1, "0.1"}]}}},
                 solve(Dom0, [{app1, "0.1"}])).

conflicting_passing_test() ->
    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "3.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],

    Dom0 = add_packages(new_graph(), [{app1, [{"0.1.0", Pkg1Deps},
                                       {"0.1.0", Pkg1Deps},
                                       {"0.2", Pkg1Deps},
                                       {"3.0", Pkg1Deps}]},
                               {app2, [{"0.0.0.1", Pkg2Deps},
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

    ?assertMatch({ok, [{app5,"2.0.0"},
                       {app3,"0.1.3"},
                       {app4,"5.0.0"},
                       {app2,"3.0"},
                       {app1,"3.0"}]},
                 solve(Dom0, [{app1, "3.0"}])),

    ?assertMatch({ok, [{app5,"2.0.0"},
                       {app3,"0.1.3"},
                       {app4,"5.0.0"},
                       {app2,"3.0"},
                       {app1,"3.0"}]},
                 solve(Dom0, [app1, app2, app5])).



circular_dependencies_test() ->
    Dom0 = add_packages(new_graph(), [{app1, [{"0.1.0", [app2]}]},
                               {app2, [{"0.0.1", [app1]}]}]),

    ?assertMatch({ok, [{app1,"0.1.0"},{app2,"0.0.1"}]},
                 solve(Dom0, [{app1, "0.1.0"}])).

conflicting_failing_test() ->
    Pkg1Deps = [app2,
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "6.0.0"}],

    Dom0 = add_packages(new_graph(), [{app1, [{"3.0", Pkg1Deps}]},
                               {app2, [{"0.0.1", Pkg2Deps}]},
                               {app3, [{"0.1.0", Pkg3Deps}]},
                               {app4, [{"5.0.0", [{app5, "2.0.0"}]}]},
                               {app5, [{"2.0.0", []},
                                       {"6.0.0", []}]}]),

    ?assertMatch({error, {unable_to_solve,app3,{[],[],[app1,app3]}}},
                 solve(Dom0, [app1, app3])).

-endif.
