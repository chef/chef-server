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
%%%      Graph0 = doest:new(),
%%%      Graph1 = doest:add_packages(
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
%%% Finally, once we have built up the graph we can ask doest to solve the
%%% dependency constraints. That is to give us a list of valid dependencies by
%%% using the solve function. Lets say we want the app3 version "0.3" and all of
%%% its resolved dependencies. We could call solve as follows.
%%%
%%%    doest:solve(Graph1, [{app3, "0.3"}]).
%%%
%%% That will give us the completely resolved dependencies including app3
%%% itself. Lets be a little more flexible. Lets ask for a graph that is rooted
%%% in anything greater then app3 "0.1". We could do that by
%%%
%%%    doest:solve(Graph1, [{app3, "0.3", '>='}]).
%%%
%%% Of course, you can specify any number of goals at the top level.
%%% @end
%%%-------------------------------------------------------------------
-module(doest).

-export([new/0,
         add_packages/2,
         add_package/3,
         add_package_version/3,
         add_package_version/4,
         solve/2]).

-export_type([t/0,
              pkg/0,
              pkg_name/0,
              vsn/0,
              constraint/0,
              dependency_set/0]).

%%============================================================================
%% type
%%============================================================================
-opaque t() :: {?MODULE, gb_tree()}.
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: string() | atom().
-type vsn() :: string().
-type constraint() :: {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), gte}
                    | {pkg_name(), vsn(),'>='}
                    | {pkg_name(), vsn(), lte}
                    | {pkg_name(), vsn(), '<='}
                    | {pkg_name(), vsn(), gt}
                    | {pkg_name(), vsn(), '>'}
                    | {pkg_name(), vsn(), lt}
                    | {pkg_name(), vsn(), '<'}
                    | {pkg_name(), vsn(), vsn(), between}.
-type dependency_set() :: {pkg(), [{vsn(), [constraint()]}]}.

%% Internal Types
-type constraints() :: [constraint()].

%%============================================================================
%% API
%%============================================================================

-spec new() -> t().
new() ->
    {?MODULE, gb_trees:empty()}.

-spec add_packages(t(), [dependency_set()]) -> t().
add_packages(Dom0, Info)
  when is_list(Info) ->
    lists:foldl(fun({Pkg, VsnInfo}, Dom1) ->
                        add_package(Dom1, Pkg, VsnInfo)
                end, Dom0, Info).

-spec add_package(t(), pkg_name(), [vsn()]) -> t().
add_package(Dom0, Pkg, Versions)
  when is_list(Versions) ->
    lists:foldl(fun({Vsn, Constraints}, Dom1) ->
                        add_package_version(Dom1, Pkg, Vsn, Constraints);
                   (Version, Dom1) ->
                        add_package_version(Dom1, Pkg, Version, [])
                end, Dom0, Versions).

-spec add_package_version(t(), pkg_name(), vsn(), [constraint()]) -> t().
add_package_version({?MODULE, Dom0}, Pkg, Vsn, PkgConstraints) ->
   Info2 =
        case gb_trees:lookup(make_key(Pkg), Dom0) of
            {value, Info0} ->
                case lists:keytake(make_key(Vsn), 1, Info0) of
                    {value, {Vsn, Constraints}, Info1} ->
                        [{Vsn, Constraints ++ PkgConstraints} | Info1];
                    false ->
                        [{Vsn,  PkgConstraints} | Info0]
                end;
            none ->
                [{Vsn, PkgConstraints}]
        end,
    {?MODULE, gb_trees:enter(make_key(Pkg), Info2, Dom0)}.

-spec add_package_version(t(), pkg_name(), vsn()) -> t().
add_package_version(State, Pkg, Vsn) ->
    add_package_version(State, Pkg, Vsn, []).

-spec solve(t(), [constraint()]) -> [pkg()].
solve({?MODULE, State}, PackageList = [Pkg | _]) ->
    Constraints = lists:foldl(fun(Info, Acc) ->
                                      add_constraint(Acc, Info)
                              end, new_constraints(), PackageList),
    [_ | OtherPkgs] = lists:map(fun dep_pkg/1, PackageList),
    case pkgs(State, [], dep_pkg(Pkg), Constraints, OtherPkgs) of
        fail ->
            erlang:throw({error, no_solution_found});
        Solution ->
            Solution
    end.

%%====================================================================
%% Internal Functions
%%====================================================================
make_key(Pkg) ->
    Pkg.

%% @doc
%% given a Pkg | {Pkg, Vsn} | {Pkg, Vsn, Constraint} return Pkg
-spec dep_pkg(constraint()) -> pkg_name().
dep_pkg({Pkg, _Vsn}) ->
    Pkg;
dep_pkg({Pkg, _Vsn, _}) ->
    Pkg;
dep_pkg({Pkg, _Vsn1, _Vsn2, _}) ->
    Pkg;
dep_pkg(Pkg) when is_atom(Pkg) orelse is_list(Pkg) ->
    Pkg.

%% @doc
%% constraints is an associated list keeping track of all the constraints that have
%% been placed on a package
-spec new_constraints() -> constraints().
new_constraints() ->
    [].

-spec add_constraint(constraints(), constraint()) -> constraints().
add_constraint(PkgsConstraints, Pkg) ->
    true = is_valid_constraint(Pkg),
    PkgName = dep_pkg(Pkg),
    Constraints1 =
        case lists:keysearch(make_key(PkgName), 1, PkgsConstraints) of
            false ->
                [];
            {value, {PkgName, Constraints0}} ->
                Constraints0
        end,
    [{PkgName, [Pkg | Constraints1]} |
     lists:keydelete(make_key(PkgName), 1, PkgsConstraints)].

%% @doc
%% Get the currently active constraints that relate to the specified package
-spec get_constraints(constraints(), pkg_name()) -> constraints().
get_constraints(PkgsConstraints, Pkg) ->
    case lists:keysearch(make_key(Pkg), 1, PkgsConstraints) of
        false ->
            [];
        {value, {Pkg, Constraints}} ->
            Constraints
    end.

%% @doc
%% Extend the currently active constraints correctly for the given constraints.
-spec extend_constraints(constraints(), constraints()) -> constraints().
extend_constraints(ExistingConstraints0, NewConstraints) ->
    lists:foldl(fun (Constraint, ExistingConstraints1) ->
                        add_constraint(ExistingConstraints1, Constraint)
                end,
                ExistingConstraints0, NewConstraints).

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

%% @doc
%% make sure a given name/vsn meets all current constraints
-spec valid_version(pkg_name(), vsn(), constraints()) -> boolean().
valid_version(PkgName, Vsn, PkgConstraints) ->
    lists:all(fun (L) ->
                      is_version_within_constraint(Vsn, L)
              end,
              get_constraints(PkgConstraints, PkgName)).
%% @doc
%% Given a Package Name and a set of constraints get a list of package
%% versions that meet all constraints.
-spec constrained_package_versions(t(), pkg_name(), constraints()) -> [vsn()].
constrained_package_versions(State, PkgName, PkgConstraints) ->
    Versions = get_versions(State, PkgName),
    [Vsn || Vsn <- Versions, valid_version(PkgName, Vsn, PkgConstraints)].

%% @doc
%% Given a package name get the list of all versions available for that package.
-spec get_versions(t(), pkg_name()) -> [vsn()].
get_versions(State, PkgName) ->
    AllVsns = gb_trees:get(make_key(PkgName), State),
    [Vsn || {Vsn, _} <- AllVsns].

-spec is_version_within_constraint(vsn(), constraint()) -> boolean().
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

all_pkgs(_State, _, [], Constraints) ->
    PkgsVsns =
        lists:map(fun({_Pkg, Lims}) ->
                          lists_some(fun (Pkg)
                                           when is_atom(Pkg) ->
                                             false;
                                         ({_Pkg1, _Vsn} = PV) ->
                                             PV;
                                         ({_Pkg2, _Vsn, _R}) ->
                                             false;
                                         ({_Pkg2, _Vsn1, _Vsn2, _R}) ->
                                             false
                                     end, Lims)
                  end, Constraints),
    case lists:all(fun({Pkg, Vsn}) ->
                           lists:all(fun(Constraint) ->
                                             is_version_within_constraint(Vsn, Constraint)
                                     end,  get_constraints(Constraints, Pkg))
                   end, PkgsVsns) of
        true ->
            PkgsVsns;
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

pkgs(State, Visited, Pkg, Constraints, OtherPkgs) ->
    F = fun (Vsn) ->
                Deps = get_deps(State, Pkg, Vsn),
                UConstraints = extend_constraints(Constraints, [{Pkg, Vsn} | Deps]),
                DepPkgs = lists:map(fun dep_pkg/1, Deps),
                NewVisited = [Pkg | Visited],
                all_pkgs(State, NewVisited, DepPkgs ++ OtherPkgs, UConstraints)
        end,
    lists_some(F, constrained_package_versions(State, Pkg, Constraints), fail).

-spec get_deps(t(), pkg_name(), vsn()) -> constraints().
get_deps(State, Pkg, Vsn) ->
    {Vsn, Constraints} = lists:keyfind(make_key(Vsn), 1,
                                       gb_trees:get(make_key(Pkg), State)),
    Constraints.

%% @doc
%% return the first Result = F(el) for El in List that is not false
-spec lists_some(fun(), [term()]) -> constraints() | false.
lists_some(F, Args) ->
    lists_some(F, Args, false).

-spec lists_some(fun(), [term()], term()) -> constraints() | false.
lists_some(_, [], False) ->
    False;
lists_some(F, [H | T], False) ->
    case F(H) of
        False ->
            lists_some(F, T, False);
        Res ->
            Res
    end.

%%============================================================================
%% Tests
%%============================================================================
-ifndef(NO_TESTS).
-include_lib("eunit/include/eunit.hrl").

first_test() ->

    Dom0 = add_packages(new(), [{app1, [{"0.1", [{app2, "0.2"},
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
    ?assertMatch([{app3,"0.3"},
                  {app2,"0.2"},
                  {app1,"0.1"}],
                 X).

second_test() ->

    Dom0 = add_packages(new(), [{app1, [{"0.1", [{app2, "0.1", '>='},
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
    ?assertMatch([{app3,"0.3"},
                  {app2,"0.3"},
                  {app4,"0.2"},
                  {app1,"0.1"}],
                 X).

third_test() ->

    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],
    Pkg4Deps = [app5],

    Dom0 = add_packages(new(), [{app1, [{"0.1.0", Pkg1Deps},
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
    ?assertMatch([{app5,"6.0.0"},
                  {app3,"0.1.3"},
                  {app4,"6.0.0"},
                  {app2,"3.0"},
                  {app1,"3.0"}],
                 solve(Dom0, [{app1, "3.0"}])),


    ?assertMatch([{app5,"6.0.0"},
                  {app3,"0.1.3"},
                  {app4,"6.0.0"},
                  {app2,"3.0"},
                  {app1,"3.0"}],
                 solve(Dom0, [app1, app2, app5])).

fail_test() ->

    Dom0 = add_packages(new(), [{app1, [{"0.1", [{app2, "0.2"},
                                                {app3, "0.2", gte}]},
                                       {"0.2", []},
                                       {"0.3", []}]},
                               {app2, [{"0.1", []},
                                       {"0.2",[{app3, "0.1"}]},
                                       {"0.3", []}]},
                               {app3, [{"0.1", []},
                                       {"0.2", []},
                                       {"0.3", []}]}]),


    ?assertThrow({error, no_solution_found},
                 solve(Dom0, [{app1, "0.1"}])).

conflicting_passing_test() ->
    Pkg1Deps = [{app2, "0.1.0", '>='},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    Pkg2Deps = [{app4, "3.0.0", gte}],
    Pkg3Deps = [{app5, "2.0.0", '>='}],

    Dom0 = add_packages(new(), [{app1, [{"0.1.0", Pkg1Deps},
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

    ?assertMatch([{app5,"2.0.0"},
                  {app3,"0.1.3"},
                  {app4,"5.0.0"},
                  {app2,"3.0"},
                  {app1,"3.0"}],
                 solve(Dom0, [{app1, "3.0"}])),

    ?assertMatch([{app5,"2.0.0"},
                  {app3,"0.1.3"},
                  {app4,"5.0.0"},
                  {app2,"3.0"},
                  {app1,"3.0"}],
                 solve(Dom0, [app1, app2, app5])).



circular_dependencies_test() ->
    Dom0 = add_packages(new(), [{app1, [{"0.1.0", [app2]}]},
                               {app2, [{"0.0.1", [app1]}]}]),

    ?assertMatch([{app1,"0.1.0"},{app2,"0.0.1"}],
                 solve(Dom0, [{app1, "0.1.0"}])).

conflicting_failing_test() ->
    Pkg1Deps = [app2,
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between}],

    Pkg2Deps = [{app4, "5.0.0", gte}],
    Pkg3Deps = [{app5, "6.0.0"}],

    Dom0 = add_packages(new(), [{app1, [{"3.0", Pkg1Deps}]},
                               {app2, [{"0.0.1", Pkg2Deps}]},
                               {app3, [{"0.1.0", Pkg3Deps}]},
                               {app4, [{"5.0.0", [{app5, "2.0.0"}]}]},
                               {app5, [{"2.0.0", []},
                                       {"6.0.0", []}]}]),

    ?assertThrow({error, no_solution_found},
                 solve(Dom0, [app1, app3])).

-endif.
