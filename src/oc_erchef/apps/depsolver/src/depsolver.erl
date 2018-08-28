%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%% ex: ts=4 sw=4 et
%%
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
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%
%%%-------------------------------------------------------------------
%%% @doc
%%% This is a dependency constraint solver. You add your 'world' to the
%%% solver. That is the packages that exist, their versions and their
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
%%% we can add this world to the system all at once as follows
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
-export([parse_version/1,
         filter_packages/2,
         filter_package/2
     ]).

-export_type([t/0,
              pkg/0,
              constraint_op/0,
              pkg_name/0,
              vsn/0,
              constraint/0,
              dependency_set/0]).

-export_type([dep_graph/0, constraints/0,
              ordered_constraints/0, fail_info/0,
              fail_detail/0]).
%%============================================================================
%% type
%%============================================================================
-type dep_graph() :: gb_trees:tree().
-opaque t() :: {?MODULE, dep_graph()}.
-type pkg() :: {pkg_name(), vsn()}.
-type pkg_name() :: binary() | atom().
-type raw_vsn() :: ec_semver:any_version().

-type vsn() :: 'NO_VSN'
             | ec_semver:semver().

-type constraint_op() ::
        '=' | gte | '>=' | lte | '<='
      | gt | '>' | lt | '<' | pes | '~>' | between.

-type raw_constraint() :: pkg_name()
                        | {pkg_name(), raw_vsn()}
                        | {pkg_name(), raw_vsn(), constraint_op()}
                        | {pkg_name(), raw_vsn(), vsn(), between}.

-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), constraint_op()}
                    | {pkg_name(), vsn(), vsn(), between}.


-type vsn_constraint() :: {raw_vsn(), [raw_constraint()]}.
-type dependency_set() :: {pkg_name(), [vsn_constraint()]}.

%% Internal Types
-type constraints() :: [constraint()].
-type ordered_constraints() :: [{pkg_name(), constraints()}].
-type fail_info() :: {[pkg()], ordered_constraints()}.
-type fail_detail() :: {fail, [fail_info()]} | {missing, pkg_name()}.

%%============================================================================
%% Macros
%%============================================================================
-define(DEFAULT_TIMEOUT, 2000).

%% Parse a string version into a tuple based version
-spec parse_version(raw_vsn() | vsn()) -> vsn().
parse_version(RawVsn)
  when erlang:is_list(RawVsn);
       erlang:is_binary(RawVsn) ->
    ec_semver:parse(RawVsn);
parse_version(Vsn)
  when erlang:is_tuple(Vsn) ->
    Vsn.

%% @doc given a list of package name version pairs, and a list of constraints
%% return every member of that list that matches all constraints.
-spec filter_packages([{pkg_name(), raw_vsn()}], [raw_constraint()]) ->
                             {ok, [{pkg_name(), raw_vsn()}]}
                                 | {error, Reason::term()}.
filter_packages(PVPairs, RawConstraints) ->
    Constraints = [fix_con(Constraint) || Constraint <- RawConstraints],
    case check_constraints(Constraints) of
        ok ->
            {ok, [PVPair || PVPair <- PVPairs,
                            filter_pvpair_by_constraint(fix_con(PVPair), Constraints)]};
        Error ->
            Error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================
-spec check_constraints(constraints()) ->
                               ok | {error, {invalid_constraints, [term()]}}.
check_constraints(Constraints) ->
    PossibleInvalids =
        lists:foldl(fun(Constraint, InvalidConstraints) ->
                            case is_valid_constraint(Constraint) of
                                true ->
                                    InvalidConstraints;
                                false ->
                                [Constraint | InvalidConstraints]
                            end
                    end, [], Constraints),
    case PossibleInvalids of
        [] ->
            ok;
        _ ->
        {error, {invalid_constraints, PossibleInvalids}}
    end.


-spec filter_pvpair_by_constraint({pkg_name(), vsn()}, [constraint()]) ->
                                         boolean().
filter_pvpair_by_constraint(PVPair, Constraints) ->
    lists:all(fun(Constraint) ->
                      filter_package(PVPair, Constraint)
              end, Constraints).

-spec filter_package({pkg_name(), vsn()}, constraint()) ->
                            boolean().
filter_package({PkgName, Vsn}, PkgName) ->
    is_version_within_constraint(Vsn, PkgName);
filter_package({PkgName, Vsn}, C = {PkgName, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package({PkgName, Vsn}, C = {PkgName, _, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package({PkgName, Vsn}, C = {PkgName, _, _, _}) ->
    is_version_within_constraint(Vsn, C);
filter_package(_, _) ->
    %% If its not explicitly excluded its included
    true.

%% @doc
%% fix the package name. If its a list turn it into a binary otherwise leave it as an atom
fix_pkg(Pkg) when is_list(Pkg) ->
    erlang:list_to_binary(Pkg);
fix_pkg(Pkg) when is_binary(Pkg); is_atom(Pkg) ->
    Pkg.

%% @doc
%% fix package. Take a package with a possible invalid version and fix it.
-spec fix_con(raw_constraint()) -> constraint().
fix_con({Pkg, Vsn}) ->
    {fix_pkg(Pkg), parse_version(Vsn)};
fix_con({Pkg, Vsn, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn), CI};
fix_con({Pkg, Vsn1, Vsn2, CI}) ->
    {fix_pkg(Pkg), parse_version(Vsn1),
     parse_version(Vsn2), CI};
fix_con(Pkg) ->
    fix_pkg(Pkg).

-spec is_valid_constraint(constraint()) -> boolean().
is_valid_constraint(Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    true;
is_valid_constraint({_Pkg, Vsn}) when is_tuple(Vsn) ->
    true;
is_valid_constraint({_Pkg, Vsn, '='}) when is_tuple(Vsn) ->
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
is_valid_constraint({_Pkg, _LVsn, pes}) ->
    true;
is_valid_constraint({_Pkg, _LVsn, '~>'}) ->
    true;
is_valid_constraint({_Pkg, _LVsn1, _LVsn2, between}) ->
    true;
is_valid_constraint(_InvalidConstraint) ->
    false.

-spec is_version_within_constraint(vsn(),constraint()) -> boolean().
is_version_within_constraint({missing}, _Pkg)->
    false;
is_version_within_constraint(_Vsn, Pkg) when is_atom(Pkg) orelse is_binary(Pkg) ->
    true;
is_version_within_constraint(Vsn, {_Pkg, NVsn}) ->
    ec_semver:eql(Vsn, NVsn);
is_version_within_constraint(Vsn, {_Pkg, NVsn, '='}) ->
    ec_semver:eql(Vsn, NVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, gte})  ->
    ec_semver:gte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>='}) ->
    ec_semver:gte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, lte}) ->
    ec_semver:lte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<='}) ->
    ec_semver:lte(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, gt}) ->
    ec_semver:gt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '>'}) ->
    ec_semver:gt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, lt}) ->
    ec_semver:lt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '<'}) ->
    ec_semver:lt(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, 'pes'}) ->
    ec_semver:pes(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn, '~>'}) ->
    ec_semver:pes(Vsn, LVsn);
is_version_within_constraint(Vsn, {_Pkg, LVsn1, LVsn2, between}) ->
    ec_semver:between(LVsn1, LVsn2, Vsn);
is_version_within_constraint(_Vsn, _Pkg) ->
    false.
