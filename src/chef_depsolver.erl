%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%%
%% Regex from chef_json_validator.erl
%% @author Marc Paradise <marc@opscode.com>
%% Copyright 2012-2013 Opscode, Inc. All Rights Reserved.
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

-module(chef_depsolver).

-include_lib("eunit/include/eunit.hrl").

-export([
         parse_binary_json/1,
         solve_dependencies/3
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").
-include("chef_regex.hrl").

 %% @doc the input into the depsolver is a single run_list
-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"run_list">>, chef_json_validator:run_list_spec()}
         ]}).

-define(DEFAULT_DEPSOLVER_TIMEOUT, 2000).

%% @doc Convert a binary JSON string representing a Chef runlist into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json( binary() ) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin) ->
    Bin0 = chef_json:decode(Bin),
    Runlist = set_default_values(Bin0),
    validate_body(Runlist).


%% @doc If the run_list is missing, fill it in with an empty
%% run_list
-spec set_default_values( ejson_term() ) -> ejson_term().
set_default_values(Bin) ->
    case ej:get({<<"run_list">>}, Bin) of
        undefined ->
            ej:set({<<"run_list">>}, Bin, []);
        _ -> Bin
    end.

-spec validate_body(ej:json_object()) -> {ok, ej:json_object()}.
validate_body(Body) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, Body) of
        ok -> {ok, Body};
        Bad -> throw(Bad)
    end.

-spec solve_dependencies(AllVersions :: [depsolver:dependency_set()],
                         EnvConstraints :: [depsolver:constraint()],
                         Cookbooks :: [Name::binary() |
                                             {Name::binary(), Version::binary()}]) ->
    {ok, [ versioned_cookbook()]} | {error, term()}.


%% @doc Main entry point into the depsolver.  It is supplied with a dependency_set()
%% containing all the cookbook versions and their dependencies that are in the database
%% along with the constraints from the environment
%%
%% It returns the solution as a list of versioned cookbooks that are available in the
%% environment
solve_dependencies(_AllVersions, _EnvConstraints, []) ->
    {ok, []};
solve_dependencies(AllVersions, EnvConstraints, Cookbooks) ->
    %% We apply the environment cookbook version constraints as a pre-filter, removing
    %% cookbook versions that don't satisfy early. This makes for a smaller graph and an
    %% easier problem to solve. However, when cookbooks are filtered out due to the
    %% environment, the solver is unable to backtrack and provide extra error detail. With
    %% this approach, the "world" of cookbooks conforms to what the user will see from
    %% listing cookbooks within an environment.
    {ok, FilteredVersions} =
        folsom_time(depsolver, filter_packages_with_deps,
                    fun() ->
                            depsolver:filter_packages_with_deps(AllVersions,
                                                                EnvConstraints)
                    end),
    Graph = folsom_time(depsolver, add_packages,
                        fun() ->
                                depsolver:add_packages(depsolver:new_graph(),
                                                       FilteredVersions)
                        end),
    Result = folsom_time(depsolver, solve,
                         fun() ->
                                 depsolver:solve(Graph, Cookbooks, depsolver_timeout())
                         end),
    sanitize_semver(Result).

%% @doc The depsolver module (as of version 0.1.0) supports semver and returns a version
%% structure as `{Name, {{1, 2, 3}, {Alpha, Build}}}'. Chef does not currently support
%% semver style versions for cookbooks. For successful solve results, we simplify the
%% return. Error returns will contain version data (with semver details). These are left in
%% place for two reasons: 1) the error structures are not as simple to sanitize; 2) the
%% depsolver_culprits module is used to format the error returns and it is expecting data in
%% this format.
sanitize_semver({ok, WithSemver}) ->
    XYZOnly = [ {Name, XYZVersion} || {Name, {XYZVersion, _SemVer}} <- WithSemver ],
    {ok, XYZOnly};
sanitize_semver(Error) ->
    Error.

depsolver_timeout() ->
    case application:get_env(chef_objects, depsolver_timeout) of
        undefined ->
            ?DEFAULT_DEPSOLVER_TIMEOUT;
        {ok, Value} when is_integer(Value) ->
            Value;
        Bad ->
            error({invalid_config, {chef_objects, depsolver_timeout, Bad}})
    end.

folsom_time(M, F, Fun) ->
    Label = oc_folsom:mf_label(M, F),
    oc_folsom:time(Label, Fun).
