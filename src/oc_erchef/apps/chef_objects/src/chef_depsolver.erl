%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
%%
%% Regex from chef_json_validator.erl
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2012-2013-2018 Chef Software, Inc. All Rights Reserved.
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
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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

-export_type([constraint/0,
              raw_constraint/0,
              dependency_set/0,
              pkg/0]).

%%============================================================================
%% Types
%%============================================================================
-type pkg_name() :: binary() | atom().
-type pkg() :: {pkg_name(), vsn()}.
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
    chef_object_base:set_default_values(Bin, [{<<"run_list">>, []}]).

-spec validate_body(ej:json_object()) -> {ok, ej:json_object()}.
validate_body(Body) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, Body) of
        ok -> {ok, Body};
        Bad -> throw(Bad)
    end.

-spec solve_dependencies(AllVersions :: [dependency_set()],
                         EnvConstraints :: [constraint()],
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
    chef_depsolver_worker:solve_dependencies(AllVersions,
                                             EnvConstraints,
                                             Cookbooks,
                                             depsolver_timeout()).

depsolver_timeout() ->
    envy:get(chef_objects, depsolver_timeout, ?DEFAULT_DEPSOLVER_TIMEOUT, non_neg_integer).

