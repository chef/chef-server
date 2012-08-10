%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%%
%% Regex from chef_json_validator.erl
%% @author Marc Paradise <marc@opscode.com>
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

-define(DEFAULT_FIELD_VALUES,
        [
         {<<"run_list">>, []}
        ]).

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"run_list">>, is_run_list}
        ]).


%% @doc Convert a binary JSON string representing a Chef runlist into an
%% EJson-encoded Erlang data structure.
%% @end
-spec parse_binary_json( binary() ) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin) ->
    Bin0 = ejson:decode(Bin),
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
    case ej:valid(depsolver_spec(), Body) of
        ok -> {ok, Body};
        Bad -> throw(Bad)
    end.

%% @doc the input into the depsolver is a single run_list
%%
depsolver_spec() ->
  {[
    {<<"run_list">>, {array_map, {fun_match, {fun valid_run_list_item/1, string,
                                              <<"Invalid Run list item">>}}}}
   ]}.

valid_run_list_item(Str) ->
    {RegEx, _Msg} = regex_for_run_list_item(Str),
    case re:run(Str, RegEx) of
        nomatch -> error;
        _ -> ok
    end.

%% @doc Given an item from a Chef run list, determines if it is
%% syntactically correct, taking into account the various types of
%% items that are allowed in a run list.
-spec regex_for_run_list_item(binary()) -> {re_regex(), re_msg()}.
regex_for_run_list_item(<<"recipe[", _Rest/binary>>) ->
    chef_regex:regex_for(qualified_recipe);
regex_for_run_list_item(<<"role[", _Rest/binary>>) ->
    chef_regex:regex_for(qualified_role);
regex_for_run_list_item(UnqualifiedRecipe) when is_binary(UnqualifiedRecipe) ->
    chef_regex:regex_for(unqualified_recipe).

-spec solve_dependencies(AllVersions :: [depsolver:dependency_set()],
                         EnvConstraints :: [depsolver:dependency_set()],
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
    Graph0 = depsolver:new_graph(),
    Graph1 = depsolver:add_packages(Graph0, AllVersions),
    Graph2 = depsolver:add_packages(Graph1, EnvConstraints),

    depsolver:solve(Graph2, Cookbooks).
