%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @author Marc Paradise <marc@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%%
%% Regex used in erchef
%%
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


-module(chef_regex).

-export([
         regex_for/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("chef_regex.hrl").

%% Regular Expression Macros
%% (Just to help DRY things up and make the usage patterns a little more clear)

%% We commonly need to fully anchor a regex
-define(ANCHOR_REGEX(Regex), "^" ++ Regex ++ "$").

%% Versions can only take the form MAJOR.MINOR or MAJOR.MINOR.PATCH.
%%
%% See http://wiki.opscode.com/display/chef/Version+Constraints
-define(VERSION_REGEX, "[[:digit:]]+(\\.[[:digit:]]+){1,2}").

%% A version is always optional for recipes
-define(RECIPE_VERSION_REGEX, "(?:@" ++ ?VERSION_REGEX ++ ")?").

%% Cookbooks, Recipes, and Roles have common naming conventions.
%% There is a concrete reference for role names at
%% http://wiki.opscode.com/display/chef/Roles#Roles-name.  Judging
%% from the cookbook names and recipes in the opscode/cookbooks
%% repository, this regular expression applies to them as well.
-define(NAME_REGEX, "[.[:alnum:]_-]+").

%% Recipes can be cookbook-qualified; if not, the name is taken to be
%% the cookbook, and the recipe is implicitly assumed to be "default".
-define(COOKBOOK_PREFIX_REGEX, "(?:" ++ ?NAME_REGEX ++ "::)?").
-define(COOKBOOK_QUALIFIED_RECIPE_REGEX, ?COOKBOOK_PREFIX_REGEX ++ ?NAME_REGEX).

%% Sometimes, recipe names can have version qualifiers as well.
-define(VERSIONED_RECIPE_REGEX, ?COOKBOOK_QUALIFIED_RECIPE_REGEX ++ ?RECIPE_VERSION_REGEX).

-spec regex_for(regex_name()) -> {re_regex(),  re_msg()}.
%% @doc provide compiled regex for use externally
regex_for(recipe_name) ->
    %% Note that this does NOT include a version suffix!
    Pattern = ?ANCHOR_REGEX(?COOKBOOK_QUALIFIED_RECIPE_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Invalid recipe name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(cookbook_version) ->
    Pattern = ?ANCHOR_REGEX(?VERSION_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Invalid cookbook version">>};
regex_for(cookbook_name) ->
    Pattern = ?ANCHOR_REGEX(?NAME_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(environment_name) ->
    Pattern = ?ANCHOR_REGEX(?NAME_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed environment name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(client_name) ->
    % This might be the same as nodename -- nodename seems to allow ':' as well
    Pattern = ?ANCHOR_REGEX(?NAME_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>};

%% used in environments
regex_for(cookbook_version_constraint) ->
    Pattern = "^(~>|=|>=?|<=?) " ++ ?VERSION_REGEX ++ "$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Invalid cookbook version constraint">>};

regex_for(qualified_role) ->
    %% Roles MUST be wrapped in "role[...]" to be recognized as such.
    %% Also, they have no cookbook prefix or version suffix.
    Pattern = "^role\\[" ++ ?NAME_REGEX ++ "\\]$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed role">>};
%% Recipes can be wrapped in "recipe[...]", or can be bare
regex_for(qualified_recipe) ->
    Pattern = "^recipe\\[" ++ ?VERSIONED_RECIPE_REGEX ++ "\\]$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed recipe">>};
regex_for(unqualified_recipe) ->
    Pattern = ?ANCHOR_REGEX(?VERSIONED_RECIPE_REGEX),
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed recipe">>}.
