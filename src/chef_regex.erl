%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @author Marc Paradise <marc@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2012 Opscode, Inc.
%%
%% Regex used in erchef

-module(chef_regex).

-export([
         regex_for/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("chef_regex.hrl").

%% Run List Regular Expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Currently Chef does not support Semantic Versioning
%% (http://www.semver.org) of cookbooks, in that versions can only
%% take the form MAJOR.MINOR or MAJOR.MINOR.PATCH.
%%
%% See http://wiki.opscode.com/display/chef/Version+Constraints
%%
%% A version is always optional, though.
-define(RECIPE_VERSION_REGEX, "(?:@[[:digit:]]+(\\.[[:digit:]]+){1,2})?").

%% Cookbooks, Recipes, and Roles have common naming conventions.
%% There is a concrete reference for role names at
%% http://wiki.opscode.com/display/chef/Roles#Roles-name.  Judging
%% from the cookbook names and recipes in the opscode/cookbooks
%% repository, this regular expression applies to them as well.
-define(NAME_REGEX, "[.[:alnum:]_-]+").

%% Recipes can be cookbook-qualified; if not, the name is taken to be
%% the cookbook, and the recipe is implicitly assumed to be "default".
-define(COOKBOOK_PREFIX_REGEX, "(?:" ++ ?NAME_REGEX ++ "::)?").
-define(VERSIONED_RECIPE_REGEX, ?COOKBOOK_PREFIX_REGEX ++ ?NAME_REGEX ++ ?RECIPE_VERSION_REGEX).


-spec regex_for(regex_name()) -> {re_regex(),  re_msg()}.
%% @doc provide compiled regex for use externally
regex_for(recipe_name) ->
    Pattern = "^[.[:alnum:]_-]+(::[.[:alnum:]_-]+)?$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Invalid recipe name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(cookbook_version) ->
    Pattern = "^[[:digit:]]+(\\.[[:digit:]]+){1,2}$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Invalid cookbook version">>};
regex_for(cookbook_name) ->
    Pattern = "^[.[:alnum:]_-]+$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(environment_name) ->
    Pattern = "^[.[:alnum:]_-]+$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed environment name. Must only contain A-Z, a-z, 0-9, _ or -">>};
regex_for(client_name) ->
    % This might be the same as nodename -- nodename seems to allow ':' as well
    Pat = <<"^[[:alnum:]_.-]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>};

%% used in environments
regex_for(cookbook_version_constraint) ->
    Pattern = "^(~>|=|>=?|<=?) [[:digit:]]+(\\.[[:digit:]]+){1,2}$",
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
    Pattern = "^" ++ ?VERSIONED_RECIPE_REGEX ++ "$",
    {ok, Regex} = re:compile(Pattern),
    {Regex, <<"Malformed recipe">>}.
