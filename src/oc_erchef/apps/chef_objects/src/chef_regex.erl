%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@chef.io>
%% @author Marc Paradise <marc@chef.io>
%% @author Seth Falcon <seth@chef.io>
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

-include("chef_regex.hrl").

%% Regular Expression Macros
%% (Just to help DRY things up and make the usage patterns a little more clear)

%% We commonly need to fully anchor a regex
-define(ANCHOR_REGEX(Regex), "^" ++ Regex ++ "$").

%% Versions can only take the form MAJOR.MINOR or MAJOR.MINOR.PATCH.
%%
%% See http://wiki.chef.io/display/chef/Version+Constraints
-define(VERSION_REGEX, "[[:digit:]]+(\\.[[:digit:]]+){1,2}").

%% Standard ISO8601 UTC format
-define(DATE_REGEX, "^([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}T[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}Z|infinity)$").

%% A version is always optional for recipes
-define(RECIPE_VERSION_REGEX, "(?:@" ++ ?VERSION_REGEX ++ ")?").

%% Cookbooks, Recipes, and Roles have common naming conventions.
%% There is a concrete reference for role names at
%% http://wiki.chef.io/display/chef/Roles#Roles-name.  Judging
%% from the cookbook names and recipes in the opscode/cookbooks
%% repository, this regular expression applies to them as well.
-define(NAME_CHAR_CLASS, "[.[:alnum:]_-]").
-define(NAME_REGEX, ?NAME_CHAR_CLASS "+").

%% This is very similar to NAME_REGEX (differs only with the addition of ':').  This is used
%% for data bags, data bag items, roles, nodes, and keys.
-define(ALTERNATIVE_NAME_REGEX, "[.[:alnum:]_\:-]+").

%% Policy names are similar to data bags et al., but are limited to 255 Chars.
-define(NAME_REGEX_MAX_255, "[.[:alnum:]_\:-]{1,255}").

%% Username validation regex
-define(USERNAME_REGEX, "[a-z0-9\-_]+").

%% Non-blank string regex
-define(NON_BLANK_REGEX, ".+").

%% Recipes can be cookbook-qualified; if not, the name is taken to be
%% the cookbook, and the recipe is implicitly assumed to be "default".
-define(COOKBOOK_PREFIX_REGEX, "(?:" ++ ?NAME_REGEX ++ "::)?").
-define(COOKBOOK_QUALIFIED_RECIPE_REGEX, ?COOKBOOK_PREFIX_REGEX ++ ?NAME_REGEX).

%% Sometimes, recipe names can have version qualifiers as well.
-define(VERSIONED_RECIPE_REGEX, ?COOKBOOK_QUALIFIED_RECIPE_REGEX ++ ?RECIPE_VERSION_REGEX).

%% Policyfile run list items must be **fully** qualified (both cookbook name
%% and recipe name must be present), and do not allow version qualifiers.
-define(POLICY_FULLY_QUALIFIED_RECIPE_REGEX, "^recipe\\[" ++ ?NAME_REGEX ++ "::" ++ ?NAME_REGEX ++ "\\]$").

%%
%% Scoped names are used when refering to actors in the body of ACL updates, allowing us to
%% distinguish between local groups and global groups. Because this regular expression is
%% run against anything that might be an actor type, we use the more permissive NAME_CHAR_CLASS
%% even though it includes characters disallowed by the USERNAME_REGEX.
%%
-define(SCOPE_SEPARATOR, <<"::">>).
-define(SCOPED_NAME_REGEX, "^(?:(" ?NAME_CHAR_CLASS "+)|(?:(" ?NAME_CHAR_CLASS "*)\\:\\:(" ?NAME_CHAR_CLASS "+)))$").


%% A SHA1 hash
-define(SHA1_HASH_REGEX, "[a-fA-F0-9]{40}").

-spec generate_regex(regex_pattern()) -> re_regex().
generate_regex(Pattern) ->
  {ok, Regex} = re:compile(Pattern),
  Regex.

-spec generate_regex_msg_tuple(regex_pattern(), re_msg()) -> {re_regex(), re_msg()}.
generate_regex_msg_tuple(Pattern, Message) ->
  Regex = generate_regex(Pattern),
  {Regex, Message}.

-spec regex_for(regex_name()) -> {re_regex(),  re_msg()}.
%% @doc provide compiled regex for use externally
regex_for(recipe_name) ->
    %% Note that this does NOT include a version suffix!
    generate_regex_msg_tuple(?ANCHOR_REGEX(?COOKBOOK_QUALIFIED_RECIPE_REGEX),
                             <<"Invalid recipe name. Must only contain A-Z, a-z, 0-9, _, . or -">>);
regex_for(cookbook_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX),
                             <<"Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _, . or -">>);
regex_for(environment_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX),
                             <<"Malformed environment name. Must only contain A-Z, a-z, 0-9, _, . or -">>);
regex_for(key_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ALTERNATIVE_NAME_REGEX),
                             <<"Malformed key name.  A key name can contain only A-Z, a-z, 0-9, _, -, :, or .">>);
regex_for(client_name) ->
    % This might be the same as nodename -- nodename seems to allow ':' as well
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX),
                             <<"Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .">>);
regex_for(date) ->
    generate_regex_msg_tuple(?DATE_REGEX,
                             <<"Malformed date. Must be a valid ISO8601 date atom ending in Z.">>);
regex_for(data_bag_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ALTERNATIVE_NAME_REGEX),
                             <<"Malformed data bag name.  Must only contain A-Z, a-z, 0-9, _, :, ., or -">>);
regex_for(data_bag_item_id) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ALTERNATIVE_NAME_REGEX),
                             <<"Malformed data bag item ID.  Must only contain A-Z, a-z, 0-9, _, :, ., or -">>);
regex_for(role_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ALTERNATIVE_NAME_REGEX),
                             <<"Malformed role name.  Must only contain A-Z, a-z, 0-9, _, :, ., or -">>);
regex_for(node_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ALTERNATIVE_NAME_REGEX),
                             <<"Malformed node name.  Must only contain A-Z, a-z, 0-9, _, :, ., or -">>);

regex_for(qualified_role) ->
   %% Roles MUST be wrapped in "role[...]" to be recognized as such.
   %% Also, they have no cookbook prefix or version suffix.
  generate_regex_msg_tuple("^role\\[" ++ ?NAME_REGEX ++ "\\]$",
                             <<"Malformed role">>);
%% Recipes can be wrapped in "recipe[...]", or can be bare
regex_for(qualified_recipe) ->
  generate_regex_msg_tuple("^recipe\\[" ++ ?VERSIONED_RECIPE_REGEX ++ "\\]$",
                           <<"Malformed recipe">>);
regex_for(unqualified_recipe) ->
  generate_regex_msg_tuple(?ANCHOR_REGEX(?VERSIONED_RECIPE_REGEX),
                           <<"Malformed recipe">>);
regex_for(policy_fully_qualified_recipe) ->
    generate_regex_msg_tuple(?POLICY_FULLY_QUALIFIED_RECIPE_REGEX,
                             <<"Malformed run list item. Policies can only contain fully qualified recipe items.">>);

regex_for(user_name) ->
   generate_regex_msg_tuple(?ANCHOR_REGEX(?USERNAME_REGEX),
                            <<"Malformed user name. Must only contain a-z, 0-9, _, or -">>);
regex_for(non_blank_string) ->
   generate_regex_msg_tuple(?ANCHOR_REGEX(?NON_BLANK_REGEX), <<"Field must have a non-empty string value">>);

regex_for(policy_file_revision_id) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX_MAX_255),
                             <<"Malformed policy name. Must be A-Z, a-z, 0-9, _, -, :, or .">>);
regex_for(policy_file_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX_MAX_255),
                             <<"Malformed policy name. Must be A-Z, a-z, 0-9, _, -, :, or .">>);

regex_for(policy_identifier) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?NAME_REGEX_MAX_255),
                             <<"Malformed policy name. Must be A-Z, a-z, 0-9, _, -, :, or .">>);
regex_for(scoped_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?SCOPED_NAME_REGEX),
                             <<"Malformed scoped name.">>).
