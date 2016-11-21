%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
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

-module(chef_json_validator).

-export([
         attribute_spec/0,
         env_run_lists_spec/0,
         run_list_spec/0
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_regex.hrl").

%% @doc `ej:valid/2` spec for attribute hashes.  This can be used for,
%% e.g., a node's default, override, normal, and automatic attributes,
%% a role's default and override attributes, etc.
%%
%% It specifies that all keys must be strings, but lets values take on
%% any type.
attribute_spec() ->
    {object_map, {{keys, string},
                  {values, any_value}}}.

%% @doc `ej:valid/2` spec for validating a run list.  This can be used
%% for nodes and roles.
%%
%% Note that it is valid for a run list to be empty.
run_list_spec() ->
    {array_map,
     {fun_match, {fun valid_run_list_item/1,
                  string,
                  <<"Invalid run list entry">>}}}.

%% @doc `ej:valid/2` spec for validating an `env_run_lists` map of a role.
%%
%% Keys must be valid environment names, and values must be valid run
%% lists.
%%
%% Note that it is valid for the map as a whole to be empty.
env_run_lists_spec() ->
    {object_map, {
       {keys, {string_match, chef_regex:regex_for(environment_name)}},
       {values, run_list_spec()}}}.

%%------------------------------------------------------------------------------
%% Private Functions
%%------------------------------------------------------------------------------

-spec valid_run_list_item(Item :: any()) -> ok | error.
valid_run_list_item(Item) when is_binary(Item) ->
    ItemType = item_type(Item),
    {RegEx, _Msg} = chef_regex:regex_for(ItemType),
    case re:run(Item, RegEx, [{capture, none}]) of
        match -> ok;
        nomatch -> error
    end;
valid_run_list_item(_) ->
    error.

-spec item_type(binary()) -> qualified_recipe | qualified_role | unqualified_recipe.
item_type(<<"recipe[", _Rest/binary>>) ->
    qualified_recipe;
item_type(<<"role[", _Rest/binary>>) ->
    qualified_role;
item_type(UnqualifiedRecipe) when is_binary(UnqualifiedRecipe) ->
    unqualified_recipe.
