%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(chef_key).

-behaviour(chef_object).

-export([
         authz_id/1,
         is_indexed/0,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         fields_for_update/1,
         fields_for_fetch/1,
         ejson_from_list/2,
         record_fields/0,
         list/2,
         set_updated/2,
         new_record/3,
         name/1,
         id/1,
         org_id/1,
         type_name/1
        ]).

%% database named queries
-export([
         create_query/0,
         update_query/0,
         delete_query/0,
         find_query/0,
         bulk_get_query/0,
         list_query/0
        ]).

-include_lib("mixer/include/mixer.hrl").
-mixin([{chef_object,[
                     {default_fetch/2, fetch},
                     {default_update/2, update}
                    ]}]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("../../include/chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

authz_id(#chef_key{}) ->
    error(not_implemented).

is_indexed() ->
    false.

ejson_for_indexing(#chef_key{}, _) ->
    error(not_indexed).

update_from_ejson(_, _) ->
    error(need_to_implement).

set_created(#chef_key{}, _ActorId) ->
    error(need_to_implement).

set_updated(#chef_key{} = _Object, _ActorId) ->
    error(need_to_implement).

fields_for_update(#chef_key{}) ->
    error(need_to_implement).

fields_for_fetch(#chef_key{id = Id, key_name = KeyName}) ->
    [Id, KeyName].

record_fields() ->
    record_info(fields, chef_key).

ejson_from_list(KeysList, URIDecorator) ->
    [ {[{<<"uri">>, URIDecorator(Name)},
        {<<"name">>, Name},
        {<<"expired">>, Expired}]} || [Name, Expired] <- KeysList ].

list(#chef_key{id = Id}, CallbackFun) when is_binary(Id) ->
    CallbackFun({list_query(), [Id], rows}).

find_query() ->
    find_key_by_id_and_name.

new_record(_OrgId, _AuthzId, _KeyData) ->
    error(need_to_implement).

name(#chef_key{key_name = KeyName}) ->
    KeyName.

id(#chef_key{id = Id}) ->
    Id.

%% TODO not sure what to do here
%% do we implement and put null for users
%% or not implement
org_id(#chef_key{}) ->
    error(need_to_implement).

type_name(#chef_key{}) ->
    key.

list_query() ->
    list_keys_for_actor.

%% TODO: need to actually implement queries in pgsql_statements
create_query() ->
    error(need_to_implement).

update_query() ->
    error(need_to_implement).

delete_query() ->
    error(need_to_implement).


bulk_get_query() ->
    error(need_to_implement).
