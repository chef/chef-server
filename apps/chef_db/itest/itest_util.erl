%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2015 Chef, Inc. All Rights Reserved.
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

-module(itest_util).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("../../include/chef_db.hrl").
-include("../../include/chef_types.hrl").

create_record(Record) ->
    Query = chef_object:create_query(Record),
    FlattenedRecord = chef_object:flatten(Record),
    chef_sql:create_object(Query, FlattenedRecord).

fetch_record(Record) ->
    chef_sql:fetch_object(
      chef_object:fields_for_fetch(Record),
      element(1, Record),
      chef_object:find_query(Record),
      chef_object:record_fields(Record)
     ).

update_record(Record) ->
    chef_sql:do_update(chef_object:update_query(Record), chef_object:fields_for_update(Record)).

delete_record(Record) ->
    chef_sql:delete_object(chef_object:delete_query(Record), chef_object:id(Record)).

list_records(Record) ->
    chef_sql:fetch_object_names(Record).
