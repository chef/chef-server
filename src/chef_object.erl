%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Falcon <seth@opscode.com>
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

-module(chef_object).

-include("chef_types.hrl").
-include_lib("ej/include/ej.hrl").

-callback new_record(OrgId :: object_id(),
                     AuthzId :: object_id() | unset,
                     ObjectEjson :: ejson_term() |
                                    binary() |
                                    {binary(), ejson_term()} |
                                    {ejson_term(), _}) ->
    tuple().

-callback name(tuple()) ->
    binary() | {binary(), binary()}.

-callback id(tuple()) ->
    object_id().

-callback type_name(tuple()) ->
    atom().

-export([
         new_record/4,
         name/1,
         id/1,
         type_name/1
        ]).

-spec new_record(RecType :: atom(),
                 OrgId :: object_id(),
                 AuthzId :: object_id() | unset,
                 ObjectEjson :: ejson_term() |
                                binary() |
                                {binary(), ejson_term()} |
                                {ejson_term(), _}) ->
                        tuple().
new_record(RecType, OrgId, AuthzId, ObjectEjson) ->
    RecType:new_record(OrgId, AuthzId, ObjectEjson).

-spec name(tuple()) -> binary() | {binary(), binary()}.
name(Rec) ->
    call(Rec, name).

-spec id(tuple()) -> object_id().
id(Rec) ->
    call(Rec, id).

-spec type_name(tuple()) -> atom().
type_name(Rec) ->
    call(Rec, type_name).

call(Rec, Fun) ->
    Mod = element(1, Rec),
    Mod:Fun(Rec).
