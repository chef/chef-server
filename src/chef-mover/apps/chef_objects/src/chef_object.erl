%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Falcon <seth@chef.io>
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

-type object_rec() :: tuple().
-type select_return() :: not_found | list(object_rec()) | object_rec() |
                         {error, _}.
-type select_callback() :: fun(({ QueryName ::atom(), BindParameters :: list(),
                                  ReturnFieldNames :: [atom()]}
                              | {QueryName :: atom(), BindParameters :: list()}
                              | {QueryName :: atom(), BindParameters :: list(),
                                 ReturnTransform :: tuple()}) ->
                                      select_return()).

-type update_return() :: pos_integer() | not_found | {conflict, _} | {error, _}.


-callback authz_id(object_rec()) -> object_id().
-callback is_indexed() -> boolean().
-callback ejson_for_indexing(object_rec(), ejson_term()) -> ejson_term().
-callback update_from_ejson(object_rec(), any()) -> object_rec().

-callback set_created(object_rec(), object_id()) -> object_rec().
-callback set_updated(object_rec(), object_id()) -> object_rec().

-callback create_query() -> atom().
-callback update_query() -> atom().
-callback delete_query() -> atom().
-callback find_query() -> atom().
-callback bulk_get_query() -> atom().

-callback fields_for_update(object_rec()) -> list().
-callback fields_for_fetch(object_rec()) -> list().
-callback record_fields() -> list(atom()).
-callback list(object_rec(), select_callback()) -> select_return().
-callback fetch(object_rec(), select_callback()) -> select_return().
-callback update(object_rec(), select_callback()) ->
     update_return().


-callback new_record(OrgId :: object_id(),
                     AuthzId :: object_id() | unset,
                     ObjectEjson :: ejson_term() |
                                    binary() |
                                    {binary(), ejson_term()} |
                                    {ejson_term(), _}) ->
    object_rec().

-callback name(object_rec()) ->
    binary() | {binary(), binary()}.

-callback id(object_rec()) ->
    object_id().

-callback org_id(object_rec()) -> object_id().

-callback type_name(object_rec()) ->
    atom().

-export([
         authz_id/1,
         set_created/2,
         set_updated/2,
         ejson_for_indexing/2,
         is_indexed/1,
         update_from_ejson/2,
         new_record/4,
         name/1,
         org_id/1,
         id/1,
         type_name/1,

         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         find_query/1,
         update_query/1,

         fields_for_fetch/1,
         fields_for_update/1,
         record_fields/1,
         flatten/1,

         list/2,
         fetch/2,
         update/3,
         delete/2,
         default_fetch/2,
         default_update/2
        ]).

-export_type([
         select_callback/0,
         select_return/0,
         object_rec/0
         ]).

-spec new_record(RecType :: atom(),
                 OrgId :: object_id(),
                 AuthzId :: object_id() | unset,
                 ObjectEjson :: ejson_term() |
                                binary() |
                                {binary(), ejson_term()} |
                                {ejson_term(), _}) ->
                        object_rec().
new_record(RecType, OrgId, AuthzId, ObjectEjson) ->
    RecType:new_record(OrgId, AuthzId, ObjectEjson).

-spec name(object_rec()) -> binary() | {binary(), binary()}.
name(Rec) ->
    call(Rec, name).

-spec id(object_rec()) -> object_id().
id(Rec) ->
    call(Rec, id).

-spec org_id(object_rec()) -> object_id().
org_id(Rec) ->
    call(Rec, org_id).

-spec type_name(object_rec()) -> atom().
type_name(Rec) ->
    call(Rec, type_name).

-spec authz_id(object_rec()) -> object_id().
authz_id(Rec) ->
    call(Rec, authz_id).

-spec ejson_for_indexing(object_rec(), ejson_term()) -> ejson_term().
ejson_for_indexing(Rec, Data) ->
    Mod = callback_mod(Rec),
    Mod:ejson_for_indexing(Rec, Data).

-spec update_from_ejson(object_rec(), any()) -> object_rec().
update_from_ejson(Rec, Data) ->
    Mod = callback_mod(Rec),
    Mod:update_from_ejson(Rec, Data).

-spec set_updated(object_rec(), object_id()) -> object_rec().
set_updated(Rec, ActorId) ->
    Mod = callback_mod(Rec),
    Mod:set_updated(Rec, ActorId).

-spec set_created(object_rec(), object_id()) -> object_rec().
set_created(Rec, ActorId) ->
    Mod = callback_mod(Rec),
    Mod:set_created(Rec, ActorId).

-spec record_fields(object_rec()) -> list(atom()).
record_fields(Rec) ->
    call0(Rec, record_fields).

create_query(Rec) ->
    call0(Rec, create_query).

update_query(Rec) ->
    call0(Rec, update_query).

delete_query(Rec) ->
    call0(Rec, delete_query).

find_query(Rec) ->
    call0(Rec, find_query).

bulk_get_query(Rec) ->
    call0(Rec, bulk_get_query).

fields_for_update(Rec) ->
    call(Rec, fields_for_update).

fields_for_fetch(Rec) ->
    call(Rec, fields_for_fetch).

is_indexed(Rec) ->
    call0(Rec, is_indexed).

-spec list(Rec:: object_rec(), CallbackFun :: select_callback()) ->
                  select_return().
list(Rec, CallbackFun) ->
    Mod = element(1, Rec),
    Mod:list(Rec, CallbackFun).

fetch(Rec, CallbackFun) ->
    Mod = element(1, Rec),
    Mod:fetch(Rec, CallbackFun).

update(Rec, ActorId, CallbackFun) ->
    Mod = element(1, Rec),
    Mod:update(chef_object:set_updated(Rec, ActorId), CallbackFun).

delete(Rec, CallbackFun) ->
    call_if_exported(Rec, delete, [Rec, CallbackFun], fun do_delete/2).

%% Return the callback module for a given object record type. We're putting the abstraction
%% in place in case we need to do something other than the identity mapping of record name
%% to callback module name that we're doing there. If we needed to swap in something else,
%% we could do that mapping here.
callback_mod(Rec) ->
    element(1, Rec).

call(Rec, Fun) ->
    Mod = callback_mod(Rec),
    Mod:Fun(Rec).

call0(Rec, Fun) ->
    Mod = callback_mod(Rec),
    Mod:Fun().


default_fetch(Rec, CallbackFun) ->
    CallbackFun({call0(Rec, find_query),
            call(Rec, fields_for_fetch),
            {first_as_record, [element(1, Rec), call0(Rec, record_fields)]}}).

default_update(ObjectRec, CallbackFun) ->
    QueryName = chef_object:update_query(ObjectRec),
    UpdatedFields = chef_object:fields_for_update(ObjectRec),
    CallbackFun({QueryName, UpdatedFields}).

flatten(ObjectRec) ->
    call_if_exported(ObjectRec, flatten, [ObjectRec], fun default_flatten/1).

call_if_exported(ObjectRec, FunName, Args, DefaultFun) ->
    Mod = callback_mod(ObjectRec),
    case erlang:function_exported(Mod, FunName, length(Args)) of
        true ->
            erlang:apply(Mod, FunName, Args);
        false  ->
            erlang:apply(DefaultFun, Args)
    end.
default_flatten(ObjectRec) ->
    [_RecName|Tail] = tuple_to_list(ObjectRec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, ObjectRec});
        false -> ok
    end,
    Tail.

is_undefined(undefined) ->
    true;
is_undefined(_) ->
    false.

do_delete(ObjectRec, CallbackFun) ->
    QueryName = delete_query(ObjectRec),
    Id = id(ObjectRec),
    CallbackFun({QueryName, [Id]}).
