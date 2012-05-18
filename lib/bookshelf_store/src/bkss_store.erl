%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.
-module(bkss_store).

-export([behaviour_info/1]).

-export([new/2,
         bucket_create/2,
         bucket_delete/2,
         bucket_exists/2,
         bucket_list/1,
         obj_copy/5,
         obj_delete/3,
         obj_exists/3,
         obj_list/2,
         obj_meta/3,
         obj_create/4,
         obj_get/3,
         obj_recv/6,
         obj_send/4
        ]).

-export_type([store/0, bucket/0, path/0, bucket_name/0, object/0]).
-include_lib("bookshelf_store/include/bookshelf_store.hrl").

%%%===================================================================
%%% Types
%%%===================================================================
-record(store_t, {callback, data}).
-opaque store() :: record(store_t).

-type bucket_name() :: binary().
-type path() :: binary().
-type object() :: record(object).
-type bucket() :: record(bucket).

%%===================================================================
%% External API
%%===================================================================
-spec behaviour_info(term()) -> term().
behaviour_info(callbacks) ->
    [{new,1},
     {bucket_create,2},
     {bucket_delete,2},
     {bucket_exists,2},
     {bucket_list,1},
     {obj_copy,5},
     {obj_delete,3},
     {obj_exists,3},
     {obj_list,2},
     {obj_meta,3},
     {obj_create,4},
     {obj_get,3},
     {obj_recv,6},
     {obj_send,4}
    ];
behaviour_info(_) ->
    undefined.

-spec new(atom(), term()) -> store().
new(ModuleName, Data) when is_atom(ModuleName) ->
    #store_t{callback=ModuleName, data=ModuleName:new(Data)}.

-spec bucket_list(store()) -> [bucket()].
bucket_list(#store_t{callback=Mod,data=Data}) ->
    Mod:bucket_list(Data).

-spec bucket_exists(store(), bucket_name()) -> boolean().
bucket_exists(#store_t{callback=Mod,data=Data}, Bucket) ->
    Mod:bucket_exists(Data, Bucket).

-spec bucket_create(store(), bucket_name()) ->
                           {store(), ok | {error, Reason::term()}}.
bucket_create(#store_t{callback=Mod,data=Data0}, Bucket) ->
    {Data1, R} = Mod:bucket_create(Data0, Bucket),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec bucket_delete(store(), bucket_name()) -> {store(), ok | {error, Reason::term()}}.
bucket_delete(#store_t{callback=Mod,data=Data0}, Bucket) ->
    {Data1, R} = Mod:bucket_delete(Data0, Bucket),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec obj_list(store(), bucket_name()) -> [object()].
obj_list(#store_t{callback=Mod,data=Data0}, Bucket) ->
    Mod:obj_list(Data0, Bucket).

-spec obj_exists(store(), bucket_name(), path()) -> boolean().
obj_exists(#store_t{callback=Mod,data=Data0}, Bucket, Path) ->
    Mod:obj_exists(Data0, Bucket, Path).

-spec obj_delete(store(), bucket_name(), path()) -> {store(), ok | {error, Reason::term()}}.
obj_delete(#store_t{callback=Mod,data=Data0}, Bucket, Path) ->
    {Data1, R} = Mod:obj_delete(Data0, Bucket, Path),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec obj_meta(store(), bucket_name(), path()) ->
                      {ok, object()} | {error, Reason::term()}.
obj_meta(#store_t{callback=Mod,data=Data0}, Bucket, Path) ->
    Mod:obj_meta(Data0, Bucket, Path).

-spec obj_create(store(), bucket_name(), path(), iolist()) ->
                      {store(), ok | {error, Reason::term()}}.
obj_create(#store_t{callback=Mod,data=Data0}, Bucket, Path, Data) ->
    {Data1, R} = Mod:obj_create(Data0, Bucket, Path, Data),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec obj_get(store(), bucket_name(), path()) ->
                     {store(), ok | {error, Reason::term()}}.
obj_get(#store_t{callback=Mod,data=Data0}, Bucket, Path) ->
    Mod:obj_get(Data0, Bucket, Path).

-spec obj_copy(store(), bucket_name(), path(), bucket_name(), path()) ->
                      {store(), ok | {error, Reason::term()}}.
obj_copy(#store_t{callback=Mod,data=Data0}, FromBucket, FromPath, ToBucket, ToPath) ->
    {Data1, R} = Mod:obj_copy(Data0, FromBucket, FromPath, ToBucket, ToPath),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec obj_send(store(), bucket_name(), path(), bkss_transport:trans()) ->
                      {store(), ok | {error, Reason::term()}}.
obj_send(#store_t{callback=Mod,data=Data0}, Bucket, Path, Bridge) ->
    {Data1, R} = Mod:obj_send(Data0, Bucket, Path, Bridge),
    {#store_t{callback=Mod, data=Data1}, R}.

-spec obj_recv(store(), bucket_name(), path(), bkss_transport:trans(),
               binary(), non_neg_integer()) ->
                      {store(), ok | {error, Reason::term()}}.
obj_recv(#store_t{callback=Mod,data=Data0}, Bucket, Path, Bridge, Buffer, Length) ->
    {Data1, Obj} = Mod:obj_recv(Data0, Bucket, Path, Bridge, Buffer, Length),
    {#store_t{callback=Mod, data=Data1}, Obj}.
