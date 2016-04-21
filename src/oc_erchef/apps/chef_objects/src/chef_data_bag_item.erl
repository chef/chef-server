%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author Mark Anderson <mark@chef.io>
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

-module(chef_data_bag_item).

-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).
-export([
         add_type_and_bag/2,
         authz_id/1,
         ejson_for_indexing/2,
         is_indexed/1,
         fields_for_fetch/1,
         fields_for_update/1,
         id/1,
         name/1,
         org_id/1,
         new_record/4,
         parse_binary_json/2,
         record_fields/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         type_name/1,
         update_from_ejson/2,
         wrap_item/3,
         list/2
        ]).

%% database named queries
-export([
         bulk_get_query/1,
         create_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         update_query/1
        ]).

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% Describes the valid structure of a data bag item for use with `ej:valid/2`.
-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"id">>, {string_match, chef_regex:regex_for(data_bag_item_id)}}
         ]}).


-spec new_record(api_version(), object_id(), object_id(), {binary(), ejson_term()}) -> #chef_data_bag_item{}.
new_record(ApiVersion, OrgId, _AuthzId, {BagName, ItemData}) ->
    ItemName = ej:get({<<"id">>}, ItemData),
    Id = chef_object_base:make_org_prefix_id(OrgId, <<BagName/binary, ItemName/binary>>),
    Data = chef_db_compression:compress(chef_data_bag_item, chef_json:encode(ItemData)),
    #chef_data_bag_item{server_api_version = ApiVersion,
                        id = Id,
                        org_id = OrgId,
                        data_bag_name = BagName,
                        item_name = ItemName,
                        serialized_object = Data
                       }.

-spec id(#chef_data_bag_item{}) -> object_id().
id(#chef_data_bag_item{id = Id}) ->
    Id.

-spec org_id(#chef_data_bag_item{}) -> object_id().
org_id(#chef_data_bag_item{org_id = OrgId}) ->
    OrgId.

-spec name(#chef_data_bag_item{}) -> {binary(), binary()}.
name(#chef_data_bag_item{data_bag_name = BagName, item_name = ItemName}) ->
    {BagName, ItemName}.

-spec type_name(#chef_data_bag_item{}) -> data_bag_item.
type_name(#chef_data_bag_item{}) ->
    data_bag_item.

authz_id(#chef_data_bag_item{}) ->
    error(not_implemented).

-spec set_created(#chef_data_bag_item{}, object_id()) -> #chef_data_bag_item{}.
set_created(#chef_data_bag_item{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_data_bag_item{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

-spec set_updated(#chef_data_bag_item{}, object_id()) -> #chef_data_bag_item{}.
set_updated(#chef_data_bag_item{} = Object, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_data_bag_item{updated_at = Now, last_updated_by = ActorId}.

is_indexed(_ObjectRec) ->
    true.

-spec ejson_for_indexing(#chef_data_bag_item{}, {binary, ejson_term()} | ejson_term()) -> ejson_term().
ejson_for_indexing(#chef_data_bag_item{} = BagItem, {_BagName, Item}) when is_binary(_BagName) ->
    ejson_for_indexing(BagItem, Item);
ejson_for_indexing(#chef_data_bag_item{data_bag_name = BagName,
                                       item_name = ItemName}, Item) ->
    %% See Chef::DataBagItem#to_hash
    %% We basically set data_bag and chef_type key against the original data bag item.
    ItemName = ej:get({<<"id">>}, Item),
    ej:set({<<"data_bag">>}, ej:set({<<"chef_type">>}, Item, <<"data_bag_item">>), BagName).


-spec update_from_ejson(#chef_data_bag_item{}, ejson_term()) -> #chef_data_bag_item{}.
update_from_ejson(#chef_data_bag_item{} = DataBagItem, DataBagItemData) ->
    Name = ej:get({<<"id">>}, DataBagItemData),
    DataBagItemJson = chef_json:encode(DataBagItemData),
    Data = chef_db_compression:compress(chef_data_bag_item, DataBagItemJson),
    DataBagItem#chef_data_bag_item{item_name = Name, serialized_object = Data}.

bulk_get_query(_ObjectRec) ->
    bulk_get_data_bag_items.

create_query(_ObjectRec) ->
    insert_data_bag_item.

delete_query(_ObjectRec) ->
    delete_data_bag_item_by_id.

find_query(_ObjectRec) ->
    find_data_bag_item_by_orgid_name.

list_query(_ObjectRec) ->
    list_data_bag_items_for_data_bag.

update_query(_ObjectRec) ->
    update_data_bag_item_by_id.

fields_for_update(#chef_data_bag_item{last_updated_by = LastUpdatedBy,
                                      updated_at = UpdatedAt,
                                      serialized_object = Object,
                                      id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Object, Id].

fields_for_fetch(#chef_data_bag_item{org_id = OrgId,
                                     data_bag_name = BagName,
                                     item_name = ItemName}) ->
    [OrgId, BagName, ItemName].

record_fields(_ApiVersion) ->
    record_info(fields, chef_data_bag_item).

-spec add_type_and_bag(BagName :: binary(), Item :: ejson_term()) -> ejson_term().
%% @doc Returns data bag item EJSON `Item' with keys `chef_type' and `data_bag' added.
add_type_and_bag(BagName, Item) ->
    Item1 = ej:set({<<"chef_type">>}, Item, <<"data_bag_item">>),
    ej:set({<<"data_bag">>}, Item1, BagName).

-spec wrap_item(binary(), binary(), ejson_term()) -> ejson_term().
%% @doc Returns a wrapped EJSON Chef::DataBagItem structure
wrap_item(BagName, ItemName, Item) ->
    {[{<<"name">>, <<"data_bag_item_", BagName/binary, "_", ItemName/binary>>},
      {<<"json_class">>, <<"Chef::DataBagItem">>},
      {<<"chef_type">>, <<"data_bag_item">>},
      {<<"data_bag">>, BagName},
      {<<"raw_data">>, Item}]}.

%% @doc Convert a binary JSON string representing a Chef data_bag into an EJson-encoded
%% Erlang data structure.
-spec parse_binary_json( binary(), create | {update, binary()} ) ->
                               { ok, ejson_term() }. % or throw
parse_binary_json(Bin, Action) ->
    DataBagItem = unwrap_item(chef_json:decode(Bin)),
    validate_data_bag_item(DataBagItem, Action).

-spec validate_data_bag_item(ej:json_object(), create | {update, binary()})
                            -> {ok, ej:json_object()}.
validate_data_bag_item(DataBagItem, create) ->
    validate(DataBagItem);
validate_data_bag_item(DataBagItem, {update, IdFromUrl}) ->
    validate(normalized_data_bag_for_update(DataBagItem, IdFromUrl)).

%% @doc Common logic for validating a data bag item, whether it's for creation or update
-spec validate(ej:json_object()) -> {ok, ej:json_object()}.
validate(DataBagItem) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, DataBagItem) of
        ok ->
            {ok, DataBagItem};
        Bad ->
            throw(Bad)
    end.

%% @doc For data bag update, the id in URL must match the id (if provided) in the JSON.
%% Missing the id is ok (you know from the URL you're hitting), but an id mismatch is not.
%%
%% Returns `DataBagItem` with `IdFromUrl` unambiguously set as its id, or throws if it was
%% mismatched.
-spec normalized_data_bag_for_update(ej:json_object(), binary()) ->ej:json_object().
normalized_data_bag_for_update(DataBagItem, IdFromUrl) ->
    case ej:get({<<"id">>}, DataBagItem)of
        undefined ->
            ej:set({<<"id">>}, DataBagItem, IdFromUrl);
        IdFromUrl ->
            DataBagItem;
        Mismatch ->
            throw({url_json_name_mismatch, {IdFromUrl, Mismatch, "DataBagItem"}})
    end.

unwrap_item(Ejson) ->
    case is_wrapped_item(Ejson) of
        true ->
            ej:get({<<"raw_data">>}, Ejson);
        false ->
            Ejson
    end.

is_wrapped_item(Ejson) ->
    (undefined =/= ej:get({<<"raw_data">>}, Ejson) andalso
     <<"Chef::DataBagItem">> =:= ej:get({<<"json_class">>}, Ejson)).

-spec(list(#chef_data_bag_item{}, chef_object:select_callback()) -> chef_object:select_return()).
list(#chef_data_bag_item{org_id = OrgId, data_bag_name = DataBagName} = DBI, CallBackFun) ->
    CallBackFun({list_query(DBI), [OrgId, DataBagName], [item_name]}).


set_api_version(ObjectRec, Version) ->
    ObjectRec#chef_data_bag_item{server_api_version = Version}.
