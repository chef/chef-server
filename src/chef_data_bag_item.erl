%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author Mark Anderson <mark@opscode.com>
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

-export([
         add_type_and_bag/2,
         parse_binary_json/2,
         wrap_item/3
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

%% Describes the valid structure of a data bag item for use with `ej:valid/2`.
-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"id">>, {string_match, chef_regex:regex_for(data_bag_item_id)}}
         ]}).

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
