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

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"id">>,             {match, "^[[:alnum:]_\:\.\-]+$"}}
        ]).


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
%%
%% @end
-spec parse_binary_json( binary(), create | {update, binary()} ) ->
                               { ok, ejson_term() }. % or throw
parse_binary_json(Bin, Action) ->
    %% TODO: invalid_json will get logged by do_malformed_request, but
    %% currently without any additional information.  Do we want to
    %% emit the JSON we recieved (size limited) or some details of the
    %% parse error from ejson if we can extract it?
    DataBagItem = ejson:decode(Bin),
    validate_data_bag_item(DataBagItem, Action).

validate_data_bag_item(DataBagItem, create) ->
    case chef_json_validator:validate_json_by_regex_constraints(DataBagItem,
                                                                ?VALIDATION_CONSTRAINTS)of
        ok -> {ok, DataBagItem};
        Bad -> throw(Bad)
    end;
validate_data_bag_item(DataBagItem, {update, UrlName}) ->
    %% For update, name in URL must match name, if provided, in JSON.  Missing name is ok
    %% (you know the name by the URL you're hitting), but name mismatch is not.
    case ej:get({<<"id">>}, DataBagItem) of
        %% item JSON *must* contain id
        Name when Name =:= UrlName;
                  Name =:= undefined ->
            WithName = ej:set({<<"id">>}, DataBagItem, UrlName),
            case chef_json_validator:validate_json_by_regex_constraints(WithName,
                                                                        ?VALIDATION_CONSTRAINTS) of
                ok -> {ok, WithName};
                Bad -> throw(Bad)
            end;
        Mismatch ->
            throw({url_json_name_mismatch, {UrlName, Mismatch, "DataBagItem"}})
    end.
