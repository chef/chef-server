%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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


-module(chef_wm_named_data_item).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

-export([allowed_methods/2,
         conflict_message/1,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2,
         finalize_update_body/3 ]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #data_state{}}.

request_type() ->
  "data".

allowed_methods(Req, State) ->
    {['GET', 'PUT', 'DELETE'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State};
validate_request('DELETE', Req, State) ->
    {Req, State};
validate_request('PUT', Req, #base_state{resource_state = DataState0} = State) ->
    %% FIXME: should we also fetch the data_bag here to make sure that it exists and ensure
    %% we have authz? With the current setup, we will deny malformed requests with 400 even
    %% for a missing or no-perms data_bag.
    Name = chef_wm_util:object_name(data_bag_item, Req),
    Body = wrq:req_body(Req),
    {ok, Item} = chef_data_bag_item:parse_binary_json(Body, {update, Name}),
    DataState = DataState0#data_state{data_bag_item_ejson = Item},
    {Req, State#base_state{resource_state = DataState}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = DataBagState} = State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    ItemName = chef_wm_util:object_name(data_bag_item, Req),
    case chef_db:fetch(#chef_data_bag{org_id = OrgId, name = DataBagName}, DbContext) of
        not_found ->
            Message = custom_404_msg(Req, DataBagName, ItemName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = node_not_found}};
        #chef_data_bag{authz_id = AuthzId} = DataBag ->
            DataBagState1 = DataBagState#data_state{chef_data_bag = DataBag,
                                                    data_bag_name = DataBagName,
                                                    data_bag_item_name = ItemName},
            State1 = State#base_state{resource_state = DataBagState1},
            {{object, AuthzId}, Req, State1}
    end.

%% If we get here, we know that the data_bag exists and we have authz, here we'll check that
%% the item exists. If items grow their own authz, this logic will move into an enhanced
%% forbidden function.
resource_exists(Req, #base_state{chef_db_context = DbContext,
                                 organization_guid = OrgId,
                                 resource_state = DataBagState} = State) ->
    DataBagName = DataBagState#data_state.data_bag_name,
    ItemName = chef_wm_util:object_name(data_bag_item, Req),
    case chef_db:fetch(#chef_data_bag_item{org_id = OrgId,
                                           data_bag_name = DataBagName,
                                           item_name = ItemName},
                       DbContext) of
        not_found ->
            Message = custom_404_msg(Req, DataBagName, ItemName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            %% WARNING: Webmachine will not halt here if this is a PUT request and we return
            %% {false, Req1, State}; So we force the halt since we do not want PUT only for
            %% update. We don't have this problem for simple objects, such as nodes, because
            %% the 404 is halted explicitly in forbidden.
            {{halt, 404}, Req1, State};
        #chef_data_bag_item{} = Item ->
            DataBagState1 = DataBagState#data_state{chef_data_bag_item = Item},
            State1 = State#base_state{resource_state = DataBagState1},
            {true, Req, State1}
    end.

from_json(Req, #base_state{resource_state = #data_state{
                             chef_data_bag_item = Item,
                             data_bag_item_ejson = ItemData}} = State) ->
    oc_chef_wm_base:update_from_json(Req, State, Item, ItemData).

finalize_update_body(_Req, #base_state{ resource_state = #data_state{data_bag_name = BagName,
                                                                     data_bag_item_ejson = ItemData}},
                    _BodyEJ) ->
    chef_data_bag_item:add_type_and_bag(BagName, ItemData).



to_json(Req, #base_state{resource_state = DataBagState} = State) ->
    Item = DataBagState#data_state.chef_data_bag_item,
    JSON = Item#chef_data_bag_item.serialized_object,
    {chef_db_compression:decompress(JSON), Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #data_state{
                                     data_bag_name = BagName,
                                     data_bag_item_name = ItemName,
                                     chef_data_bag_item = Item}
                                }=State) ->

    ok = oc_chef_wm_base:delete_object(DbContext, Item, RequestorId),
    Json = chef_db_compression:decompress(Item#chef_data_bag_item.serialized_object),
    EjsonItem = chef_json:decode(Json),
    WrappedItem = chef_data_bag_item:wrap_item(BagName, ItemName, EjsonItem),
    {true, chef_wm_util:set_json_body(Req, WrappedItem), State}.

%% Private utility functions
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

%% The Ruby API returns a different 404 message just for POSTs
custom_404_msg(Req, BagName, ItemName) ->
    case wrq:method(Req) of
        Update when Update =:= 'POST' ->
            chef_wm_util:not_found_message(data_bag_missing_for_item_post, BagName);
        'PUT' ->
            chef_wm_util:not_found_message(data_bag_item2, {BagName, ItemName});
        'GET' ->
            chef_wm_util:not_found_message(data_bag_item2, {BagName, ItemName});
        'DELETE' ->
            chef_wm_util:not_found_message(data_bag_item1, {BagName, ItemName})
    end.

-spec conflict_message({binary(), binary()}) -> {[{<<_:40>>, [any(), ...]}, ...]}.
conflict_message({BagName, ItemName}) ->
    Msg = <<"Data Bag Item '", ItemName/binary, "' already exists in Data Bag '",
            BagName/binary, "'.">>,
    {[{<<"error">>, [Msg]}]}.
