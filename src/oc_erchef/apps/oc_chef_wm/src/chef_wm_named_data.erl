%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Christopher Maier <cm@chef.io>
%%
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
%% @doc Resource for /data/:BAG_NAME
%%
%% This resource module serves two purposes. We initially factored it into two separate
%% modules, but this trips up webmachine for reporting allowed methods when returning 405
%% errors (and possibly other issues).
%%
%% The data_bag object is currently only a place to hang authz and hold a name. You cannot
%% GET a data_bag itself or modify it via PUT. Once created, you can only DELETE it. Note
%% that data_bag deletion will cascade the delete to all data_bag_items contained in the
%% data_bag.
%%
%% The functions that serve 'GET' and 'POST' in this resource are easiest to think about if
%% you imagine that our API looked like /data/:BAG_NAME/items. This module handles GET to
%% list all data_bag_items within a given data_bag and POST to create new data_bag_items
%% within a data_bag.
%%
%%
%% @end


-module(chef_wm_named_data).

-include("oc_chef_wm.hrl").

-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           post_is_create/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3,
         conflict_message/1,
         finalize_create_body/4 ]).

-export([
         allowed_methods/2,
         create_path/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2
       ]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #data_state{}}.

request_type() ->
    "data".

allowed_methods(Req, State) ->
    %% DELETE - delete the data_bag and all items if it has any
    %% You cannot update a data_bag, only data_bag_items, hence no PUT
    %% GET is handled by chef_wm_data_item.
    {['GET', 'POST', 'DELETE'], Req, State}.

%% DELETE is an operation on the data_bag
validate_request('DELETE', Req, State) ->
    {Req, State};
%% GET is an operation on the collection of items in the data_bag
validate_request('GET', Req, #base_state{resource_state = DataState} = State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    {Req, State#base_state{resource_state = DataState#data_state{
                                              data_bag_name = DataBagName }}};
%% POST creates a new item in the data_bag
validate_request('POST', Req, #base_state{resource_state = DataState} = State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    Body = wrq:req_body(Req),
    {ok, DataBagItemEjson} = chef_data_bag_item:parse_binary_json(Body, create),
    <<Name/binary>> = ej:get({<<"id">>}, DataBagItemEjson),
    {Req, State#base_state{resource_state = DataState#data_state{
                                              data_bag_name = DataBagName,
                                              data_bag_item_name = Name,
                                              data_bag_item_ejson = DataBagItemEjson}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_guid = OrgId,
                           resource_state = DataBagState} = State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    case chef_db:fetch(#chef_data_bag{org_id = OrgId, name = DataBagName}, DbContext) of
        not_found ->
            Message = custom_404_msg(Req, DataBagName),
            Req1 = chef_wm_util:set_json_body(Req, Message),
            {{halt, 404}, Req1, State#base_state{log_msg = node_not_found}};
        #chef_data_bag{authz_id = AuthzId} = DataBag ->
            DataBagState1 = DataBagState#data_state{chef_data_bag = DataBag,
                                                    data_bag_name = DataBagName},
            State1 = State#base_state{resource_state = DataBagState1},
            {{object, AuthzId}, Req, State1}
    end.

%% Org is checked for in malformed_request/2, data_bag is checked for in auth_info/2;
%% if we get this far, it exists.
resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #data_state{
                               data_bag_item_name = ItemName}}=State) ->
    {binary_to_list(ItemName), Req, State}.


-spec from_json(#wm_reqdata{}, #base_state{}) -> {boolean()|{halt,409|500}, #wm_reqdata{}, #base_state{}}.
from_json(Req, #base_state{resource_state = #data_state{data_bag_name = DataBagName,
                                                        data_bag_item_ejson = ItemData} } = State) ->
    oc_chef_wm_base:create_from_json(Req, State, chef_data_bag_item,
                                     {authz_id,undefined}, {DataBagName, ItemData}).

% Callback from create_from_json, which allows us to customize our body response.
finalize_create_body(_Req, #base_state{ resource_state = #data_state{data_bag_name = DataBagName,
                                                                     data_bag_item_ejson = ItemData} }, _DataBagItem, _BodyEJ ) ->
    %% The Ruby API returns created items as-is, but with added chef_type and
    %% data_bag fields. If those fields are present in the request, they are put
    %% into the raw item data, but the values are overwritten for the return. When
    %% this feature is enabled, erchef will mimic the Ruby API and force-add the
    %% chef_type and data_bag keys to the item data. Current clients appear to rely
    %% on this value, but it would be good to disable this and make the API more
    %% sane. It is very confusing that you can specify bogus values, that look like
    %% they are ignored, but actually end up in the item data.
    chef_data_bag_item:add_type_and_bag(DataBagName, ItemData).

to_json(Req, State) ->
    {items_for_data_bag(Req, State), Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 requestor_id = RequestorId,
                                 resource_state = #data_state{
                                     chef_data_bag = DataBag,
                                     data_bag_name = DataBagName}
                                } = State) ->

    ok = oc_chef_wm_base:delete_object(DbContext, DataBag, RequestorId),
    NakedBag = {[{<<"name">>, DataBagName},
                 {<<"json_class">>, <<"Chef::DataBag">>},
                 {<<"chef_type">>, <<"data_bag">>}]},
    Json = chef_json:encode(NakedBag),
    {true, wrq:set_resp_body(Json, Req), State}.

%% Private utility functions
items_for_data_bag(Req, #base_state{chef_db_context = DbContext,
                                    organization_guid = OrgId,
                                    resource_state = #data_state{
                                      data_bag_name = DataBagName}}) ->
    %% FIXME: error handling for {error, _}. Can also return {not_found, org}, but I think
    %% we will have encountered that earlier on in the request processing.
    ItemNames = chef_db:list(#chef_data_bag_item{org_id = OrgId, data_bag_name = DataBagName},
                             DbContext),
    RouteFun = oc_chef_wm_routes:bulk_route_fun(data_bag_item, DataBagName, Req),
    UriMap = [ {Name, RouteFun(Name)}  || Name <- ItemNames ],
    chef_json:encode({UriMap}).

conflict_message({BagName, ItemName}) ->
    Msg = <<"Data Bag Item '", ItemName/binary, "' already exists in Data Bag '",
            BagName/binary, "'.">>,
    {[{<<"error">>, [Msg]}]}.

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).

%% The Ruby API returns a different 404 message just for POSTs
custom_404_msg(Req, BagName) ->
    case wrq:method(Req) of
        'DELETE' ->
            chef_wm_util:not_found_message(data_bag, BagName);
        'POST' ->
            chef_wm_util:not_found_message(data_bag_missing_for_item_post, BagName);
        'GET' ->
            chef_wm_util:not_found_message(data_bag, BagName)
    end.
