%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @copyright 2012 Opscode, Inc.
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

-include("chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_accepted/2,
                        content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2,
                        post_is_create/2]}]).

-mixin([{?BASE_RESOURCE, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

%% chef_wm behaviour callbacks
-behaviour(chef_wm).
-export([
         auth_info/2,
         init/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3
        ]).

-export([
         allowed_methods/2,
         create_path/2,
         delete_resource/2,
         from_json/2,
         resource_exists/2,
         to_json/2
       ]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

request_type() ->
    "data".

allowed_methods(Req, State) ->
    %% DELETE - delete the data_bag and all items if it has any
    %% You cannot update a data_bag, only data_bag_items, hence no PUT
    %% GET is handled by chef_wm_data_item.
    {['GET', 'POST', 'DELETE'], Req, State}.

%% DELETE is an operation on the data_bag
validate_request('DELETE', Req, State) ->
    {Req, State#base_state{resource_state = #data_state{}}};
%% GET is an operation on the collection of items in the data_bag
validate_request('GET', Req, State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    {Req, State#base_state{resource_state = #data_state{
                             data_bag_name = DataBagName }}};
%% POST creates a new item in the data_bag
validate_request('POST', Req, State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    Body = wrq:req_body(Req),
    {ok, DataBagItemEjson} = chef_data_bag_item:parse_binary_json(Body, create),
    <<Name/binary>> = ej:get({<<"id">>}, DataBagItemEjson),
    {Req, State#base_state{resource_state = #data_state{
                             data_bag_name = DataBagName,
                             data_bag_item_name = Name,
                             data_bag_item_ejson = DataBagItemEjson}}}.

auth_info(Req, #base_state{chef_db_context = DbContext,
                           organization_name = OrgName,
                           resource_state = DataBagState} = State) ->
    DataBagName = chef_wm_util:object_name(data_bag, Req),
    case chef_db:fetch_data_bag(DbContext, OrgName, DataBagName) of
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

from_json(Req, #base_state{chef_db_context = DbContext,
                           resource_state = #data_state{data_bag_name = DataBagName,
                                                        data_bag_item_name = ItemName,
                                                        data_bag_item_ejson = ItemData},
                           organization_guid = OrgId,
                           requestor = #chef_requestor{authz_id = ActorId},
                           %reqid = _ReqId,
                           db_type = DbType} = State) ->

    %% Note: potential race condition.  If we don't have perms, the create will fail.
    %% Although we checked rights above, they could have changed.

    %% the 'ignored' atom is where an AuthzId would typically go
    DataBagItem = chef_object:new_record(chef_data_bag_item, OrgId, ignored,
                                         {DataBagName, ItemData}, DbType),
    %% We send the data_bag_item data to solr for indexing *first*. If it fails, we'll error out on a
    %% 500 and client can retry. If we succeed and the db call fails or conflicts, we can
    %% safely send a delete to solr since this is a new data_bag_item with a unique ID unknown to the
    %% world.
    #chef_data_bag_item{id = Id} = DataBagItem,
    ok = chef_index_queue:set(data_bag_item, Id,
                              chef_otto:dbname(OrgId),
                              chef_object:ejson_for_indexing(DataBagItem, ItemData)),

    case chef_db:create_data_bag_item(DbContext, DataBagItem, ActorId) of
        {conflict, _} ->
            LogMsg = {data_bag_name_conflict, DataBagName},
            {{halt, 409}, chef_wm_util:set_json_body(Req,
                                                       conflict_message(data_bag_item, ItemName, DataBagName)),
             State#base_state{log_msg = LogMsg}};
        ok ->
            LogMsg = {created, DataBagName},
            Uri = ?BASE_ROUTES:route(data_bag, Req, [{name, DataBagName}]),
            %% set the Location header to return a 201 Created response. Don't use
            %% set_uri_of_created_resource since we want the body to be the data_bag_item
            %% data.
            Req1 = wrq:set_resp_header("Location", binary_to_list(Uri), Req),
            %% The Ruby API returns created items as-is, but with added chef_type and
            %% data_bag fields. If those fields are present in the request, they are put
            %% into the raw item data, but the values are overwritten for the return. When
            %% this darklaunch feature is enabled, erchef will mimic the Ruby API and
            %% force-add the chef_type and data_bag keys to the item data. It should be safe
            %% to disable this and will make the API more sane. It is very confusing that
            %% you can specify bogus values, that look like they are ignored, but actually
            %% end up in the item data.
            ItemDataWithCruft = case darklaunch:is_enabled(<<"add_type_and_bag_to_items">>) of
                                    true ->
                                        chef_data_bag_item:add_type_and_bag(DataBagName,
                                                                            ItemData);
                                    false ->
                                        ItemData
                                end,
            Req2 = chef_wm_util:set_json_body(Req1, ItemDataWithCruft),
            {true, Req2, State#base_state{log_msg = LogMsg}};
        What ->
            %% ignore return value of solr delete, this is best effort.
            chef_object_db:delete_from_solr(DataBagItem),
            {{halt, 500}, Req, State#base_state{log_msg = What}}
    end.

to_json(Req, State) ->
    {items_for_data_bag(Req, State), Req, State}.

delete_resource(Req, #base_state{chef_db_context = DbContext,
                                 resource_state = #data_state{
                                     chef_data_bag = DataBag,
                                     data_bag_name = DataBagName},
                                 requestor = #chef_requestor{
                                     authz_id = RequestorId}} = State) ->

    ok = chef_object_db:delete(DbContext, DataBag, RequestorId),
    NakedBag = {[{<<"name">>, DataBagName},
                 {<<"json_class">>, <<"Chef::DataBag">>},
                 {<<"chef_type">>, <<"data_bag">>}]},
    Json = ejson:encode(NakedBag),
    {true, wrq:set_resp_body(Json, Req), State}.

%% Private utility functions
items_for_data_bag(Req, #base_state{chef_db_context = DbContext,
                                    organization_name = OrgName,
                                    resource_state = #data_state{
                                      data_bag_name = DataBagName}}) ->
    %% FIXME: error handling for {error, _}. Can also return {not_found, org}, but I think
    %% we will have encountered that earlier on in the request processing.
    ItemNames = chef_db:fetch_data_bag_items(DbContext, OrgName, DataBagName),
    RouteFun = ?BASE_ROUTES:bulk_route_fun(data_bag_item, DataBagName, Req),
    UriMap = [ {Name, RouteFun(Name)}  || Name <- ItemNames ],
    ejson:encode({UriMap}).

conflict_message(data_bag_item, ItemName, BagName) ->
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
