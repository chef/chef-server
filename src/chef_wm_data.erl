%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode, Inc.

-module(chef_wm_data).

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
         from_json/2,
         resource_exists/2,
         to_json/2
       ]).


init(Config) ->
    chef_wm_base:init(?MODULE, Config).

request_type() ->
  "data".

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State#base_state{resource_state = #data_state{}}};
validate_request('POST', Req, State) ->
    Body = wrq:req_body(Req),
    {ok, DataBagEjson} = chef_data_bag:parse_binary_json(Body, create),
    <<Name/binary>> = ej:get({<<"name">>}, DataBagEjson),
    {Req, State#base_state{resource_state = #data_state{data_bag_name = Name}}}.

auth_info(Req, State) ->
  {{create_in_container, data}, Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

create_path(Req, #base_state{resource_state = #data_state{
      data_bag_name = DataBagName}}=State) ->
  {binary_to_list(DataBagName), Req, State}.

from_json(Req, #base_state{resource_state =
                               #data_state{data_bag_name = DataBagName,
                                           data_bag_authz_id = AuthId}} = State) ->
    chef_rest_wm:create_from_json(Req, State, chef_data_bag, {authz_id, AuthzId}, DataBagName).

to_json(Req, State) ->
    {all_data_bags_json(Req, State), Req, State}.


%% Internal functions
%% @doc Generate a JSON string for a hash of rolename => role URI
%% pairs.
%% @end
%%
%% TODO: try to extract this to a common function, as this pattern
%% pops up with a few other endpoints, too
all_data_bags_json(Req, #base_state{chef_db_context = DbContext,
                                    organization_name = OrgName}) ->
    DataBagNames = chef_db:fetch_data_bags(DbContext, OrgName),
    RouteFun = chef_rest_routes:bulk_route_fun(data_bag, Req),
    UriMap= [{Name, RouteFun(Name)} || Name <- DataBagNames],
    ejson:encode({UriMap}).


%% error message functions

malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
