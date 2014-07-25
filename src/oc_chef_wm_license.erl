%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@getchef.com>
%% @copyright 2014 Chef Software Inc

%% @doc license status endpoint

-module(oc_chef_wm_license).

-include_lib("chef_wm/include/chef_wm.hrl").
-include("oc_chef_wm.hrl").

-mixin([{chef_wm_base, [content_types_provided/2,
                        finish_request/2,
                        malformed_request/2,
                        ping/2 ]}]).

-mixin([{oc_chef_wm_base, [forbidden/2,
                          is_authorized/2,
                          service_available/2]}]).

-behavior(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

-export([allowed_methods/2,
         to_json/2]).

init(Config) ->
    chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #base_state{}}.

request_type() ->
    "license".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {authorized, Req, State}.

to_json(Req, State) ->
    {chef_json:encode(license_body(State)), Req, State}.

%% internal functions

license_body(State) ->
    License = envy:get(oc_chef_wm, node_license, pos_integer),
    Count = node_count(State),
    Exceeded = Count > License,
    UpgradeURL = envy:get(oc_chef_wm, upgrade_url, binary),
    {[ {<<"limit_exceeded">>, Exceeded},
       {<<"node_license">>, License},
       {<<"node_count">>, Count},
       {<<"upgrade_url">>, UpgradeURL} ]}.

node_count(#base_state{chef_db_context = DbContext}) ->
    chef_db:count_nodes(DbContext).

%% error message functions

malformed_request_message(Any, Req, State) ->
    chef_wm_util:malformed_request_message(Any, Req, State).

