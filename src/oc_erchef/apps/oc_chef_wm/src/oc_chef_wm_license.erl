%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Douglas Triggs <doug@chef.io>
%% @copyright 2014 Chef Software Inc

%% @doc license status endpoint

-module(oc_chef_wm_license).

-include("oc_chef_wm.hrl").

%% Webmachine resource callbacks
-mixin([{oc_chef_wm_base, [content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           validate_request/3,
                           {allow_all/2, auth_info},
                           service_available/2]}]).

%% chef_wm behaviour callbacks
-behavior(chef_wm).
-export([init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0
        ]).

%% Our implemented webmachine callbacks
-export([allowed_methods/2, to_json/2]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

% We're as simple as it gets, no state here.
init_resource_state(_Config) ->
    {ok, undefined}.

request_type() ->
    "license".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

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

-spec malformed_request_message(any(), wm_req(), #base_state{}) -> no_return().
malformed_request_message(Any, Req, State) ->
    oc_chef_wm_base:default_malformed_request_message(Any, Req, State).
