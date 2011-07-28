%%%-------------------------------------------------------------------
%%% @author Kevin Smith <kevin@opscode.com>
%%% @copyright (C) 2011, Opscode, Inc.
%%% @doc
%%% REST resource for monitoring status of erchef
%%% @end
%%% @copyright Copyright 2011 Opscode, Inc.
%%% @version 0.1
%%%-------------------------------------------------------------------
-module(chef_rest_status_resource).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").


init(_Any) ->
    {ok, <<"{}">>}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    {State, Req, State}.
