%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
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


-module(chef_wm_search_index).

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
         to_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #search_state{}}.

request_type() ->
    "search".

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

validate_request('GET', Req, State) ->
    {Req, State}.

auth_info(Req, State) ->
    {authorized, Req, State}.

%% @doc Create a list of the various search indexes that the given organization has access
%% to.  This includes the four standard indexes (client, environment, node, and role), as
%% well as an index for each of the organization's data bags.
-spec generate_index_list(DBContext::tuple(), OrgId::binary()) -> IndexNames::list(binary()).
generate_index_list(DBContext, OrgId) ->
    %% The order of these lists doesn't really matter, given that they are ultimately
    %% destined to be keys in a map structure.  In any event, the web UI currently orders
    %% them alphabetically, regardless of whether they are data bag indexes or "standard"
    %% Chef indexes.  Since ++ copies the LHS list, though, we'll put the fixed list of four
    %% items on the left to avoid copying a potentially long list of data bags.
    [<<"client">>, <<"environment">>, <<"node">>, <<"role">>]
        ++ chef_db:list(#chef_data_bag{org_id = OrgId}, DBContext).

%% @doc Return a proplist of `{IndexName, URL}' pairs, denoting all the search indexes an
%% organization has access to.  The host, port, and organization name are all derived from
%% the Webmachine request.
%%
%% Example:
%%
%%  index_map(["nodes", "clients"], Req) =>
%%     [{"nodes", "http://server.com/search/nodes"},
%%      {"clients", "http://server.com/search/clients"}]
%%
-spec index_map(SearchIndexes::list(binary()), Req::tuple()) ->
                       list({IndexName::binary(), URL::binary()}).
index_map(SearchIndexes, Req) ->
    [{Index, oc_chef_wm_routes:route(organization_search, Req, [{search_index, Index}])}
     || Index <- SearchIndexes].

to_json(Req, #base_state{chef_db_context = DBContext,
                         organization_guid = OrgId} = State) ->
    %% Just listing the search indexes; no searching necessary
    SearchIndexes = generate_index_list(DBContext, OrgId),
    IndexMap = index_map(SearchIndexes, Req),
    %% IndexMap is a proplist, need to wrap in a tuple for ejson
    Json = chef_json:encode({IndexMap}),
    {Json, Req, State}.

%% This should never get called; if it does, something is very wrong.
%% Should it get called, we should revisit malformed_request to
%% investigate having an arity-2 form (i.e., that doesn't require a
%% message function).
malformed_request_message(Any, _Req, _State) ->
    error({unexpected_malformed_request_message, Any}).
