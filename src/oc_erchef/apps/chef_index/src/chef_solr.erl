%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@chef.io>
%% @doc Helper module for calling various Chef REST endpoints
%% @end
%%
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_solr).

-export([
         %% Operations
         search/1,
         update/1,
         commit/0,
         ping/0,
         delete_search_db/1,
         delete_search_db_by_type/2,
         %% Document Buidling
         transform_data/1]).

-include("chef_solr.hrl").
-define(PING_URL, "/admin/ping?wt=json").
-define(UPDATE_URL, "/update").

%% Document Building Helpers
%% These helpers are called by chef_index_expand to build
%% XML documents appropriate for the given provider.
transform_data(Data) ->
    xml_text_escape(Data).

%% @doc Given a binary or list of binaries, replace occurances of `<',
%% `&', `"', and `>' with the corresponding entity code such that the
%% resulting binary or list of binaries is suitable for inclusion as
%% text in an XML element.
%%
%% We cheat and simply process the binaries byte at a time. This
%% should be OK for UTF-8 binaries, but relies on multi-byte
%% characters not starting with the same value as those we are
%% searching for to escape.  Note that technically we don't need to
%% escape `>' nor `"', symmetry and matching of a pre-existing Ruby
%% implementation suggest otherwise.
-spec xml_text_escape(binary()|[binary()]) -> binary()|[binary()].
xml_text_escape(BinStr) ->
    iolist_to_binary(xml_text_escape1(BinStr)).

xml_text_escape1(BinStr) when is_binary(BinStr) ->
    efast_xs:escape(BinStr);

xml_text_escape1(BinList) when is_list(BinList) ->
    [ xml_text_escape1(B) || B <- BinList ].

%% Search/Solr Operations
-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query) ->
    Url = make_solr_query_url(Query),
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, []),
    case Code of
        "200" ->
            handle_successful_search(Body);
        %% We only have the transformed query at this point, so for the following two error
        %% conditions, we just send along the full query URL. This is for logging only and
        %% should NOT be sent back to the client. Note that a 400 from solr can occur when
        %% the query is bad or when something that ends up in the filter query parameter is
        %% bad, for example, an index with special characters.
        "400" ->
            {error, {solr_400, Url}};
        "500" ->
            {error, {solr_500, Url}}
    end.

make_solr_query_url(Query = #chef_solr_query{filter_query = FilterQuery}) ->
    %% ensure we filter on an org ID
    chef_index_query:assert_org_id_filter(FilterQuery),
    Url = search_url_fmt(),
    Args = search_url_args(Query),
    lists:flatten(io_lib:format(Url, Args)).

search_url_fmt() ->
    "/select?"
        "fq=~s"
        "&indent=off"
        "&q=~s"
        "&start=~B"
        "&rows=~B"
        "&wt=json"
        "&sort=~s".

search_url_args(#chef_solr_query{
                   query_string = Query,
                   filter_query = FilterQuery,
                   start = Start,
                   rows = Rows,
                   sort = Sort}) ->
    [ibrowse_lib:url_encode(FilterQuery),
     ibrowse_lib:url_encode(Query),
     Start, Rows,
     ibrowse_lib:url_encode(Sort)].

-spec handle_successful_search(binary() | string()) -> {ok, non_neg_integer(), non_neg_integer(), [binary()]}.
handle_successful_search(ResponseBody) ->
    Response = ej:get({<<"response">>}, jiffy:decode(ResponseBody)),
    Start    = ej:get({<<"start">>}, Response),
    NumFound = ej:get({<<"numFound">>}, Response),
    DocList  = ej:get({<<"docs">>}, Response),
    Ids = [ ej:get({<<"X_CHEF_id_CHEF_X">>}, Doc) || Doc <- DocList ],
    {ok, Start, NumFound, Ids}.

-spec ping() -> pong | pang.
ping() ->
    case chef_index_http:get(?PING_URL) of
        ok -> pong;
        _Error -> pang
    end.

%% TODO: Deal properly with errors
%% @doc Delete all search index entries for a given organization.
-spec delete_search_db(OrgId :: binary()) -> ok.
delete_search_db(OrgId) ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        chef_index_query:search_db_from_orgid(OrgId) ++
        "</query></delete>",
    ok = update(DeleteQuery),
    ok = commit(),
    ok.

%% @doc Delete all search index entries for a given
%% organization and type.  Types are generally binaries or strings elsewhere in this
%% module. We should think about converting the other APIs in this file to use atoms
%% instead.
%% @end
-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        chef_index_query:search_db_from_orgid(OrgId) ++ " AND " ++
        chef_index_query:search_type_constraint(Type) ++
        "</query></delete>",
    update(DeleteQuery).

%% @doc Sends `Body` to the Solr server's "/update" endpoint.
%% @end
%%
%% Body is really a string(), but Dialyzer can only determine it is a list of bytes due to
%% the implementation of search_db_from_orgid/1
%% For that matter, `update` is really 'post to provider'.
-spec update(iolist() | binary()) -> ok | {error, term()}.
update(Body) when is_list(Body) ->
    update(iolist_to_binary(Body));
update(Body) ->
    chef_index_http:post(?UPDATE_URL, Body).

%% @doc Sends a "commit" message directly to Solr
%% This is exposed for the users of delete_search_db_by_type
-spec commit() -> ok | {error, term()}.
commit() ->
    update("<?xml version='1.0' encoding='UTF-8'?><commit/>").
