%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author John Keiser <jkeiser@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @doc Helper module for calling various Chef REST endpoints
% @end
-module(chef_solr).

-export([search/2, make_query_from_params/1, add_org_guid_to_query/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(chef_solr_query, {query_string :: string(),
                          filter_query :: string(),
			  start :: integer(),
                          rows :: integer(),
                          sort :: string()}).

make_query_from_params(Req) ->
    % TODO: super awesome error messages
    % TODO: verify that FilterQuery, Start, Rows and Sort have correct values
    ObjType = wrq:path_info(object_type, Req),
    QueryString = transform_query(http_uri:decode(wrq:get_qs_value("q", Req))),
    FilterQuery = make_fq_type(ObjType),
    {Start, _Any} = string:to_integer(http_uri:decode(wrq:get_qs_value("start", Req))),
    {Rows, _Any} = string:to_integer(http_uri:decode(wrq:get_qs_value("rows", Req))),
    Sort = http_uri:decode(wrq:get_qs_value("sort", Req)),
    #chef_solr_query{query_string = QueryString,
                     filter_query = FilterQuery,
                     start = Start,
                     rows = Rows,
                     sort = Sort}.

add_org_guid_to_query(Query = #chef_solr_query{filter_query = FilterQuery}, OrgGuid) ->
    Query#chef_solr_query{filter_query = "+X_CHEF_database_CHEF_X:chef_" ++ binary_to_list(OrgGuid) ++ " " ++ FilterQuery}.

search(SolrUrl, Query) ->
    Url = SolrUrl ++ make_solr_query_url(Query),
    % FIXME: error handling
    {ok, _Code, _Head, Body} = ibrowse:send_req(Url, [], get),
    SolrData = ejson:decode(Body),
    DocList = ej:get({<<"response">>, <<"docs">>}, SolrData),
    [ ej:get({<<"X_CHEF_id_CHEF_X">>}, Doc) || Doc <- DocList ].

transform_query(RawQuery) when is_list(RawQuery) ->
    transform_query(list_to_binary(RawQuery));
transform_query(RawQuery) ->
    case chef_lucene:parse(RawQuery) of
        Query when is_binary(Query) ->
            binary_to_list(Query);
        _ ->
            throw({bad_query, RawQuery})
    end.


%% Internal functions

% /solr/select?
    % fq=%2BX_CHEF_type_CHEF_X%3Anode+%2BX_CHEF_database_CHEF_X%3Achef_288da1c090ff45c987346d2829257256
    % &indent=off
    % &q=content%3Aattr1__%3D__v%2A

make_solr_query_url(#chef_solr_query{query_string = Query, filter_query = FilterQuery, start = Start, rows = Rows, sort = Sort}) ->
    Url = "/select?"
        "fq=~s"
        "&indent=off"
        "&q=~s"
        "&start=~i"
        "&rows=~i"
        "&wt=json"
        "&sort=~s",
    io_lib:format(Url, [ ibrowse_lib:url_encode(FilterQuery), ibrowse_lib:url_encode(Query), Start, Rows, ibrowse_lib:url_encode(Sort)] ).

make_fq_type(ObjType) when ObjType =:= "node";
                           ObjType =:= "role";
                           ObjType =:= "client";
                           ObjType =:= "environment" ->
    "+X_CHEF_type_CHEF_X:" ++ ObjType;
make_fq_type(ObjType) ->
    "+X_CHEF_type_CHEF:data_bag +data_bag:" ++ ObjType.
