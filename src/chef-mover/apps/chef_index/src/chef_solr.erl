%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@chef.io>
%% @doc Helper module for calling various Chef REST endpoints
%% @end
%%
%% Copyright 2011-2018 Chef Software, Inc.
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
         add_org_guid_to_query/2,
         delete_search_db/1,
         delete_search_db_by_type/2,
         make_query_from_params/4,
         ping/0,
         search/1,
         search/2,
         solr_commit/0
        ]).

-include("chef_solr.hrl").

-spec make_query_from_params(binary()|string(),
                             string() | binary() | undefined,
                             string(),
                             string()) -> #chef_solr_query{}.
make_query_from_params(ObjType, QueryString, Start, Rows) ->
    % TODO: super awesome error messages
    FilterQuery = make_fq_type(ObjType),
    %% 'sort' param is ignored and hardcoded because indexing
    %% scheme doesn't support sorting since there is only one field.
    Sort = "X_CHEF_id_CHEF_X asc",
    #chef_solr_query{query_string = check_query(QueryString),
                     filter_query = FilterQuery,
                     start = decode({nonneg_int, "start"}, Start, 0),
                     rows = decode({nonneg_int, "rows"}, Rows, 1000),
                     sort = Sort,
                     index = index_type(ObjType)}.

-spec add_org_guid_to_query(#chef_solr_query{}, binary()) ->
                                   #chef_solr_query{}.
add_org_guid_to_query(Query = #chef_solr_query{filter_query = FilterQuery},
                      OrgGuid) ->
    Query#chef_solr_query{filter_query = "+" ++
                              search_db_from_orgid(OrgGuid) ++
                              " " ++ FilterQuery}.

-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(Query) ->
    SolrUrl = envy:get(chef_index, solr_url, string),
    search(Query, SolrUrl).


-spec search(#chef_solr_query{}, string()) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query, SolrUrl) ->
    %% FIXME: error handling
    Url = SolrUrl ++ make_solr_query_url(Query),
    {ok, Code, _Head, Body} = ibrowse:send_req(Url, [], get),
    case Code of
        "200" ->
            SolrData = jiffy:decode(Body),
            Response = ej:get({<<"response">>}, SolrData),
            Start = ej:get({<<"start">>}, Response),
            NumFound = ej:get({<<"numFound">>}, Response),
            DocList = ej:get({<<"docs">>}, Response),
            Ids = [ ej:get({<<"X_CHEF_id_CHEF_X">>}, Doc) || Doc <- DocList ],
            {ok, Start, NumFound, Ids};
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

-spec ping() -> pong | pang.
ping() ->
    try
        SolrUrl = envy:get(chef_index, solr_url, string),
        %% FIXME: solr will barf on doubled '/'s so SolrUrl must not end with a trailing slash
        Url = SolrUrl ++ "/admin/ping?wt=json",
        case ibrowse:send_req(Url, [], get) of
            %% FIXME: verify that solr returns non-200 if something is wrong and not "status":"ERROR".
            {ok, "200", _Head, _Body} -> pong;
            _Error -> pang
        end
    catch
        How:Why ->
            error_logger:error_report({chef_solr, ping, How, Why}),
            pang
    end.

%% TODO: Deal properly with errors
%% @doc Delete all search index entries for a given organization.
-spec delete_search_db(OrgId :: binary()) -> ok.
delete_search_db(OrgId) ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        search_db_from_orgid(OrgId) ++
        "</query></delete>",
    ok = solr_update(DeleteQuery),
    ok = solr_commit(),
    ok.

%% @doc Delete all search index entries for a given
%% organization and type.  Types are generally binaries or strings elsewhere in this
%% module. We should think about converting the other APIs in this file to use atoms
%% instead.
%% Note: This omits solr_commit because of the high cost of that call in production.
%% Some users will want to call the commit directly.
%% @end
-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        search_db_from_orgid(OrgId) ++ " AND " ++
        search_type_constraint(Type) ++
        "</query></delete>",
    solr_update(DeleteQuery).


%% Internal functions

%% @doc Generates the name of the organization's search database from its ID
%% @end
%%
%% Note: this really returns a string(), but Dialyzer is convinced it's a byte list (which
%% it is, technically).  In order for it to be recognized as a printable string, though,
%% we'd have to use io_lib:format
-spec search_db_from_orgid(OrgId :: binary()) -> DBName :: [byte(),...].
search_db_from_orgid(OrgId) ->
    "X_CHEF_database_CHEF_X:chef_" ++ binary_to_list(OrgId).

%% @doc Generates a constraint for chef_type
%% @end
-spec search_type_constraint(Type :: atom()) -> TypeConstraint :: [byte(),...].
search_type_constraint(Type) ->
    "X_CHEF_type_CHEF_X:" ++ atom_to_list(Type).


% /solr/select?
    % fq=%2BX_CHEF_type_CHEF_X%3Anode+%2BX_CHEF_database_CHEF_X%3Achef_288da1c090ff45c987346d2829257256
    % &indent=off
    % &q=content%3Aattr1__%3D__v%2A
-spec make_solr_query_url(#chef_solr_query{}) -> string().
make_solr_query_url(#chef_solr_query{
                       query_string = Query,
                       %% ensure we filter on an org ID
                       filter_query = FilterQuery = "+X_CHEF_database_CHEF_X:chef_" ++ _Rest,
                       start = Start,
                       rows = Rows,
                       sort = Sort}) ->
    Url = "/select?"
        "fq=~s"
        "&indent=off"
        "&q=~s"
        "&start=~B"
        "&rows=~B"
        "&wt=json"
        "&sort=~s",
    lists:flatten(io_lib:format(Url, [ibrowse_lib:url_encode(FilterQuery),
                                      ibrowse_lib:url_encode(Query),
                                      Start, Rows,
                                      ibrowse_lib:url_encode(Sort)])).

make_fq_type(ObjType) when is_binary(ObjType) ->
    make_fq_type(binary_to_list(ObjType));
make_fq_type(ObjType) when ObjType =:= "node";
                           ObjType =:= "role";
                           ObjType =:= "client";
                           ObjType =:= "environment" ->
    "+X_CHEF_type_CHEF_X:" ++ ObjType;
make_fq_type(ObjType) ->
    "+X_CHEF_type_CHEF_X:data_bag_item +data_bag:" ++ ObjType.

index_type(Type) when is_binary(Type) ->
    index_type(binary_to_list(Type));
index_type("node") ->
    'node';
index_type("role") ->
    'role';
index_type("client") ->
    'client';
index_type("environment") ->
    'environment';
index_type(DataBag) ->
    {'data_bag', list_to_binary(DataBag)}.

check_query(RawQuery) ->
    case RawQuery of
        undefined ->
            %% Default query string if no 'q' param is present. We might
            %% change this to be a 400 in the future.
            "*:*";
        "" ->
            %% thou shalt not query with the empty string
            throw({bad_query, ""});
        Query ->
            transform_query(http_uri:decode(Query))
    end.

transform_query(RawQuery) when is_list(RawQuery) ->
    transform_query(list_to_binary(RawQuery));
transform_query(RawQuery) ->
    case chef_lucene:parse(RawQuery) of
        Query when is_binary(Query) ->
            binary_to_list(Query);
        _ ->
            throw({bad_query, RawQuery})
    end.

decode({nonneg_int, Key}, Val, Default) ->
    {Int, Orig} =
        case Val of
            undefined ->
                {Default, default};
            Value ->
                try
                    {list_to_integer(http_uri:decode(Value)), Value}
                catch
                    error:badarg ->
                        throw({bad_param, {Key, Value}})
                end
        end,
    validate_non_neg(Key, Int, Orig).

validate_non_neg(Key, Int, OrigValue) when Int < 0 ->
    throw({bad_param, {Key, OrigValue}});
validate_non_neg(_Key, Int, _OrigValue) ->
    Int.

%%------------------------------------------------------------------------------
%% Direct Solr Server Interaction
%%
%% To drop all entries for a given org's search "database", we need to bypass the indexer
%% queue and interact directly with the Solr server.  These functions facilitate that.
%%------------------------------------------------------------------------------

%% @doc Sends `Body` to the Solr server's "/update" endpoint.
%% @end
%%
%% Body is really a string(), but Dialyzer can only determine it is a list of bytes due to
%% the implementation of search_db_from_orgid/1
-spec solr_update(Body :: [byte(),...]) -> ok | {error, term()}.
solr_update(Body) ->
    try
        SolrUrl = envy:get(chef_index, solr_url, string),
        %% FIXME: solr will barf on doubled '/'s so SolrUrl must not end with a trailing slash
        Url = SolrUrl ++ "/update",
        Headers = [{"Content-Type", "text/xml"}],
        case ibrowse:send_req(Url, Headers, post, Body) of
            %% FIXME: verify that solr returns non-200 if something is wrong and not "status":"ERROR".
            {ok, "200", _Head, _Body} -> ok;
            Error -> {error, Error}
        end
    catch
        How:Why ->
            error_logger:error_report({chef_solr, update, How, Why}),
            {error, Why}
    end.

%% @doc Sends a "commit" message directly to Solr
%% This is exposed for the users of delete_search_db_by_type
-spec solr_commit() -> ok | {error, term()}.
solr_commit() ->
    solr_update("<?xml version='1.0' encoding='UTF-8'?><commit/>").
