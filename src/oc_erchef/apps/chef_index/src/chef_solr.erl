%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author John Keiser <jkeiser@opscode.com>
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
         %% Static Data Accessors
         search_provider/0,
         id_field/0,
         database_field/0,
         kv_sep/0,
         type_field/0,
         update_url/0,
         %% Query Building
         add_org_guid_to_query/2,
         make_query_from_params/4,
         %% Document Buidling
         transform_data/1,
         %% Operations
         search/1,
         update/1,
         commit/0,
         ping/0,
         delete_search_db/1,
         delete_search_db_by_type/2,
         %% Helpers used by providers
         db_from_orgid/1
        ]).

-include("chef_solr.hrl").
-define(PROVIDER_FUNC(FuncName),
        begin
            Mod = list_to_atom(atom_to_list(search_provider()) ++ "_provider"),
            Mod:FuncName()
        end).

-define(PROVIDER_FUNC(FuncName, Args),
        begin
            Mod = list_to_atom(atom_to_list(search_provider()) ++ "_provider"),
            apply(Mod, FuncName, Args)
        end).

search_provider() ->
    envy:get(chef_index, search_provider, solr, envy:one_of([solr, cloudsearch])).

-spec id_field() -> binary().
id_field() ->
    ?PROVIDER_FUNC(id_field).

-spec database_field() -> binary().
database_field() ->
    ?PROVIDER_FUNC(database_field).

-spec type_field() -> binary().
type_field() ->
    ?PROVIDER_FUNC(type_field).

-spec ping_url() -> string().
ping_url() ->
    ?PROVIDER_FUNC(ping_url).

-spec update_url() -> string().
update_url() ->
    ?PROVIDER_FUNC(update_url).

kv_sep() ->
    ?PROVIDER_FUNC(kv_sep).

%% Query Building functions
%%
%% Callers of search can use make_query_from_params/4 and add_org_guid_to_query/2 to
%% construct a chef_solr_query object suitable for consumption by search/1.
%%
%% Query constructions is split into 2 parts since we typically want to validate the
%% user-provided search query before we have the OrgId in the webmachine module that calls
%% this function (chef_wm_search in the oc_chef_wm app).
-spec make_query_from_params(binary()|string(),
                             string() | binary() | undefined,
                             string(),
                             string()) -> #chef_solr_query{}.
make_query_from_params(ObjType, QueryString, Start, Rows) ->
    % TODO: super awesome error messages
    FilterQuery = make_fq_type(ObjType),
    %% 'sort' param is ignored and hardcoded because indexing
    %% scheme doesn't support sorting since there is only one field.
    Sort = binary_to_list(id_field()) ++ " asc",
    #chef_solr_query{query_string = check_query(QueryString),
                     filter_query = FilterQuery,
                     search_provider = search_provider(),
                     start = decode({nonneg_int, "start"}, Start, 0),
                     rows = decode({nonneg_int, "rows"}, Rows, 1000),
                     sort = Sort,
                     index = index_type(ObjType)}.

-spec add_org_guid_to_query(#chef_solr_query{}, binary())
                           -> #chef_solr_query{}.
add_org_guid_to_query(Query = #chef_solr_query{filter_query = FilterQuery},
                      OrgGuid) ->
    Query#chef_solr_query{filter_query = ?PROVIDER_FUNC(add_org_guid_to_fq,
                                                        [OrgGuid, FilterQuery])}.


%% Document Building Helpers
%% These helpers are called by chef_index_expand to build
%% XML documents appropriate for the given provider.
transform_data(Data) ->
    ?PROVIDER_FUNC(transform_data, [Data]).

%% Search/Solr Operations
%%
%%
-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query) ->
    %% FIXME: error handling
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

-spec handle_successful_search(binary() | string()) -> {ok, non_neg_integer(), non_neg_integer(), [binary()]}.
handle_successful_search(Body) ->
    SolrData = jiffy:decode(Body),
    ?PROVIDER_FUNC(handle_successful_search, [SolrData]).

-spec ping() -> pong | pang.
ping() ->
    try
        %% FIXME: solr will barf on doubled '/'s so SolrUrl must not end with a trailing slash
        case chef_index_http:request(ping_url(), get, []) of
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
    ?PROVIDER_FUNC(delete_search_db, [OrgId]).

%% @doc Delete all search index entries for a given
%% organization and type.  Types are generally binaries or strings elsewhere in this
%% module. We should think about converting the other APIs in this file to use atoms
%% instead.
%% @end
-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type) ->
    ?PROVIDER_FUNC(delete_search_db_by_type, [OrgId, Type]).

%% @doc Sends `Body` to the Solr server's "/update" endpoint.
%% @end
%%
%% Body is really a string(), but Dialyzer can only determine it is a list of bytes due to
%% the implementation of search_db_from_orgid/1
-spec update(Body :: [byte(),...]) -> ok | {error, term()}.
update(Body) when is_list(Body) ->
    update(iolist_to_binary(Body));
update(Body) ->
    try
        %% FIXME: solr will barf on doubled '/'s so SolrUrl must not end with a trailing slash
        case chef_index_http:request(update_url(), post, Body) of
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
-spec commit() -> ok | {error, term()}.
commit() ->
    ?PROVIDER_FUNC(commit).

%% Helpers

%% @doc Generates the name of the organization's search database from its ID
%% @end
%%
%% Note: this really returns a string(), but Dialyzer is convinced it's a byte list (which
%% it is, technically).  In order for it to be recognized as a printable string, though,
%% we'd have to use io_lib:format
db_from_orgid(OrgId) ->
    "chef_" ++ binary_to_list(OrgId).


%% Internal Functions

%% Construct a solr query URL
%% Calls assert_org_id_filter and search_url_fmt from provider
%% The search_url_fmt should accept all standard search args.
-spec make_solr_query_url(#chef_solr_query{}) -> string().
make_solr_query_url(Query = #chef_solr_query{filter_query = FilterQuery}) ->
    %% ensure we filter on an org ID
    ?PROVIDER_FUNC(assert_org_id_filter, [FilterQuery]),
    Url = ?PROVIDER_FUNC(search_url_fmt),
    Args = search_url_args(Query),
    lists:flatten(io_lib:format(Url, Args)).

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

make_fq_type(ObjType) when is_binary(ObjType) ->
    make_fq_type(binary_to_list(ObjType));
make_fq_type(ObjType) when ObjType =:= "node";
                           ObjType =:= "role";
                           ObjType =:= "client";
                           ObjType =:= "environment" ->
    ?PROVIDER_FUNC(make_standard_fq, [ObjType]);
make_fq_type(ObjType) ->
    ?PROVIDER_FUNC(make_data_bag_fq, [ObjType]).

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
            binary_to_list(?PROVIDER_FUNC(transform_query, [Query]));
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
