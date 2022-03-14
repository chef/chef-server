-module(chef_opensearch).
-export([
         %% Solr Operations
         search/1,
         update/1,
         commit/0,
         ping/0,
         delete_search_db/1,
         delete_search_db_by_type/2,
         %% Document Building Helpers
         transform_data/1,
         declare_metrics/0
        ]).

-include("chef_solr.hrl").
-define(JSON_HEADER, chef_index_http:get_headers()).

-spec ping() -> pong | pang.
ping() ->
    case chef_index_http:get("/chef", [], ?JSON_HEADER) of
        ok -> pong;
        _Error -> pang
    end.

declare_metrics() ->
    prometheus_counter:declare([{name, chef_opensearch_update_count},
                                {help, "Total calls to chef_opensearch:update"}]),
    prometheus_counter:declare([{name, chef_opensearch_search_count},
                                {help, "Total calls to chef_opensearch:search"}]),
    prometheus_counter:declare([{name, chef_opensearch_delete_search_db_count},
                                {help, "Total calls to chef_opensearch:delete_es_search_db"}]),
    prometheus_counter:declare([{name, chef_opensearch_delete_search_db_by_type_count},
                                {help, "Total calls to chef_opensearch:delete_search_db_by_type"}]),

    %% TODO(ssd) 2020-05-15: Right now we don't actually check the
    %% responses for these calls.  We should fix this but I'm trying
    %% to limit the number of logic changes introduced by adding the
    %% metrics.
    %% prometheus_counter:declare([{name, chef_opensearch_update_resp_count},
    %%                             {help, "Calls to chef_opensearch:update by response code"},
    %%                             {labels, [resp_code]}]),
    %% prometheus_counter:declare([{name, chef_opensearch_delete_search_db_resp_count},
    %%                             {help, "Calls to chef_opensearch:delete_es_search_db by response code"},
    %%                             {labels, [resp_code]}]),
    %% prometheus_counter:declare([{name, chef_opensearch_delete_search_db_by_type_resp_count},
    %%                             {help, "Calls to chef_opensearch:delete_search_db_by_type by response code"},
    %%                             {labels, [resp_code]}]),
    prometheus_counter:declare([{name, chef_opensearch_search_resp_count},
                                {help, "Calls to chef_opensearch:search by response code"},
                                {labels, [resp_code]}]).

-spec update(iolist() | binary()) -> ok | {error, term()}.
update(Body) when is_list(Body) ->
    update(iolist_to_binary(Body));
update(Body) ->
    prometheus_counter:inc(chef_opensearch_update_count),
    chef_index_http:post("/_bulk", Body, ?JSON_HEADER).

-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query) ->
    Url = "/chef/_search",
    prometheus_counter:inc(chef_opensearch_search_count),
    %% NOTE(ssd,praj) 2020-05-15: We held off on adding a histogram
    %% here since we have a histogram a layer lower in
    %% chef_index_http.
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, query_body(Query), ?JSON_HEADER),
    prometheus_counter:inc(chef_opensearch_search_resp_count, [Code]),
    %% TODO(ssd) 2020-05-15: I'm surprised we don't get case_clause
    %% errors here regularly as we only handle 3 HTTP status codes.
    case Code of
        "200" ->
            handle_successful_search(Body);
        %% For now keep these error messages
        %% consistent with chef_solr
        "400" ->
            {error, {solr_400, Url}};
        "500" ->
            {error, {solr_500, Url}}
    end.

handle_successful_search(ResponseBody) ->
    Response = ej:get({<<"hits">>}, jiffy:decode(ResponseBody)),
    NumFound = case ej:get({<<"total">>}, Response) of
                    Total when is_tuple(Total) ->
                        ej:get({<<"value">>}, Total);
                    Else ->
                        Else
               end,
    DocList  = ej:get({<<"hits">>}, Response),
    Ids = [ ej:get({<<"_id">>}, Doc) || Doc <- DocList ],
    {ok, undefined, NumFound, Ids}.

transform_data(Data) ->
    Data.

commit() ->
    chef_index_http:post("/_refresh", [], ?JSON_HEADER).

query_body(#chef_solr_query{
              query_string = Query,
              filter_query = undefined,
              start = Start,
              rows = Rows}) ->
    jiffy:encode({[{fields_tag(), <<"_id">>},
                   {<<"from">>, Start},
                   {<<"size">>, Rows},
                   {<<"query">>, {[query_string_query_ejson(Query)]}}
                  ]});
query_body(#chef_solr_query{
              query_string = Query,
              filter_query = FilterQuery,
              start = Start,
              rows = Rows}) ->
    chef_index_query:assert_org_id_filter(FilterQuery),
    jiffy:encode({[{ fields_tag(), <<"_id">>},
        {<<"from">>, Start},
        {<<"size">>, Rows},
        {<<"sort">>, [{[{<<"X_CHEF_id_CHEF_X">>, {[{<<"order">>, <<"asc">>}]}}]}]},
        {<<"query">>, {[
            {<<"bool">>,{[
                {<<"must">>, {[query_string_query_ejson(Query)]}},
                {<<"filter">>, {[query_string_query_ejson(FilterQuery)]}}
            ]}}]}
        }]}).

fields_tag() ->
    <<"stored_fields">>.

query_string_query_ejson(QueryString) ->
    {<<"query_string">>,{[{<<"query">>, list_to_binary(QueryString)}]}}.

%% A note on deleting
%%
%% The delete-by-query API was removed from opensearch in 2.0, and is only
%% available as a plugin. As a result, we can't rely on it being there in external
%% opensearch instances we don't control. Rather, we replicate the delete-by-query
%% by performing a search, iterating over the results, and then deleting the
%% documents individually.
%%
%% Because the number of documents is unknown, we use the Scroll API, which
%% allows us to return all the results without having to set size to some ungodly
%% high number. While this does increase network traffic to/from opensearch, it
%% reduces the memory load. If network performance becomes a problem, we could
%% increase the number of rows we return with each query.
%%
%% opensearch 5.0 has added back delete-by-query. Therefore if the version is
%% 5.0 or greater we are using this more efficient request.
-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    QueryString = chef_index_query:search_db_from_orgid(OrgId) ++
        "AND" ++ chef_index_query:search_type_constraint(Type),
    prometheus_counter:inc(chef_opensearch_delete_search_db_by_type_count),
    chef_index_http:post("/chef/_delete_by_query", delete_query_body(QueryString), ?JSON_HEADER),
    commit(),
    ok.

-spec delete_search_db(OrgId :: binary()) -> ok.
delete_search_db(OrgId) ->
    QueryString = chef_index_query:search_db_from_orgid(OrgId),
    prometheus_counter:inc(chef_opensearch_delete_search_db_count),
    chef_index_http:post("/chef/_delete_by_query", delete_query_body(QueryString), ?JSON_HEADER),
    commit(),
    ok.

delete_query_body(QueryString) ->
    jiffy:encode({[
                   {<<"query">>, {[query_string_query_ejson(QueryString)]}}
                  ]}).
