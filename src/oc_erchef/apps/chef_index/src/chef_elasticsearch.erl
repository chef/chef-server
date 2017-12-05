-module(chef_elasticsearch).
-export([
         %% Solr Operations
         search/1,
         update/1,
         commit/0,
         ping/0,
         delete_search_db/1,
         delete_search_db_by_type/2,
         %% Document Building Helpers
         transform_data/1
        ]).

-include("chef_solr.hrl").
-define(JSON_HEADER, [{"Content-Type", "application/json"}]).

-spec ping() -> pong | pang.
ping() ->
    case chef_index_http:get("/chef", [], ?JSON_HEADER) of
        ok -> pong;
        _Error -> pang
    end.

-spec update(iolist() | binary()) -> ok | {error, term()}.
update(Body) when is_list(Body) ->
    update(iolist_to_binary(Body));
update(Body) ->
    chef_index_http:post("/_bulk", Body, ?JSON_HEADER).

-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query) ->
    Url = "/chef/_search",
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, query_body(Query), ?JSON_HEADER),
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
    NumFound = ej:get({<<"total">>}, Response),
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
    case envy:get(chef_index, solr_elasticsearch_major_version, 2, non_neg_integer) of
        5 -> <<"stored_fields">>;
        _ -> <<"fields">>
    end.

query_string_query_ejson(QueryString) ->
    {<<"query_string">>,{[
        {<<"lowercase_expanded_terms">>, false},
        {<<"analyzer">>, "keyword"},
        {<<"query">>, list_to_binary(QueryString)}
    ]}}.

%%
%% A note on deleting
%%
%% The delete-by-query API was removed from Elasticsearch in 2.0, and is only
%% available as a plugin. As a result, we can't rely on it being there in external
%% Elasticsearch instances we don't control. Rather, we replicate the delete-by-query
%% by performing a search, iterating over the results, and then deleting the
%% documents individually.
%%
%% Because the number of documents is unknown, we use the Scroll API, which
%% allows us to return all the results without having to set size to some ungodly
%% high number. While this does increase network traffic to/from Elasticsearch, it
%% reduces the memory load. If network performance becomes a problem, we could
%% increase the number of rows we return with each query.
%%
%% Elasticsearch 5.0 has added back delete-by-query. Therefore if the version is
%% 5.0 or greater we are using this more efficient request.
-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    QueryString = chef_index_query:search_db_from_orgid(OrgId) ++
                                "AND" ++ chef_index_query:search_type_constraint(Type),
    Query = #chef_solr_query{
                             rows = 1000,
                             start = 0,
                             search_provider = elasticsearch,
                             query_string = QueryString
                            },
    case envy:get(chef_index, solr_elasticsearch_major_version, 2, non_neg_integer) of
        X when X >= 5 ->
            chef_index_http:post("/chef/_delete_by_query", delete_query_body(QueryString), ?JSON_HEADER),
            commit(),
            ok;
        _ ->
            {ok, _, _, Ids} = search_with_scroll(Query),
            delete_ids(Ids)
    end.

-spec delete_search_db(OrgId :: binary()) -> ok.
delete_search_db(OrgId) ->
    QueryString = chef_index_query:search_db_from_orgid(OrgId),
    Query = #chef_solr_query{
                             rows = 1000,
                             start = 0,
                             search_provider = elasticsearch,
                             query_string = QueryString
                            },
    case envy:get(chef_index, solr_elasticsearch_major_version, 2, non_neg_integer) of
        X when X >= 5 ->
            chef_index_http:post("/chef/_delete_by_query", delete_query_body(QueryString), ?JSON_HEADER),
            commit(),
            ok;
        _ ->
            {ok, _, _, Ids} = search_with_scroll(Query),
            delete_ids(Ids)
    end.

delete_query_body(QueryString) ->
    jiffy:encode({[
                   {<<"query">>, {[query_string_query_ejson(QueryString)]}}
                  ]}).

%% Do a search query using the Elasticsearch Scroll API. We only use this when
%% doing the search used for reindexing.
-spec search_with_scroll(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search_with_scroll(#chef_solr_query{} = Query) ->
    Url = "/chef/_search?scroll=1m",
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, query_body(Query), ?JSON_HEADER),
    case Code of
        "200" ->
            EjsonBody = jiffy:decode(Body),
            ScrollId = ej:get({<<"_scroll_id">>}, EjsonBody),
            Response = ej:get({<<"hits">>}, EjsonBody),
            NumFound = ej:get({<<"total">>}, Response),
            DocList  = ej:get({<<"hits">>}, Response),
            Ids = [ ej:get({<<"_id">>}, Doc) || Doc <- DocList ],
            scroll([ScrollId], NumFound, length(Ids), Ids);
        %% For now keep these error messages
        %% consistent with chef_solr
        "400" ->
            {error, {solr_400, Url}};
        "500" ->
            {error, {solr_500, Url}}
    end.

scroll(ScrollIds, NumFound, NumFound, Ids) ->
    ok = chef_index_http:delete("/_search/scroll/", scroll_body(ScrollIds), ?JSON_HEADER),
    {ok, undefined, NumFound, Ids};
scroll([ScrollIdHead | _] = ScrollIds, NumFound, _, Ids) ->
    Url = "/_search/scroll?scroll=1m",
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, ScrollIdHead, ?JSON_HEADER),
    case Code of
        "200" ->
            DocList = ej:get({<<"hits">>, <<"hits">>}, jiffy:decode(Body)),
            NewScrollId = ej:get({<<"_scroll_id">>}, jiffy:decode(Body)),
            NewIds = [ ej:get({<<"_id">>}, Doc) || Doc <- DocList ],
            AllIds = lists:append([ Ids, NewIds ]),
            scroll([ NewScrollId | ScrollIds ], NumFound, length(AllIds), AllIds);
        %% For now keep these error messages
        %% consistent with chef_solr
        "400" ->
            {error, {solr_400, Url}};
        "500" ->
            {error, {solr_500, Url}}
    end.

scroll_body(ScrollId) ->
    jiffy:encode({[{<<"scroll_id">>, ScrollId}]}).

delete_ids([]) ->
    ok = commit(),
    ok;
delete_ids([Id | Ids]) ->
    ok = chef_index_http:delete("/chef/object/" ++ Id, [], ?JSON_HEADER),
    delete_ids(Ids).
