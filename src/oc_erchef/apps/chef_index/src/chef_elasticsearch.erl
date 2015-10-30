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

-spec ping() -> pong | pang.
ping() ->
    case chef_index_http:get("/chef") of
        ok -> pong;
        _Error -> pang
    end.

-spec update(iolist() | binary()) -> ok | {error, term()}.
update(Body) when is_list(Body) ->
    update(iolist_to_binary(Body));
update(Body) ->
    chef_index_http:post("/_bulk", Body).

-spec search(#chef_solr_query{}) ->
                    {ok, non_neg_integer(), non_neg_integer(), [binary()]} |
                    {error, {solr_400, string()}} |
                    {error, {solr_500, string()}}.
search(#chef_solr_query{} = Query) ->
    Url = "/chef/_search",
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, query_body(Query)),
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
    chef_index_http:post("/_refresh", []).

query_body(#chef_solr_query{
              query_string = Query,
              filter_query = FilterQuery,
              start = Start,
              rows = Rows}) ->
    chef_index_query:assert_org_id_filter(FilterQuery),
    jiffy:encode({[{<<"from">>, Start},
                   {<<"size">>, Rows},
                   {<<"sort">>, [{[{<<"X_CHEF_id_CHEF_X">>, {[{<<"order">>, <<"asc">>}]}}]}]},
                   {<<"query">>, {[
                                   {<<"filtered">>,{[
                                                     query_string_query_ejson(Query),
                                                     {<<"filter">>, {[query_string_query_ejson(FilterQuery)]}}
                                                    ]}}]}}]}).

query_string_query_ejson(QueryString) ->
    { <<"query">>, {
          [{<<"query_string">>,{
                [{<<"lowercase_expanded_terms">>, false},
                 {<<"query">>, list_to_binary(QueryString)}]}}]
         }
    }.

-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    DeleteQuery = jiffy:encode({[query_string_query_ejson(
                                   chef_index_query:search_db_from_orgid(OrgId) ++
                                       "AND" ++
                                       chef_index_query:search_type_constraint(Type))]}),
    ok = chef_index_http:delete("/chef/_query", DeleteQuery).

delete_search_db(OrgId) ->
    DeleteQuery = jiffy:encode({[query_string_query_ejson(
                                   chef_index_query:search_db_from_orgid(OrgId))]}),
    ok = chef_index_http:delete("/chef/_query", DeleteQuery),
    ok = commit(),
    ok.
