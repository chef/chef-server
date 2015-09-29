-module(elasticsearch_provider).
-export([%% Static Data Accessors
         database_field/0,
         id_field/0,
         ping_url/0,
         type_field/0,
         update_url/0,
         %% Query Building Helpers
         add_org_guid_to_fq/2,
         assert_org_id_filter/1,
         make_standard_fq/1,
         make_data_bag_fq/1,
         make_search_query_body/1,
         search_url_fmt/0,
          %% Document Building Helpers
         transform_data/1,
         %% Response Handling Functions
         handle_successful_search/1,
         %% Solr Operations
         commit/0,
         delete_search_db/1,
         delete_search_db_by_type/2
        ]).

-include("chef_solr.hrl").

id_field() ->
    <<"X_CHEF_id_CHEF_X">>.

database_field() ->
    <<"X_CHEF_database_CHEF_X">>.

type_field() ->
    <<"X_CHEF_type_CHEF_X">>.

ping_url() ->
    "/chef".

update_url() ->
    "/_bulk".

search_url_fmt() ->
    "/chef/_search".

handle_successful_search(ResponseBody) ->
    Response = ej:get({<<"hits">>}, ResponseBody),
    NumFound = ej:get({<<"total">>}, Response),
    DocList  = ej:get({<<"hits">>}, Response),
    Ids = [ ej:get({<<"_id">>}, Doc) || Doc <- DocList ],
    {ok, undefined, NumFound, Ids}.

make_standard_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":" ++ ObjType.

make_data_bag_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":data_bag_item +data_bag:" ++ ObjType.

add_org_guid_to_fq(OrgGuid, FilterQuery) ->
    "+" ++ search_db_from_orgid(OrgGuid) ++ " " ++ FilterQuery.

transform_data(Data) ->
    Data.

assert_org_id_filter(FieldQuery) ->
    Start = "+" ++ binary_to_list(database_field()) ++ ":chef_",
    Len = length(Start),
    Start = string:substr(FieldQuery, 1, Len).

commit() ->
    post("/_refresh", []).

-spec post(list(), iolist() | binary()) -> ok | {error, term()}.
post(Url, Body) when is_list(Body) ->
    post(Url, iolist_to_binary(Body));
post(Url, Body) ->
    request_with_caught_errors(Url, post, Body).

-spec delete(list(), iolist() | binary()) -> ok | {error, term()}.
delete(Url, Body) when is_list(Body) ->
    post(Url, iolist_to_binary(Body));
delete(Url, Body) ->
    request_with_caught_errors(Url, delete, Body).


request_with_caught_errors(Url, Method, Body) ->
    try
        case chef_index_http:request(Url, Method, Body) of
            {ok, "200", _Head, _Body} -> ok;
            Error -> {error, Error}
        end
    catch
        How:Why ->
            error_logger:error_report({elasticsearch_provider, Method, How, Why}),
            {error, Why}
    end.

make_search_query_body(#chef_solr_query{
                          query_string = Query,
                          filter_query = FilterQuery,
                          start = Start,
                          rows = Rows}) ->
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
    DeleteQuery = jiffy:encode({[query_string_query_ejson(search_db_from_orgid(OrgId) ++ "AND" ++ search_type_constraint(Type))]}),
    ok = delete("/chef/_query", DeleteQuery).

delete_search_db(OrgId) ->
    DeleteQuery = jiffy:encode({[query_string_query_ejson(search_db_from_orgid(OrgId))]}),
    ok = delete("/chef/_query", DeleteQuery),
    ok = commit(),
    ok.

-spec search_db_from_orgid(OrgId :: binary()) -> DBName :: [byte(),...].
search_db_from_orgid(OrgId) ->
    binary_to_list(database_field()) ++ ":" ++ chef_solr:db_from_orgid(OrgId).

%% @doc Generates a constraint for chef_type
%% @end
-spec search_type_constraint(Type :: atom()) -> TypeConstraint :: [byte(),...].
search_type_constraint(Type) ->
    binary_to_list(type_field()) ++ atom_to_list(Type).
