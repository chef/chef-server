-module(cloudsearch_provider).
-export([
         %% Static Data Accessors
         database_field/0,
         id_field/0,
         kv_sep/0,
         ping_url/0,
         type_field/0,
         update_url/0,
         %% Query Building Helpers
         add_org_guid_to_fq/2,
         assert_org_id_filter/1,
         make_standard_fq/1,
         make_data_bag_fq/1,
         search_url_fmt/0,
         transform_query_all/1,
         transform_query_safe/1,
         transform_query_term/1,
         transform_query_phrase/1,
         %% Document Building Helpers
         transform_data/1,
         %% Response Handling Functions
         handle_successful_search/1,
         %% Solr Operations
         commit/0,
         delete_search_db/1,
         delete_search_db_by_type/2
        ]).

id_field() ->
    <<"x_chef_id_chef_x">>.

database_field() ->
    <<"x_chef_database_chef_x">>.

type_field() ->
    <<"x_chef_type_chef_x">>.

ping_url() ->
    "/search".

update_url() ->
    "/documents/batch".

search_url_fmt() ->
    "/search?"
        "fq=~s"
        "&q=~s"
        "&q.parser=lucene"
        "&start=~B"
        "&size=~B"
        "&sort=~s".

kv_sep() ->
    <<"__EQ__">>.

handle_successful_search(ResponseBody) ->
    Response = ej:get({<<"hits">>}, ResponseBody),
    Start    = ej:get({<<"start">>}, Response),
    NumFound = ej:get({<<"found">>}, Response),
    DocList  = ej:get({<<"hit">>}, Response),
    Ids = [ ej:get({id_field()}, Doc) || Doc <- unwrap_doclist(DocList) ],
    {ok, Start, NumFound, Ids}.

unwrap_doclist(DocList) ->
    [ej:get({<<"fields">>}, Doc) || Doc <- DocList].

make_standard_fq(ObjType) ->
    sq_term(binary_to_list(type_field()), ObjType).

make_data_bag_fq(ObjType) ->
    sq_term(binary_to_list(type_field()), "data_bag_item") ++ sq_term("data_bag", ObjType).

add_org_guid_to_fq(OrgGuid, FilterQuery) ->
    sq_and(
      sq_term(binary_to_list(database_field()),
              chef_solr:db_from_orgid(OrgGuid)),
      FilterQuery).

transform_data(Data) when is_binary(Data) ->
    unicode:characters_to_binary(cs_escape:escape(binary_to_list(Data)), utf8).

transform_query_all(Data) when is_binary(Data) ->
    unicode:characters_to_binary(cs_escape:escape(binary_to_list(Data)), utf8).

transform_query_safe(Data) when is_binary(Data) ->
    unicode:characters_to_binary(cs_escape:escape_safe(binary_to_list(Data)), utf8).

transform_query_term(Data) when is_binary(Data) ->
    unicode:characters_to_binary(cs_escape:escape_term_safe(binary_to_list(Data)), utf8).

transform_query_phrase(Data) ->
    unicode:characters_to_binary(cs_escape:escape_phrase_safe(binary_to_list(Data)), utf8).

assert_org_id_filter(FieldQuery) ->
    Start = "(and (term field=" ++ binary_to_list(database_field()) ++ " 'chef_",
    Len = length(Start),
    Start = string:substr(FieldQuery, 1, Len).

delete_search_db(OrgId) ->
    lager:warning("CloudSearch does not support manual commiting. Search requests may return deleted data for some time."),
    Query = "x_chef_database_chef_x:chef_" ++ binary_to_list(OrgId),
    delete_all_docs_for_query(Query).

delete_search_db_by_type(OrgId, Type) ->
    lager:warning("CloudSearch does not support manual commiting. Search requests may return deleted data for some time."),
    Query = "x_chef_type_chef_x:" ++ safe_bin_to_list(Type) ++ " AND x_chef_database_chef_x:chef_" ++ safe_bin_to_list(OrgId),
    delete_all_docs_for_query(Query).

commit() ->
    lager:info("Commit not supported when using cloudsearch as a search provider"),
    ok.

%% Internal Functions
safe_bin_to_list(List) when is_list(List)->
    List;
safe_bin_to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).

delete_all_docs_for_query(Query) ->
    delete_all_docs_for_query(Query, <<"initial">>, undefined).

delete_all_docs_for_query(_Query, _NextCursor, _NextCursor) ->
    ok;
delete_all_docs_for_query(Query, NextCursor, _LastCursor) ->
    UrlFmt = "/search?q=~s&q.parser=lucene&size=10000&&cursor=~s&sort=~s",
    Sort = binary_to_list(id_field()) ++ "+asc",
    Url = io_lib:format(UrlFmt, [Query, NextCursor, Sort]),
    {ok, Code, _Head, Body} = chef_index_http:request(Url, get, []),
    case Code of
        "200" ->
            ResponseBody = jiffy:decode(Body),
            Response = ej:get({<<"hits">>}, ResponseBody),
            NewNextCursor = ej:get({<<"cursor">>}, Response),
            DocList  = ej:get({<<"hit">>}, Response),
            Ids = [ ej:get({id_field()}, Doc) || Doc <- unwrap_doclist(DocList) ],
            DeleteDocs = [ chef_index_expand:doc_for_delete(unused, Id, unused) || Id <- Ids],
            case DeleteDocs of
                [] ->
                    %% special case an empty page, it is likely the last page
                    delete_all_docs_for_query(Query, NewNextCursor, NextCursor);
                _ ->
                    chef_index_expand:send_delete(DeleteDocs),
                    delete_all_docs_for_query(Query, NewNextCursor, NextCursor)
            end;
        "400" ->
            {error, {solr_400, Url}};
        "500" ->
            {error, {solr_500, Url}}
    end.

%% Structured Query Helpers
%%
%% The filter queries for CloudSearch use the "structured query" format.
sq_term(Field, Value) ->
    "(term field=" ++ Field ++ " " ++ sq_quote(Value) ++ ")".

sq_and(First, Second) ->
    "(and " ++ First ++ Second ++ ")".

sq_quote(Value) ->
    "'" ++ Value ++ "'".
