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
         transform_query/1,
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

transform_data(Data) ->
    replace_specials(Data).

transform_query(Query) ->
    substitute_sep(Query).

assert_org_id_filter(FieldQuery) ->
    Start = "(and (term field=" ++ binary_to_list(database_field()) ++ " 'chef_",
    Len = length(Start),
    Start = string:substr(FieldQuery, 1, Len).

delete_search_db(_OrgId) ->
    lager:warning("delete_search_db not implemented"),
    ok.

delete_search_db_by_type(_OrgId, _Type) ->
    lager:warning("delete_search_db_by_type not implemented"),
    ok.

commit() ->
    lager:info("Commit not supported when using cloudsearch as a search provider"),
    ok.

%% Internal Functions

%% Structured Query Helpers
%%
%% The filter queries for CloudSearch use the "structured query" format.
sq_term(Field, Value) ->
    "(term field=" ++ Field ++ " " ++ sq_quote(Value) ++ ")".

sq_and(First, Second) ->
    "(and " ++ First ++ Second ++ ")".

sq_quote(Value) ->
    "'" ++ Value ++ "'".

replace_specials(Item) ->
    replace_equal(Item).

replace_equal(Item) ->
    New = re:replace(Item, "=", "__EQ__", [{return, binary}, global]),
    replace_dash(New).

replace_dash(Item) ->
    re:replace(Item, "-", "__DASH__", [{return, binary}, global]).

substitute_sep(Query) ->
    New = re:replace(Query, "__=__", "__EQ__", [{return, binary}, global]),
    replace_specials(New).
