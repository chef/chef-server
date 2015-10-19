-module(solr_provider).
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
    <<"X_CHEF_id_CHEF_X">>.

database_field() ->
    <<"X_CHEF_database_CHEF_X">>.

type_field() ->
    <<"X_CHEF_type_CHEF_X">>.

ping_url() ->
    "/admin/ping?wt=json".

update_url() ->
    "/update".

search_url_fmt() ->
    "/select?"
        "fq=~s"
        "&indent=off"
        "&q=~s"
        "&start=~B"
        "&rows=~B"
        "&wt=json"
        "&sort=~s".

kv_sep() ->
    <<"__=__">>.

handle_successful_search(ResponseBody) ->
    Response = ej:get({<<"response">>}, ResponseBody),
    Start    = ej:get({<<"start">>}, Response),
    NumFound = ej:get({<<"numFound">>}, Response),
    DocList  = ej:get({<<"docs">>}, Response),
    Ids = [ ej:get({id_field()}, Doc) || Doc <- DocList ],
    {ok, Start, NumFound, Ids}.

make_standard_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":" ++ ObjType.

make_data_bag_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":data_bag_item +data_bag:" ++ ObjType.

add_org_guid_to_fq(OrgGuid, FilterQuery) ->
    "+" ++ search_db_from_orgid(OrgGuid) ++ " " ++ FilterQuery.

transform_query_all(Query) ->
    Query.

transform_query_safe(Query) ->
    Query.

transform_query_term(Query) ->
    Query.

transform_query_phrase(Query) ->
    Query.

transform_data(Data) ->
    Data.

assert_org_id_filter(FieldQuery) ->
    Start = "+" ++ binary_to_list(database_field()) ++ ":chef_",
    Len = length(Start),
    Start = string:substr(FieldQuery, 1, Len).

delete_search_db(OrgId) ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        search_db_from_orgid(OrgId) ++
        "</query></delete>",
    ok = chef_solr:update(?MODULE, DeleteQuery),
    ok = commit(),
    ok.

commit() ->
    chef_solr:update(?MODULE, "<?xml version='1.0' encoding='UTF-8'?><commit/>").

-spec delete_search_db_by_type(OrgId :: binary(), Type :: atom()) -> ok.
delete_search_db_by_type(OrgId, Type)
  when Type == client orelse Type == data_bag_item orelse
       Type == environment orelse Type == node orelse
       Type == role ->
    DeleteQuery = "<?xml version='1.0' encoding='UTF-8'?><delete><query>" ++
        search_db_from_orgid(OrgId) ++ " AND " ++
        search_type_constraint(Type) ++
        "</query></delete>",
    chef_solr:update(?MODULE, DeleteQuery).

-spec search_db_from_orgid(OrgId :: binary()) -> DBName :: [byte(),...].
search_db_from_orgid(OrgId) ->
    binary_to_list(database_field()) ++ ":" ++ chef_solr:db_from_orgid(OrgId).

%% @doc Generates a constraint for chef_type
%% @end
-spec search_type_constraint(Type :: atom()) -> TypeConstraint :: [byte(),...].
search_type_constraint(Type) ->
    binary_to_list(type_field()) ++ atom_to_list(Type).
