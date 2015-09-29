-module(chef_index_query).
-export([
         from_params/5,
         add_org_guid_to_query/2,
         search_db_from_orgid/1,
         search_type_constraint/1,
         assert_org_id_filter/1,
         id_field/0,
         database_field/0,
         type_field/0
        ]).

-include("chef_solr.hrl").

id_field() ->
    <<"X_CHEF_id_CHEF_X">>.
database_field() ->
    <<"X_CHEF_database_CHEF_X">>.
type_field() ->
    <<"X_CHEF_type_CHEF_X">>.

-spec from_params(atom(),
                  binary()|string(),
                  string() | binary() | undefined,
                  string(),
                  string()) -> #chef_solr_query{}.
from_params(Provider, ObjType, QueryString, Start, Rows) ->
    #chef_solr_query{query_string = check_query(QueryString),
                     filter_query = make_fq_type(ObjType),
                     search_provider = Provider,
                     start = decode({nonneg_int, "start"}, Start, 0),
                     rows = decode({nonneg_int, "rows"}, Rows, 1000),
                     sort = "X_CHEF_id_CHEF_X asc",
                     index = index_type(ObjType)}.

-spec add_org_guid_to_query(#chef_solr_query{}, binary()) -> #chef_solr_query{}.
add_org_guid_to_query(Query = #chef_solr_query{filter_query = FilterQuery}, OrgGuid) ->
    Query#chef_solr_query{filter_query = add_org_guid_to_fq(OrgGuid, FilterQuery)}.

-spec assert_org_id_filter(string()) -> ok.
assert_org_id_filter(FilterQuery) ->
    Start = "+" ++ binary_to_list(database_field()) ++ ":chef_",
    Len = length(Start),
    Start = string:substr(FilterQuery, 1, Len),
    ok.

add_org_guid_to_fq(OrgGuid, FilterQuery) ->
    "+" ++ search_db_from_orgid(OrgGuid) ++ " " ++ FilterQuery.

make_fq_type(ObjType) when is_binary(ObjType) ->
    make_fq_type(binary_to_list(ObjType));
make_fq_type(ObjType) when ObjType =:= "node";
                           ObjType =:= "role";
                           ObjType =:= "client";
                           ObjType =:= "environment" ->
    make_standard_fq(ObjType);
make_fq_type(ObjType) ->
    make_data_bag_fq(ObjType).

make_standard_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":" ++ ObjType.

make_data_bag_fq(ObjType) ->
    "+" ++ binary_to_list(type_field()) ++ ":data_bag_item +data_bag:" ++ ObjType.

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


%% @doc Generates a constraint for chef_type
%% @end
-spec search_type_constraint(Type :: atom()) -> TypeConstraint :: [byte(),...].
search_type_constraint(Type) ->
    binary_to_list(type_field()) ++ atom_to_list(Type).

%% @doc Generate a constraint for chef_database
-spec search_db_from_orgid(OrgId :: binary()) -> DBName :: [byte(),...].
search_db_from_orgid(OrgId) ->
    binary_to_list(database_field()) ++ ":" ++ db_from_orgid(OrgId).

db_from_orgid(OrgId) ->
    "chef_" ++ binary_to_list(OrgId).
