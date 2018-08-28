%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Brown <cb@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @author John Keiser <jkeiser@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author Kevin Smith
%% @author Seth Chisamore <schisamo@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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


-module(chef_wm_search).

-include("oc_chef_wm.hrl").
-include("chef_solr.hrl").

-define(DEFAULT_BATCH_SIZE, 5).

%% We chose to *not* mixin oc_chef_wm_base:post_is_create/2 as a POST in
%% this resource is purely for processing...not resource creation.
-mixin([{oc_chef_wm_base, [content_types_accepted/2,
                           content_types_provided/2,
                           finish_request/2,
                           malformed_request/2,
                           ping/2,
                           forbidden/2,
                           is_authorized/2,
                           service_available/2]}]).


-export([allowed_methods/2,
         process_post/2,
         resource_exists/2,
         to_json/2]).

%% chef_wm behavior callbacks
-behaviour(chef_wm).
-export([auth_info/2,
         init/1,
         init_resource_state/1,
         malformed_request_message/3,
         request_type/0,
         validate_request/3]).


init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_Config) ->
    {ok, #search_state{}}.

request_type() ->
    "search".

allowed_methods(Req, State) ->
    {['GET','POST'], Req, State}.

validate_request('GET', Req, State) ->
    Query = make_query_from_params(Req),
    SearchState = #search_state{solr_query = Query},
    {Req, State#base_state{resource_state = SearchState}};
validate_request('POST', Req, State) ->
    Query = make_query_from_params(Req),
    Body = chef_json:decode(wrq:req_body(Req)),
    validate_body(Body),
    {NamePaths} = Body,
    SearchState = #search_state{solr_query = Query, partial_paths = NamePaths},
    {Req, State#base_state{resource_state = SearchState}}.

auth_info(Req, State) ->
    {authorized, Req, State}.

resource_exists(Req, #base_state{organization_guid = OrgGuid,
                                 resource_state = SearchState} = State) ->
    QueryWithoutGuid = SearchState#search_state.solr_query,
    try
        Query = chef_solr:add_org_guid_to_query(QueryWithoutGuid, OrgGuid),
        SearchState1 = SearchState#search_state{solr_query = Query},
        {true, Req, State#base_state{organization_guid = OrgGuid,
                                     resource_state = SearchState1}}
    catch
        throw:org_not_found ->
            %% Not sure we can ever get here; user in org check will
            %% have failed with 403 if no such org.
            OrgName = chef_wm_util:extract_from_path(organization_id, Req),
            NoOrg = resource_exists_message(org_not_found, OrgName),
            Req1 = chef_wm_util:set_json_body(Req, NoOrg),
            {false, Req1, State#base_state{log_msg = org_not_found}}
    end.

resource_exists_message(org_not_found, Org) ->
    Msg = iolist_to_binary([<<"organization '">>, Org,
                            <<"' does not exist.">>]),
    {[{<<"error">>, [Msg]}]}.

to_json(Req, #base_state{chef_db_context = DbContext,
                         resource_state = SearchState,
                         organization_name = OrgName,
                         organization_guid = OrgId,
                         reqid = ReqId} = State) ->
    BatchSize = batch_size(),
    Query = SearchState#search_state.solr_query,
    case solr_query(Query, ReqId) of
        {ok, Start, SolrNumFound, Ids} ->
            IndexType = Query#chef_solr_query.index,
            Paths = SearchState#search_state.partial_paths,
            BulkGetFun = make_bulk_get_fun(DbContext, OrgName, IndexType, Paths, Req),
            {DbNumFound, Ans} = make_search_results(BulkGetFun, Ids, BatchSize,
                                                    Start, SolrNumFound),
            State1 = State#base_state{log_msg = search_log_msg(SolrNumFound,
                                                               solr_ids_length(Ids), DbNumFound)},
            case IndexType of
                {data_bag, BagName} when DbNumFound =:= 0 ->
                    case chef_db:data_bag_exists(DbContext, OrgId, BagName) of
                        true ->
                            {Ans, Req, State1};
                        false ->
                            Msg = iolist_to_binary([<<"I don't know how to search for ">>,
                                                    BagName, <<" data objects.">>]),
                            {{halt, 404},
                                chef_wm_util:set_json_body(Req, {[{<<"error">>, [Msg]}]}),
                                State1#base_state{log_msg=lists:flatten(["no data bag: ", BagName])}}
                    end;
                _Else ->
                    {Ans, Req, State1}
            end;
        {error, {solr_400, _}=Why} ->
            {{halt, 400},
                chef_wm_util:set_json_body(Req,
                    malformed_request_message(Why, Req, State)),
                State#base_state{log_msg=Why}};
        {error, {solr_500, _}=Why} ->
            {{halt, 500},
                chef_wm_util:set_json_body(Req,
                    malformed_request_message(Why, Req, State)),
                State#base_state{log_msg=Why}}
    end.

solr_ids_length({Solr1Ids, _}) ->
    length(Solr1Ids);
solr_ids_length(Solr1Ids) ->
    length(Solr1Ids).

%% Return current app config value for batch size, defaulting if absent.
batch_size() ->
    envy:get(oc_chef_wm, bulk_fetch_batch_size, ?DEFAULT_BATCH_SIZE, positive_integer).

search_log_msg(SolrNumFound, NumIds, DbNumFound) ->
    {search, SolrNumFound, NumIds, DbNumFound}.

solr_query(Query, ReqId) ->
        stats_hero:ctime(ReqId, {chef_solr, search},
                     fun() ->
                             solr_search(Query)
                     end).

solr_search(Query) ->
    try
        chef_solr:search(Query)
    catch
        Error:Reason ->
            {Error, Reason}
    end.

%% POST to /search represents a partial search request
%% The posted request body should be of the form:
%% { "mk1" : [ "K1", "K2" ],
%%   "mk2" : [ "K3", "K4", "K5" ] }
%%
process_post(Req, State) ->
    {Ans, Req1, State1} = to_json(Req, State),
    case Ans of
        {halt, _} -> {Ans, Req1, State1};
        Result -> {true, wrq:set_resp_body(Result, Req), State1}
    end.

-spec make_bulk_get_fun(chef_db:db_context(),
                        OrgName :: binary(),
                        %% FIXME: these are the chef_types that are indexable for search and
                        %% should move into chef_types.hrl
                        Type :: client |
                                {data_bag, binary()} |
                                environment |
                                node |
                                role,
                        NamePaths :: any(), %% really: [{binary(), [binary()]}],
                        Req :: wm_req() ) ->
                               fun(([binary()]) -> [ej:json_object() | binary()]).

%% @doc Returns a fun/1 that can be given a list of object IDs and returns a list of the
%% corresponding EJSON object. The fun wraps `chef_db:bulk_get' and in some cases does some
%% post-processing of the JSON to make it conform to the expected shape for Chef search
%% results.
%%
%% If `NamePaths' is non-empty, then the returned fun will create partial search results by
%% extracting the values specified by the paths and mapping them to the specified names in
%% the returned EJSON object.
make_bulk_get_fun(DbContext, OrgName, client, [], _Req) ->
    %% clients get special handling to add json_class which is not stored in the db (not
    %% even in couch).
    %%
    %% BUGBUG in waiting: special casing for one class is UBER CODE SMELL
    fun(Ids) ->
            Clients = chef_db:bulk_get(DbContext, OrgName, client, Ids),
            [ ej:set({<<"json_class">>}, Client, <<"Chef::ApiClient">>)
              || Client <- Clients ]
    end;
make_bulk_get_fun(DbContext, OrgName, {data_bag, BagName}, [], _Req) ->
    %% We need to wrap the item in some additional JSON cruft to make it
    %% match the expected shape.
    %%
    %% BUGBUG BUGBUG: special casing for two classes is MEGA UBER CODE STENCH
    fun(Ids) ->
            Items = chef_db:bulk_get(DbContext, OrgName, data_bag_item, Ids),
            %% FIXME: it would be great if we didn't have to wrap the data_bag_item
            %% results this way at all. To reduce the CPU and memory use, we could
            %% add a special bulk get query that also returns bag_name and item_name
            %% and hand-craft the json to avoid parsing.
            [ begin
                        RawItem = chef_json:decode(chef_db_compression:decompress(Item)),
                        ItemName = ej:get({<<"id">>}, RawItem),
                        chef_data_bag_item:wrap_item(BagName, ItemName, RawItem)
                end || Item <- Items ]
    end;
make_bulk_get_fun(DbContext, OrgName, Type, [], _Req) ->
    %% all other types just call into chef_db
    fun(Ids) ->
            chef_db:bulk_get(DbContext, OrgName, Type, Ids)
    end;
make_bulk_get_fun(DbContext, OrgName, Type, NamePaths, Req) ->
    %% Here NamePaths is a non-empty list of {Name, Path} tuples. This is the bulk_get fun
    %% that will be created if the user has requested partial search.
    fun(Ids) ->
            Items = chef_db:bulk_get(DbContext, OrgName, index_type_to_db_type(Type), Ids),
            RouteFun = oc_chef_wm_routes:url_for_search_item_fun(Req, Type, OrgName),
            [ begin
                  EJsonItem = parse_item(Type, Item),
                  Url = RouteFun(EJsonItem),
                  {[{<<"url">>, Url},
                    {<<"data">>,
                     {[ {Name, extract_path(EJsonItem, Path)}
                        || {Name, Path} <- NamePaths ]}}]}
              end
              || Item <- Items ]
    end.

%% chef_db:bulk_get expects a Chef object type, but the Type we have available is for the
%% search index. This will be correct except for the case of data_bag searches that need to
%% bulk_get data_bag_items. So we convert here.
index_type_to_db_type({data_bag, _}) ->
    data_bag_item;
index_type_to_db_type(Type) ->
    Type.

%% Possibly decompress and parse raw JSON into EJSON terms, else pass-through. Data coming
%% from chef_db:bulk_get may either be a list of JSON binary or a list of EJSON so this
%% function is used to normalize to EJSON.
%%
%% node data is a special case because node attributes are deep-merged prior to indexing for
%% search and we want to work with the same merged attributes for extracting partial search
%% data.
%%
%% data_bag is another special case while data bags are still in couchdb where they are
%% stored with wraper cruft. We provide a fun-head to deal with this.
parse_item(node, Item) ->
    Node = parse_item0(Item),
    %% This is fairly hacky. :(
    NodeRecStub = #chef_node{name = ej:get({<<"name">>}, Node),
                             environment = ej:get({<<"chef_environment">>}, Node)},
    chef_object:ejson_for_indexing(NodeRecStub, Node);
parse_item({data_bag, _}, Item) ->
    RawItem = parse_item0(Item),
    %% TODO: when data bags are no longer in couchdb, clean this
    case ej:get({<<"id">>}, RawItem) of
        undefined ->
            %% we have a crufted data_bag_item, de-cruft!
            ej:get({<<"raw_data">>}, RawItem);
        Id when is_binary(Id) ->
            %% no cruft, just return it
            RawItem
    end;
parse_item(_, Item) ->
    parse_item0(Item).

parse_item0(Item) when is_binary(Item) ->
    chef_json:decode(chef_db_compression:decompress(Item));
parse_item0({L}=Item) when is_list(L) ->
    %% should be valid EJSON format
    Item.

%% This helper function extracts the necessary params from the request and passes it to
%% chef_solr:make_query_from_params to return the query.
make_query_from_params(Req) ->
    % TODO - sort this out
    % ObjType = chef_wm_util:extract_from_path(object_type, Req),
    ObjType = chef_wm_util:extract_from_path(object_type, Req),
    QueryString = wrq:get_qs_value("q", Req),
    Start = wrq:get_qs_value("start", Req),
    Rows = wrq:get_qs_value("rows", Req),
    chef_solr:make_query_from_params(ObjType, QueryString, Start, Rows).

extract_path(_Item, []) ->
    null;
extract_path(Item, Path) ->
    ej:get(list_to_tuple(Path), Item, null).

make_search_results(BulkGetFun, Ids, BatchSize, Start, NumFound) ->
    Ans0 = search_result_start(Start, NumFound),
    {N, Ans1} = fetch_result_rows(Ids, BatchSize, BulkGetFun, {0, Ans0}),
    {N, search_result_finish(Ans1)}.

%% @doc Fetch a list of `Ids' in batches of size `BatchSize'.
%%
%% Each batch is fetched using `BulkGetFun'.  Results are cons'd onto `Acc' with each batch
%% separated by ``<<",">>''.
%%
%% Each set of results is processed to remove the _rev key and encode to JSON using ejson.
%% The ejson return value is post-processed to remove the JSON array markers.  The caller is
%% responsible for adding this back to create valid JSON.
%%
%% The purpose of this function is to allow us to build up the search response without ever
%% having more than `BatchSize' objects parsed into full EJSON terms. This helps us to limit
%% the RAM required to produce results for large searches. The trade-off is more complicated
%% processing logic than would be required if we just gathered all of the EJSON into a list
%% and then encoded it to JSON binary.
fetch_result_rows([], _BatchSize, _BulkGetFun, {N, Acc}) ->
    %% fetch complete, return fetched count and inner part of JSON array binary
    {N, Acc};
fetch_result_rows(Ids, BatchSize, BulkGetFun, {N, Acc}) when is_list(Ids) ->
    %% this head match when we are first called with the entire list of Ids. To get things
    %% started, we split the list of Ids into a batch of size `BatchSize' and the rest and
    %% get started.
    fetch_result_rows(safe_split(BatchSize, Ids), BatchSize, BulkGetFun, {N, Acc});
fetch_result_rows({Ids, []}, _BatchSize, BulkGetFun, {N, Acc}) ->
    %% This is the last batch, don't add a "," separator
    Docs = BulkGetFun(Ids),
    {N + length(Docs), encode_results(Docs, Acc)};
fetch_result_rows({Ids, Rest}, BatchSize, BulkGetFun, {N, Acc}) ->
    %% processing a batch happens here. we fetch the objects corresponding to the batch of
    %% Ids, encode them and then add them to our accumulator with the "," separator.
    Next = safe_split(BatchSize, Rest),
    Docs = BulkGetFun(Ids),
    fetch_result_rows(Next, BatchSize, BulkGetFun,
                      {N + length(Docs),
                       encode_results(Docs, <<",">>, Acc)}).

%% Catch the case where we're building results ending with
%% a dangling comma and strip it out. Completely ugly since
%% we're assuming the separator is a comma.
%% FIXME Refactor into a more readable/understandable design
encode_results([], [<<",">>|Acc]) ->
    Acc;
encode_results([], Acc) ->
    Acc;
encode_results(Results, Acc) ->
    [encode_result_rows(Results) | Acc].

encode_results([], _Prefix, Acc) ->
    Acc;
encode_results(Results, Prefix, Acc) ->
    [Prefix, encode_result_rows(Results) | Acc].

%% Encode a list of items as a JSON array partial. That is, encode as a JSON array and then
%% strip the '[' and ']' off the result. This allows us to incrementally encode a long array
%% of objects in batches avoiding having all objects in memory in order to encode them.
%%
%% This function knows how to deal with gzip binary from SQL and with EJSON data coming
%% straight from couch. If the data has come from couch, this is where couch cruft keys _id
%% and _rev are removed.
encode_result_rows([Item|_Rest]=Items) when is_binary(Item) ->
    ItemList = << <<(chef_db_compression:decompress(Bin))/binary, ",">> || Bin <- Items >>,
    %% remove trailing "," from binary
    binary:part(ItemList, {0, size(ItemList) - 1});
encode_result_rows(Items) ->
    %% ensure no couchdb cruft leaks out
    CleanItems = [ {remove_couchdb_keys(Doc)} || {Doc} <- Items ],
    Bin = chef_json:encode(CleanItems),
    %% remove leading '[' and trailing ']' so that we can add to this result.
    binary:part(Bin, {1, size(Bin) - 2}).

%% Remove couchdb internal keys "_rev" and "_id" from a tuple list where the keys are
%% assumed to be binaries. The first two instances of _rev or _id will be removed, so if you
%% somehow have duplicates, this will not remove all occurances.
remove_couchdb_keys([]) ->
    [];
remove_couchdb_keys(L) ->
    remove_couchdb_keys(L, 0).

remove_couchdb_keys([{Key, _}|T], N) when Key =:= <<"_rev">>;
                                          Key =:= <<"_id">> ->
    remove_couchdb_keys(T, N+1);
remove_couchdb_keys(L, N) when N > 1 ->
    L;
remove_couchdb_keys([H|T], N) ->
    [H|remove_couchdb_keys(T, N)];
remove_couchdb_keys([], _) ->
    [].


safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.

%% Return the start of a JSON response for search results. We take this approach to limit
%% RAM use and avoid having the entire result parsed into EJSON terms at one time.
search_result_start(Start, Total) ->
    % {"total":Total,"start":Start,"rows":[i1, i2]}
    ["\"rows\":[", ",",
     integer_to_list(Start), "\"start\":", ",",
     integer_to_list(Total), "\"total\":", "{"].

search_result_finish(Result) ->
    %% Note that all we need here is an iolist not a flat binary.
    lists:reverse([<<"]}">>|Result]).

malformed_request_message(#ej_invalid{}, _Req, _State) ->
    Msg = <<"invalid partial search request body">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_query, Query}, _Req, _State) ->
    Msg = iolist_to_binary([<<"invalid search query: '">>, Query, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({bad_param, {Param, Value}}, _Req, _State) ->
    Msg = iolist_to_binary([<<"invalid '">>, Param, <<"' value: '">>, Value, <<"'">>]),
    {[{<<"error">>, [Msg]}]};
malformed_request_message({solr_400, _}, _Req, _State) ->
    Msg = <<"invalid index name or query">>,
    {[{<<"error">>, [Msg]}]};
malformed_request_message({solr_500, _Query}, _Req, _State) ->
    Msg = <<"internal search error">>,
    {[{<<"error">>, [Msg]}]}.

-spec validate_body(ej:json_object()) -> {ok, ej:json_object()}.
validate_body(Body) ->
    case ej:valid(partial_search_spec(), Body) of
        ok -> {ok, Body};
        Bad -> throw(Bad)
    end.

partial_search_spec() ->
    {object_map,
     {{keys, string},
      {values, {array_map, string}}}}.
