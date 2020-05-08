%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2012-2014 Chef Software, Inc. All Rights Reserved.
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

-module(chef_reindex).
-compile([warnings_as_errors]).

-ifdef(namespaced_types).
-type dict() :: dict:dict().
-endif.

-include("chef_types.hrl").
-include_lib("ej/include/ej.hrl").

%% A binary() index is taken to be a data bag name.
-type index() :: client | environment | node | role | binary().

%% A bundle of OrgID and Org name
-type org_info() :: {object_id(), binary()}.

-export([
         make_context/0,
         reindex/2,
         reindex_by_id/4,
         reindex_by_name/4
        ]).

%% @doc Sends all indexed items (clients, data bag items,
%% environments, nodes, and roles) from an organization to Solr for
%% reindexing.  Does not drop existing index entries beforehand; this
%% is explicitly a separate step.
-spec reindex(Ctx :: chef_db:db_context(),
              OrgInfo :: org_info()) -> ok.
reindex(Ctx, {OrgId, _OrgName}=OrgInfo) ->
    AllIndexes = fetch_org_indexes(Ctx, OrgId),
    [ reindex(Ctx, OrgInfo, Index) || Index <- AllIndexes ],
    ok.

fetch_org_indexes(Ctx, OrgId) ->
    BuiltInIndexes = [node, role, environment, client],
    DataBags = chef_db:list(#chef_data_bag{org_id = OrgId}, Ctx),
    BuiltInIndexes ++ DataBags.

%% A note about our approach to reindexing:
%%
%% In order to make maximum reuse of existing code, we need to perform
%% a few gymnastics in order to get reindexing working properly.  On a
%% high level, reindexing involves iterating through every indexable
%% object in the database and placing each one on the indexing queue.
%%
%% The object placed on the queue is in most cases identical to the
%% JSON that is submitted to the Chef API (and stored in the
%% 'serialized_object' column in the appropriate database table).
%% Some object types (namely nodes and data bag items) must be
%% processed further, however, so we must take that into account.
%%
%% Additionally, Solr currently only stores the database ID of an
%% object; search results actually return just the IDs of objects that
%% match the search; this information is then used to retrieve the
%% actual data from the database to return to the user.  This ID is
%% thus required when putting an object on the indexing queue.
%% Unfortunately, current code for "bulk retrieval" of the
%% 'serialized_object' column from relevant tables in the database
%% does not return this database ID.  Until Sqerl is able to perform
%% ad-hoc SQL queries (a feature that should arrive soon!), it is
%% impractical to add additional "bulk" queries that return, say, the
%% serialized_object AND id column, or perhaps an entire record
%% representation.
%%
%% Thus, the current approach is to perform one additional database
%% query that fetches both the database ID and the unique name of an
%% item; the latter is available in the serialized_object data (once
%% it is decompressed and decoded into an EJson data structure).  By
%% processing this information into a dict mapping the name to the ID,
%% and carrying this dict through the reindexing processing, we can
%% quickly find the information we need.
%%
%% Thus, this name-to-id dict can be thought of as supplemental
%% information that enables the correct reconstruction of index data
%% for a given Chef object.  This data is NOT required for the
%% reindexing process per se, but is strictly a feature of the current
%% implementation, whose priority is the maximum reuse of existing
%% code.  Once Sqerl can perform ad-hoc queries (which will enable us
%% to get away from our current "bulk_get" approach more generally), a
%% more focused implementation may be substituted.

%% @doc Gathers supplemental information required for proper index
%% data reconstruction and begins the batch reindexing of all items of
%% the given indexable type.
-spec reindex(DbContext :: chef_db:db_context(),
              OrgInfo :: org_info(),
              Index :: index()) -> ok.
reindex(Ctx, {OrgId, _OrgName}=OrgInfo, Index) ->
    NameIdDict = chef_db:create_name_id_dict(Ctx, Index, OrgId),
    %% Grab all the database IDs to do batch retrieval on
    AllIds = all_ids_from_name_id_dict(NameIdDict),
    BatchSize = envy:get(oc_chef_wm, bulk_fetch_batch_size, pos_integer),
    batch_reindex(Ctx, AllIds, BatchSize, OrgInfo, Index, NameIdDict).

%% @doc Reindex the objects with the specified `Ids' in the given `Index'.
-spec reindex_by_id(DbContext :: chef_db:db_context(),
                    OrgInfo :: org_info(),
                    Index :: index(),
                    Ids :: [<<_:256>>]) -> ok.
reindex_by_id(Ctx, {OrgId, _OrgName} = OrgInfo, Index, Ids) ->
    %% This is overkill, but workable until we have more robust reindexing
    NameIdDict = chef_db:create_name_id_dict(Ctx, Index, OrgId),
    {ok, BatchSize} = application:get_env(oc_chef_wm, bulk_fetch_batch_size),
    batch_reindex(Ctx, Ids, BatchSize, OrgInfo, Index, NameIdDict).

-spec reindex_by_name(DbContext :: chef_db:db_context(),
                      OrgInfo :: org_info(),
                      Index :: index(),
                      Names :: [binary()]) -> ok.
reindex_by_name(Ctx, {OrgId, _OrgName} = OrgInfo, Index, Names) ->
    NameIdDict = chef_db:create_name_id_dict(Ctx, Index, OrgId),
    MissingIds = [],
    {Ids, MissingIds} = lists:foldl(
                          fun(Name, Acc) ->
                            case dict:find(Name, NameIdDict) of
                              {ok, Id} ->
                                {[Id | Acc], MissingIds};
                            error ->
                              lager:warning("skipping: no id found for name ~p", [Name]),
                              %% The lager warning does not print anything on the console
                              io:format("skipping, no id found for name ~p", [Name]),
                              {Acc, lists:append([Name], MissingIds)}
                            end
                          end, [], Names),
    {ok, BatchSize} = application:get_env(oc_chef_wm, bulk_fetch_batch_size),
    batch_reindex(Ctx, Ids, BatchSize, OrgInfo, Index, NameIdDict).

all_ids_from_name_id_dict(NameIdDict) ->
    dict:fold(fun(_K, V, Acc) -> [V|Acc] end,
              [],
              NameIdDict).

make_context() ->
    Time = string:join([ integer_to_list(I) || I <- tuple_to_list(os:timestamp())], "-"),
    ReqId = erlang:iolist_to_binary([atom_to_list(node()),
                                     "-chef_reindex-",
                                     Time
                                    ]),
    chef_db:make_context(ReqId).

%% All object types are returned from chef_db:bulk_get/4 as
%% binaries (compressed or not) EXCEPT for clients, which are
%% returned as EJson directly, because none of their
%% information is actually stored as a JSON "blob" in the
%% database.
decompress_and_decode(Bin) when is_binary(Bin) ->
    chef_db_compression:decompress_and_decode(Bin);
decompress_and_decode(Object) ->
    Object.

%% @doc Recursively batch-process a list of database IDs by retrieving
%% the serialized_object data from the database, processing them as
%% appropriate, and placing them on the indexing queue.
-spec batch_reindex(chef_db:db_context(),
                    Ids :: [object_id()] |                   %% Initial entry
                           {[object_id()], [object_id()]},   %% Recursive calls; current batch & remaining IDs
                    BatchSize :: non_neg_integer(),
                    OrgInfo :: org_info(),
                    Index :: index(),
                    NameIdDict :: dict()) -> ok.
batch_reindex(Ctx, Ids, BatchSize, OrgInfo, Index, NameIdDict) when is_list(Ids) ->
    DoBatch = fun(Batch, _Acc) ->
                      ok = index_a_batch(Ctx, Batch, OrgInfo, Index, NameIdDict),
                      ok
                 end,
    chefp:batch_fold(DoBatch, Ids, ok, BatchSize).

%% @doc Helper function to retrieve and index objects for a single
%% batch of database IDs.
-spec index_a_batch(chef_db:db_context(),
                    BatchOfIds :: [object_id()],
                    OrgInfo :: org_info(),
                    Index :: index(),
                    NameIdDict :: dict()) -> ok.
index_a_batch(Ctx, BatchOfIds, {OrgId, OrgName}, Index, NameIdDict) ->
    lager:info("reindexing[~s] indexing batch of ~p ~ss", [OrgName, length(BatchOfIds), Index]),
    SerializedObjects = chef_db:bulk_get(Ctx, OrgName, chef_object_type(Index), BatchOfIds),
    ok = send_to_index_queue(OrgId, Index, SerializedObjects, NameIdDict),
    ok.

%% @doc Resolve an index to the name of the corresponding Chef object type.
chef_object_type(Index) when is_binary(Index) -> data_bag_item;
chef_object_type(Index)                       -> Index.

%% @doc Converts a list of serialized_objects into proper index data,
%% taking into account the kind of objects they are, and places each
%% item on the index queue.
-spec send_to_index_queue(OrgId :: object_id(),
                          Index :: index(),
                          SerializedObjects :: [binary()] | [ej:json_object()],
                          NameIdDict :: dict()) -> ok.
send_to_index_queue(_, _, [], _) ->
    ok;
send_to_index_queue(OrgId, Index, [SO|Rest], NameIdDict) ->
    %% All object types are returned from chef_db:bulk_get/4 as
    %% binaries (compressed or not) EXCEPT for clients, which are
    %% returned as EJson directly, because none of their
    %% information is actually stored as a JSON "blob" in the
    %% database.
    PreliminaryEJson = decompress_and_decode(SO),
    NameKey = name_key(chef_object_type(Index)),
    ItemName = ej:get({NameKey}, PreliminaryEJson),
    {ok, ObjectId} = dict:find(ItemName, NameIdDict),
    StubRec = stub_record(Index, OrgId, ObjectId, ItemName, PreliminaryEJson),
    %% Since we get here via valid `Index', we don't have to check that the objects are
    %% indexable.
    ok = oc_chef_object_db:add_to_solr(StubRec, PreliminaryEJson),
    send_to_index_queue(OrgId, Index, Rest, NameIdDict).

%% @doc Determine the proper key to use to retrieve the unique name of
%% an object from its EJson representation.
name_key(data_bag_item) -> <<"id">>;
name_key(_Type)         -> <<"name">>.

%% The {@link oc_chef_object_db:add_to_solr} expects a Chef object record and EJSON as
%% arguments. Since we have only some meta data and the EJSON, we stub out enough of an
%% object record to work. This is a fragile hack. The reindexing function should retrieve a
%% list of complete records from the db and process those.
stub_record(client, OrgId, Id, Name, _EJson) ->
    #chef_client{org_id = OrgId, id = Id, name = Name};
stub_record(environment, OrgId, Id, Name, _EJson) ->
    #chef_environment{org_id = OrgId, id = Id, name = Name};
stub_record(node, OrgId, Id, Name, EJson) ->
    #chef_node{org_id = OrgId, id = Id, name = Name,
               environment = ej:get({<<"chef_environment">>}, EJson)};
stub_record(role, OrgId, Id, Name, _EJson) ->
    #chef_role{org_id = OrgId, id = Id, name = Name};
stub_record(DataBagName, OrgId, Id, _Name, EJson) ->
    ItemName = ej:get({<<"id">>}, EJson),
    #chef_data_bag_item{org_id = OrgId, id = Id,
                        data_bag_name = DataBagName,
                        item_name = ItemName}.
