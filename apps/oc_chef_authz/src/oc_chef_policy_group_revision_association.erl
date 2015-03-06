%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Daniel DeLeo <dan@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_policy_group_revision_association).

-include("../../include/oc_chef_types.hrl").

-export([find_policy_revision_by_orgid_name_group_name/1,
         insert_association/1,
         delete_association/1,

         id/1,
         fields_for_fetch/1,
         find_query/0,
         create_query/0,
         delete_query/0,
         record_fields/0,
         flatten/1,
         is_indexed/0,
         ejson_for_indexing/2]).

find_policy_revision_by_orgid_name_group_name(Record) ->
    case chef_sql:fetch_object(
      chef_object:fields_for_fetch(Record),
      element(1, Record),
      chef_object:find_query(Record),
      chef_object:record_fields(Record)
      ) of
        {ok, #oc_chef_policy_group_revision_association{} = Assoc} ->
          Decompressed = decompress_record(Assoc),
          {ok, Decompressed};
        Any -> Any
    end.

insert_association(#oc_chef_policy_group_revision_association{} = Record) ->
    ok = ensure_policy_exists(Record),
    ok = ensure_policy_group_exists(Record),
    ok = ensure_policy_revision_exists(Record),
    Query = chef_object:create_query(Record),
    FlattenedRecord = chef_object:flatten(Record),
    chef_sql:create_object(Query, FlattenedRecord).

delete_association(#oc_chef_policy_group_revision_association{} = Assoc) ->
    Query = chef_object:delete_query(Assoc),
    Id = chef_object:id(Assoc),
    chef_sql:delete_object(Query, Id).

ensure_policy_exists(#oc_chef_policy_group_revision_association{policy = Policy}) ->
    QueryResult = fetch_associated_object(Policy),
    case QueryResult of
      {ok, #oc_chef_policy{}} -> ok;
      {ok, not_found} ->
        create_associated_object(Policy),
        ok;
      Bad -> {error, Bad}
    end.

ensure_policy_group_exists(#oc_chef_policy_group_revision_association{policy_group = PolicyGroup}) ->
    QueryResult = fetch_associated_object(PolicyGroup),
    case QueryResult of
      {ok, #oc_chef_policy_group{}} -> ok;
      {ok, not_found} ->
        create_associated_object(PolicyGroup),
        ok;
      Bad -> {error, Bad}
    end.

ensure_policy_revision_exists(#oc_chef_policy_group_revision_association{policy_revision = PolicyRevision}) ->
    QueryResult = fetch_associated_object(PolicyRevision),
    case QueryResult of
      {ok, #oc_chef_policy_revision{}} -> ok;
      {ok, not_found} ->
        create_associated_object(PolicyRevision),
        ok;
      Bad -> {error, Bad}
    end.

create_associated_object(QueryRecord) ->
    Query = chef_object:create_query(QueryRecord),
    FlattenedRecord = chef_object:flatten(QueryRecord),
    {ok, 1} = chef_sql:create_object(Query, FlattenedRecord).

fetch_associated_object(QueryRecord) ->
    chef_sql:fetch_object(chef_object:fields_for_fetch(QueryRecord),
                          element(1, QueryRecord),
                          chef_object:find_query(QueryRecord),
                          chef_object:record_fields(QueryRecord)
                         ).

id(#oc_chef_policy_group_revision_association{id = ID}) ->
    ID.

fields_for_fetch(#oc_chef_policy_group_revision_association{
                  org_id = OrgID,
                  policy_group_name = PolicyGroupName,
                  policy_revision_name = PolicyRevisionName}) ->
    [OrgID, PolicyGroupName, PolicyRevisionName].

find_query() ->
    find_policy_by_group_asoc_and_name.

create_query() ->
    insert_policy_group_policy_revision_association.

delete_query() ->
    delete_policy_group_policy_revision_association_by_id.

record_fields() ->
    record_info(fields, oc_chef_policy_group_revision_association).

decompress_record(#oc_chef_policy_group_revision_association{
                  serialized_object = CompressedSerializedObject} = Assoc) ->
    SerializedObject = jiffy:decode(chef_db_compression:decompress(CompressedSerializedObject)),
    Assoc#oc_chef_policy_group_revision_association{serialized_object = SerializedObject}.

flatten(#oc_chef_policy_group_revision_association{
        id = Id,
        org_id = OrgId,
        policy_revision_name = PolicyName,
        policy_revision_revision_id = RevisionId,
        policy_group_name = GroupName,
        last_updated_by = LastUpdatedBy}) ->
    [Id, OrgId, RevisionId, PolicyName, GroupName, LastUpdatedBy].

is_indexed() ->
    false.

ejson_for_indexing(#oc_chef_policy_group_revision_association{}, _EjsonTerm) ->
   {[]}.


%% Things this should implement:
%% ## GET /policy_groups/:policy_group_name/policies/:policy_name
%% ### SQL Stuff
%% * find policy_revision by policy name and (join) policy group name
%% ### Authz Stuff
%% * read on policy group
%% * read on policy
%%
%% ### Statement
%% SELECT g.group_authz_id, rev.policy_authz_id, rev.serialized_object
%%   FROM policy_revisions_policy_groups g"
%% LEFT JOIN policy_revisions r ON (g.policy_revision_revision_id = r.revision_id)
%%  WHERE (g.org_id = $1 AND g.name = $2 AND r.name = $3 )">>},
%%
%% ## PUT /policy_groups/:policy_group_name/policies/:policy_name
%% ### SQL Stuff
%% * create policy group if needed
%% * create policy if needed
%% * create policy_revision if needed
%% * associate policy_revision to group
%% ### Authz Stuff
%% * create on policy group container or update on policy group
%% * create on policies container or update on policy
%%
%% ## POST /policy_groups/:policy_group_name/policies/:policy_name
%% ### SQL Stuff
%% * create policy group if needed
%% * policy must exist
%% * policy revision must exist
%% * associate policy_revision to groop
%% ### Authz stuff
%% * create on policy groups container or update on policy group
%% * read on policy
%%
%% ## DELETE /policy_groups/:policy_group_name/policies/:policy_name
%% ### SQL stuff
%% * remove policy_revision to policy group association
%% ### Authz Stuff
%% * update on policy group
%%
