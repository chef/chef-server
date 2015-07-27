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

-include_lib("mixer/include/mixer.hrl").
-include("oc_chef_types.hrl").

% TOD - is this supposed to be a chef_object? It looks kind of like one, but missing serveral things.
-export([find_policy_revision_by_orgid_name_group_name/2,
         fetch_prereq_objects/2,
         insert_association/3,
         update_association/3,
         delete_association/2,

         id/1,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         fields_for_fetch/1,
         fields_for_update/1,
         find_query/1,
         create_query/1,
         update_query/1,
         delete_query/1,
         record_fields/1,
         record_for_find/3,
         new_record/7,
         update_record/8,
         fields_for_insert/1,
         is_indexed/1,
         ejson_for_indexing/2
        ]).

-mixin([{chef_object_default_callbacks, [fetch/2, update/2]}]).


find_policy_revision_by_orgid_name_group_name(Record, DBContext) ->
    case chef_db:fetch(Record, DBContext) of
        #oc_chef_policy_group_revision_association{} = Assoc ->
          decompress_record(Assoc);
        Any -> Any
    end.

%% In the WM resources, we optimisticially try to fetch the revision
%% association. If we don't find it, and we're trying to create a revision
%% association, we need to check authz against the policy and policy group; if
%% either of those doesn't exist, the WM resource will have to create authz IDs
%% for them.
fetch_prereq_objects(#oc_chef_policy_group_revision_association{
                    policy = PolicyQuery,
                    policy_group = PolicyGroupQuery
                }, DBContext) ->
    MaybePolicy = fetch_associated_object(PolicyQuery, DBContext),
    MaybePolicyGroup = fetch_associated_object(PolicyGroupQuery, DBContext),
    [{policy, MaybePolicy}, {policy_group, MaybePolicyGroup}].

insert_association(#oc_chef_policy_group_revision_association{} = Record, DBContext, ActorID) ->
    ok = ensure_policy_exists(Record, DBContext, ActorID),
    ok = ensure_policy_group_exists(Record, DBContext, ActorID),
    ok = ensure_policy_revision_exists(Record, DBContext, ActorID),
    chef_db:create(Record, DBContext, ActorID).

update_association(#oc_chef_policy_group_revision_association{} = Record, DBContext, ActorID) ->
    % if the association exists, then the policy and policy_group must exist.
    % The policy_revision might be new.
    ok = ensure_policy_revision_exists(Record, DBContext, ActorID),
    chef_db:update(Record, DBContext, ActorID).

delete_association(#oc_chef_policy_group_revision_association{} = Assoc, DBContext) ->
    chef_db:delete(Assoc, DBContext).

ensure_policy_exists(#oc_chef_policy_group_revision_association{policy = Policy}, DBContext, ActorID) ->
    QueryResult = fetch_associated_object(Policy, DBContext),
    case QueryResult of
      #oc_chef_policy{} -> ok;
      not_found ->
        create_associated_object(Policy, DBContext, ActorID),
        ok;
      Bad -> {error, Bad}
    end.

ensure_policy_group_exists(#oc_chef_policy_group_revision_association{policy_group = PolicyGroup}, DBContext, ActorID) ->
    QueryResult = fetch_associated_object(PolicyGroup, DBContext),
    case QueryResult of
      #oc_chef_policy_group{} -> ok;
      not_found ->
        create_associated_object(PolicyGroup, DBContext, ActorID),
        ok;
      Bad -> {error, Bad}
    end.

ensure_policy_revision_exists(#oc_chef_policy_group_revision_association{policy_revision = PolicyRevision}, DBContext, ActorID) ->
    QueryResult = fetch_associated_object(PolicyRevision, DBContext),
    case QueryResult of
      #oc_chef_policy_revision{} -> ok;
      not_found ->
        create_associated_object(PolicyRevision, DBContext, ActorID),
        ok;
      Bad -> {error, Bad}
    end.

create_associated_object(QueryRecord, DBContext, ActorID) ->
    ok = chef_db:create(QueryRecord, DBContext, ActorID).

fetch_associated_object(QueryRecord, DBContext) ->
    chef_db:fetch(QueryRecord, DBContext).

id(#oc_chef_policy_group_revision_association{id = ID}) ->
    ID.

set_created(#oc_chef_policy_group_revision_association{} = Object, ActorId) ->
    Object#oc_chef_policy_group_revision_association{last_updated_by = ActorId}.

set_updated(#oc_chef_policy_group_revision_association{} = Object, ActorId) ->
    Object#oc_chef_policy_group_revision_association{last_updated_by = ActorId}.

fields_for_fetch(#oc_chef_policy_group_revision_association{
                  org_id = OrgID,
                  policy_group_name = PolicyGroupName,
                  policy_revision_name = PolicyRevisionName}) ->
    [OrgID, PolicyGroupName, PolicyRevisionName].

fields_for_update(#oc_chef_policy_group_revision_association{
        id = ID,
        policy_revision_revision_id = PolicyRevisionRevisionId,
        last_updated_by = LastUpdatedBy}) ->
    [PolicyRevisionRevisionId, LastUpdatedBy, ID].

find_query(_ObjectRec) ->
    find_policy_by_group_asoc_and_name.

create_query(_ObjectRec) ->
    insert_policy_group_policy_revision_association.

update_query(_ObjectRec) ->
    update_policy_group_policy_revision_association.

delete_query(_ObjectRec) ->
    delete_policy_group_policy_revision_association_by_id.

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_policy_group_revision_association).

record_for_find(OrgID, PolicyName, GroupName) ->
    #oc_chef_policy_group_revision_association{
        org_id = OrgID,
        policy_revision_name = PolicyName,
        policy_group_name = GroupName,
        policy = #oc_chef_policy{org_id = OrgID, name = PolicyName },
        policy_group = #oc_chef_policy_group{org_id = OrgID, name = GroupName }
        }.

new_record(ApiVersion, OrgID, PolicyName, PolicyAuthzID, PolicyGroupName, PolicyGroupAuthzID, PolicyData) ->
    %% POLICY:   name, org_id, authz_id
    Policy = oc_chef_policy:new_record(ApiVersion, OrgID, PolicyAuthzID, PolicyName),
    %% GROUP:    name, org_id, authz_id
    PolicyGroup = oc_chef_policy_group:new_record(ApiVersion, OrgID, PolicyGroupAuthzID, PolicyGroupName),
    %% Revision: name, ord_id, policy_authz_id, revision_id, serialized_object
    PolicyRevision = oc_chef_policy_revision:new_record(ApiVersion, OrgID, PolicyAuthzID, PolicyData),
    %% PGRA:     org_id, policy_group_authz_id, revision_id, revision_name, policy_group_name, associated records
    #oc_chef_policy_revision{revision_id = RevisionID} = PolicyRevision,

    Id = chef_object_base:make_org_prefix_id(OrgID, <<PolicyName/binary, PolicyGroupName/binary, RevisionID/binary>>),
    #oc_chef_policy_group_revision_association{
        id = Id,
        org_id = OrgID,
        policy_group_authz_id = PolicyGroupAuthzID,
        policy_revision_revision_id = RevisionID,
        policy_revision_name = PolicyName,
        policy_group_name = PolicyGroupName,
        policy = Policy,
        policy_group = PolicyGroup,
        policy_revision = PolicyRevision
        }.

update_record(ApiVersion, ID, OrgID, PolicyName, PolicyAuthzID, PolicyGroupName, PolicyGroupAuthzID, PolicyData) ->
    %% Policy and PolicyGroup must exist or else you'll run afoul of FK constraints.
    PolicyRevision = oc_chef_policy_revision:new_record(ApiVersion, OrgID, PolicyAuthzID, PolicyData),
    #oc_chef_policy_revision{revision_id = RevisionID} = PolicyRevision,
    #oc_chef_policy_group_revision_association{
        server_api_version = ApiVersion,
        id = ID,
        org_id = OrgID,
        policy_group_authz_id = PolicyGroupAuthzID,
        policy_revision_revision_id = RevisionID,
        policy_revision_name = PolicyName,
        policy_group_name = PolicyGroupName,

        policy_revision = PolicyRevision
        }.

decompress_record(#oc_chef_policy_group_revision_association{
                  serialized_object = CompressedSerializedObject} = Assoc) ->
    SerializedObject = jiffy:decode(chef_db_compression:decompress(CompressedSerializedObject)),
    Assoc#oc_chef_policy_group_revision_association{serialized_object = SerializedObject}.

fields_for_insert(#oc_chef_policy_group_revision_association{
        id = Id,
        org_id = OrgId,
        policy_revision_name = PolicyName,
        policy_revision_revision_id = RevisionId,
        policy_group_name = GroupName,
        policy_group_authz_id = PolicyGroupAuthzID,
        last_updated_by = LastUpdatedBy}) ->
    [Id, OrgId, RevisionId, PolicyName, GroupName, PolicyGroupAuthzID, LastUpdatedBy].

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_policy_group_revision_association{}, _EjsonTerm) ->
   {[]}.

set_api_version(ObjectRec, ApiVersion) ->
    ObjectRec#oc_chef_policy_group_revision_association{server_api_version = ApiVersion}.


