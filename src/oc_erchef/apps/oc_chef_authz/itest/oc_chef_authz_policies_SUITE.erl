%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%%
%% Copyright 2013-2015 Chef, Inc. All Rights Reserved.
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

-module(oc_chef_authz_policies_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("oc_chef_authz.hrl").
-include("oc_chef_types.hrl").
-include("server_api_version.hrl").

%% Note: this is also defined in the schema test data
%% TODO: this is NOT the id of the org we make in init_per_suite/1, which is
%% probably confusing... this seems unused, delete it?
-define(ORG_ID, <<"33330000000000000000000000000000">>).

all() -> [
          policy_ops,
          policy_group_ops,
          policy_revision_ops
         ].

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db(LastConfig, "oc_chef_authz_itests"),
    suite_helper:start_server(Config),
    OrgsConfig = chef_test_suite_helper:make_orgs(),
    OrgsConfig ++ Config.

end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, suite_helper:needed_apps()).

policy_ops(Config) ->
    insert_policy_data(Config),
    list_policies(Config),
    show_policy(Config),
    delete_policies(Config).

insert_policy_data(Config) ->
    Policies = primary_org_policy_fixtures(Config) ++ other_org_policy_fixtures(Config),
    Results = [ chef_test_suite_helper:create_record(Policy) || Policy <- Policies ],
    Expected = lists:duplicate(length(Policies), {ok, 1}),
    ?assertEqual(Expected, Results).

list_policies(Config) ->
    ExistingPolicies = primary_org_policy_fixtures(Config),
    Names = [ [ Policy#oc_chef_policy.name ] || Policy <- ExistingPolicies],
    Expected = Names,
    Actual = chef_test_suite_helper:list_records(hd(ExistingPolicies)),
    ?assertEqual(Expected, Actual).

show_policy(Config) ->
    PolicyFixture = hd(primary_org_policy_fixtures(Config)),
    {ok, Got} = chef_test_suite_helper:fetch_record(PolicyFixture),
    ?assertEqual(PolicyFixture, Got).

delete_policies(Config) ->
    Policies = primary_org_policy_fixtures(Config) ++ other_org_policy_fixtures(Config),
    Results = [ chef_test_suite_helper:delete_record(Policy) || Policy <- Policies ],
    Expected = lists:duplicate(length(Policies), {ok, 1}),
    ?assertEqual(Expected, Results),
    ListResult = chef_test_suite_helper:list_records(hd(Policies)),
    ?assertEqual([], ListResult),
    OtherOrgList = chef_test_suite_helper:list_records(lists:last(Policies)),
    ?assertEqual([], OtherOrgList).


make_policy(Prefix, OrgId) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    Name = <<"policy", Prefix/binary>>,
    #oc_chef_policy{server_api_version = ?API_MIN_VER, id = Id, authz_id = AzId,
                    org_id = OrgId, name = Name,
                    last_updated_by = chef_test_suite_helper:actor_id()}.

primary_org_policy_fixtures(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    [make_policy(<<"1">>, OrgId),
     make_policy(<<"2">>, OrgId),
     make_policy(<<"3">>, OrgId)].

other_org_policy_fixtures(Config) ->
    OtherOrgId = proplists:get_value(other_org_id, Config),
    [make_policy(<<"4">>, OtherOrgId)].

policy_group_ops(Config) ->
    insert_policy_group_data(Config),
    list_policy_groups(Config),
    show_policy_group(Config),
    delete_policy_groups(Config).

insert_policy_group_data(Config) ->
    PolicyGroups = primary_org_policy_group_fixtures(Config) ++ other_org_policy_group_fixtures(Config),
    Results = [ chef_test_suite_helper:create_record(PolicyGroup) || PolicyGroup <- PolicyGroups ],
    Expected = lists:duplicate(length(PolicyGroups), {ok, 1}),
    ?assertEqual(Expected, Results).

list_policy_groups(Config) ->
    ExistingPolicyGroups = primary_org_policy_group_fixtures(Config),
    Names = [ [ PolicyGroup#oc_chef_policy_group.name ] || PolicyGroup <- ExistingPolicyGroups],
    Expected = Names,
    Actual = chef_test_suite_helper:list_records(hd(ExistingPolicyGroups)),
    ?assertEqual(Expected, Actual).

show_policy_group(Config) ->
    PolicyGroupFixture = hd(primary_org_policy_group_fixtures(Config)),
    {ok, Got} = chef_test_suite_helper:fetch_record(PolicyGroupFixture),
    ?assertEqual(PolicyGroupFixture, Got).

delete_policy_groups(Config) ->
    PolicyGroups = primary_org_policy_group_fixtures(Config) ++ other_org_policy_group_fixtures(Config),
    Results = [ chef_test_suite_helper:delete_record(PolicyGroup) || PolicyGroup <- PolicyGroups ],
    Expected = lists:duplicate(length(PolicyGroups), {ok, 1}),
    ?assertEqual(Expected, Results),
    ListResult = chef_test_suite_helper:list_records(hd(PolicyGroups)),
    ?assertEqual([], ListResult),
    OtherOrgList = chef_test_suite_helper:list_records(lists:last(PolicyGroups)),
    ?assertEqual([], OtherOrgList).

primary_org_policy_group_fixtures(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    [make_policy_group(<<"1">>, OrgId),
     make_policy_group(<<"2">>, OrgId),
     make_policy_group(<<"3">>, OrgId)].

other_org_policy_group_fixtures(Config) ->
    OtherOrgId = proplists:get_value(other_org_id, Config),
    [make_policy_group(<<"4">>, OtherOrgId)].

make_policy_group(Prefix, OrgId) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    Name = <<"policy_group", Prefix/binary>>,
    #oc_chef_policy_group{server_api_version = ?API_MIN_VER,
                          id = Id, authz_id = AzId, org_id = OrgId, name = Name,
                          last_updated_by = chef_test_suite_helper:actor_id()}.

policy_revision_ops(Config) ->
    % setup (needed for FK constraints)
    insert_policy_data(Config),
    insert_policy_group_data(Config),
    % test policy revisions
    insert_policy_revision_data(Config),
    list_policy_revisions_by_name(Config),
    find_policy_revisions(Config),

    policy_group_revision_association_ops(Config),

    delete_policy_revisions(Config),

    % cleanup other fixtures
    delete_policy_groups(Config),
    delete_policies(Config).

policy_group_revision_association_ops(Config) ->
    fetch_policy_via_group_association_not_existing(Config),
    verify_insert_policy_group_association(Config),
    fetch_prereq_objects_when_all_exist(Config),
    verify_insert_policy_group_association_missing_rev(Config),
    fetch_prereq_objects_when_missing_rev(Config),
    verify_insert_policy_group_association_missing_group(Config),
    fetch_prereq_objects_when_missing_policy_and_rev(Config),
    verify_insert_policy_group_association_missing_policy_and_rev(Config),
    fetch_prereq_objects_when_missing_policy_and_rev_and_group(Config),
    verify_insert_policy_group_association_missing_policy_and_rev_and_group(Config),
    insert_policy_group_policy_revision_association(Config),
    fetch_policy_via_group_association(Config),
    update_policy_group_policy_revision_association(Config),
    update_policy_group_policy_revision_association_create_revision(Config),
    ok.

create_pgr_find_query_record(_Config) ->
    Result = oc_chef_policy_group_revision_association:record_for_find(?ORG_ID, <<"policy_name">>, <<"group_name">>),
    Expected = #oc_chef_policy_group_revision_association{
            server_api_version = ?API_MIN_VER,
            org_id = ?ORG_ID,
            policy_revision_name = <<"policy_name">>,
            policy_group_name = <<"group_name">>,
            policy = #oc_chef_policy{org_id = ?ORG_ID, name = <<"policy_name">> },
            policy_group = #oc_chef_policy_group{org_id = ?ORG_ID, name = <<"group_name">> }},
    ?assertEqual(Expected, Result).

pgr_assoc_doesnt_exist(Config) ->
    Prefix = <<"000">>,
    Policy = hd(primary_org_policy_fixtures(Config)),
    Group = hd(primary_org_policy_group_fixtures(Config)),
    Revision = hd(policy_revisions_in_policy1(Config)),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).

pgr_assoc_has_all_deps(Config) ->
    Prefix = <<"111">>,

    Policy = hd(primary_org_policy_fixtures(Config)),
    Group = hd(primary_org_policy_group_fixtures(Config)),
    Revision = hd(policy_revisions_in_policy1(Config)),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).

updated_pgr_assoc_has_all_deps(Config) ->
    BaseAssoc = pgr_assoc_has_all_deps(Config),
    NewRevision = lists:last(policy_revisions_in_policy1(Config)),
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER, revision_id = RevisionId} = NewRevision,
    BaseAssoc#oc_chef_policy_group_revision_association{
        server_api_version = ?API_MIN_VER,
        policy_revision = NewRevision,
        policy_revision_revision_id = RevisionId}.

pgr_assoc_missing_rev(Config) ->
    OrgId = proplists:get_value(org_id, Config),

    Prefix = <<"222">>,

    Policy = hd(primary_org_policy_fixtures(Config)),
    Group = hd(primary_org_policy_group_fixtures(Config)),
    Revision = make_policy_revision(<<"90">>, <<"1">>, OrgId),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).

updated_pgr_assoc_new_policy_rev(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    BaseAssoc = pgr_assoc_has_all_deps(Config),
    NewRevision = make_policy_revision(<<"91">>, <<"1">>, OrgId),
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER, revision_id = RevisionId} = NewRevision,
    BaseAssoc#oc_chef_policy_group_revision_association{
        server_api_version = ?API_MIN_VER,
        policy_revision = NewRevision,
        policy_revision_revision_id = RevisionId}.

pgr_assoc_missing_rev_and_policy(Config) ->
    OrgId = proplists:get_value(org_id, Config),

    Prefix = <<"222">>,

    Policy = make_policy(<<"9">>, OrgId),
    Group = hd(primary_org_policy_group_fixtures(Config)),
    Revision = make_policy_revision(<<"91">>, <<"1">>, OrgId),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).

pgr_assoc_missing_group(Config) ->
    OrgId = proplists:get_value(org_id, Config),

    Prefix = <<"111">>,

    Policy = hd(primary_org_policy_fixtures(Config)),
    Group = make_policy_group(<<"9">>, OrgId),
    Revision = hd(policy_revisions_in_policy1(Config)),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).

pgr_assoc_missing_rev_and_policy_and_group(Config) ->
    OrgId = proplists:get_value(org_id, Config),

    Prefix = <<"222">>,

    Policy = make_policy(<<"9">>, OrgId),
    Group = make_policy_group(<<"9">>, OrgId),
    Revision = make_policy_revision(<<"91">>, <<"1">>, OrgId),
    make_policy_group_association(Config, Prefix, Policy, Group, Revision).


make_policy_group_association(Config, Prefix, Policy,
                              #oc_chef_policy_group{authz_id = GroupAzID} = Group,
                              #oc_chef_policy_revision{policy_authz_id = PolicyAuthzID} = Revision) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    OrgId = proplists:get_value(org_id, Config),
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER, name = PolicyName, revision_id = RevisionId} = Revision,

    #oc_chef_policy_group{server_api_version = ?API_MIN_VER, name = GroupName} = Group,

    #oc_chef_policy_group_revision_association{
                server_api_version = ?API_MIN_VER,
                id = Id,
                org_id = OrgId,
                policy_revision_name = PolicyName,
                policy_revision_revision_id = RevisionId,
                policy_authz_id = PolicyAuthzID,
                policy_group_name = GroupName,
                policy_group_authz_id = GroupAzID,
                last_updated_by = chef_test_suite_helper:actor_id(),

                policy = Policy,
                policy_revision = Revision,
                policy_group = Group
            }.

insert_policy_group_policy_revision_association(Config) ->
    Assoc = pgr_assoc_has_all_deps(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result).

verify_insert_policy_group_association(Config) ->
    Assoc = pgr_assoc_has_all_deps(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult).

fetch_prereq_objects_when_all_exist(Config) ->
    Assoc = pgr_assoc_has_all_deps(Config),
    Context = chef_test_suite_helper:context(),
    Result = oc_chef_policy_group_revision_association:fetch_prereq_objects(Assoc, Context),
    #oc_chef_policy_group_revision_association{server_api_version = ?API_MIN_VER,
                                               policy = ExpectedPolicy,
                                               policy_group = ExpectedPolicyGroup} = Assoc,
    Expected = [{policy, ExpectedPolicy}, {policy_group, ExpectedPolicyGroup}],
    ?assertEqual(Expected, Result).

verify_insert_policy_group_association_missing_rev(Config) ->
    Assoc = pgr_assoc_missing_rev(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult),
    DeleteRevResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_revision),
    ?assertEqual({ok, 1}, DeleteRevResult).

fetch_prereq_objects_when_missing_rev(Config) ->
    Assoc = pgr_assoc_missing_rev(Config),
    Context = chef_test_suite_helper:context(),
    Result = oc_chef_policy_group_revision_association:fetch_prereq_objects(Assoc, Context),
    #oc_chef_policy_group_revision_association{server_api_version = ?API_MIN_VER,
                                               policy = ExpectedPolicy,
                                               policy_group = ExpectedPolicyGroup} = Assoc,
    Expected = [{policy, ExpectedPolicy}, {policy_group, ExpectedPolicyGroup}],
    ?assertEqual(Expected, Result).


verify_insert_policy_group_association_missing_group(Config)->
    Assoc = pgr_assoc_missing_group(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult),
    DeleteGroupResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_group),
    ?assertEqual({ok, 1}, DeleteGroupResult).

%% Note, there is a fk constraint on policy_revisions so you can't have a
%% revision w/ no policy
verify_insert_policy_group_association_missing_policy_and_rev(Config) ->
    Assoc = pgr_assoc_missing_rev_and_policy(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult),
    DeleteRevResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_revision),
    ?assertEqual({ok, 1}, DeleteRevResult),
    DeletePolicyResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy),
    ?assertEqual({ok, 1}, DeletePolicyResult).


fetch_prereq_objects_when_missing_policy_and_rev(Config) ->
    Assoc = pgr_assoc_missing_rev_and_policy(Config),
    Context = chef_test_suite_helper:context(),
    Result = oc_chef_policy_group_revision_association:fetch_prereq_objects(Assoc, Context),
    #oc_chef_policy_group_revision_association{server_api_version = ?API_MIN_VER, policy_group = ExpectedPolicyGroup} = Assoc,
    Expected = [{policy, not_found}, {policy_group, ExpectedPolicyGroup}],
    ?assertEqual(Expected, Result).

verify_insert_policy_group_association_missing_policy_and_rev_and_group(Config) ->
    Assoc = pgr_assoc_missing_rev_and_policy_and_group(Config),
    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:insert_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult),
    DeleteRevResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_revision),
    ?assertEqual({ok, 1}, DeleteRevResult),
    DeletePolicyResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy),
    ?assertEqual({ok, 1}, DeletePolicyResult),
    DeleteGroupResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_group),
    ?assertEqual({ok, 1}, DeleteGroupResult).

fetch_prereq_objects_when_missing_policy_and_rev_and_group(Config) ->
    Assoc = pgr_assoc_missing_rev_and_policy_and_group(Config),
    Context = chef_test_suite_helper:context(),
    Result = oc_chef_policy_group_revision_association:fetch_prereq_objects(Assoc, Context),
    Expected = [{policy, not_found}, {policy_group, not_found}],
    ?assertEqual(Expected, Result).

fetch_policy_via_group_association(Config) ->
    Assoc = pgr_assoc_has_all_deps(Config),
    Context = chef_test_suite_helper:context(),
    Returned = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(Assoc, Context),

    #oc_chef_policy_revision{serialized_object = ExpectedObject} = hd(policy_revisions_in_policy1(Config)),
    assert_pgr_associations_match(Assoc, ExpectedObject, Returned).

fetch_policy_via_group_association_not_existing(Config) ->
    Assoc = pgr_assoc_doesnt_exist(Config),
    Context = chef_test_suite_helper:context(),
    Returned = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(Assoc, Context),
    ?assertEqual(not_found, Returned).

update_policy_group_policy_revision_association(Config) ->
    Assoc = updated_pgr_assoc_has_all_deps(Config),

    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:update_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),

    Returned = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(Assoc, Context),
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER, serialized_object = ExpectedObject} = hd(policy_revisions_in_policy1(Config)),
    assert_pgr_associations_match(Assoc, ExpectedObject, Returned).

update_policy_group_policy_revision_association_create_revision(Config) ->
    Assoc = updated_pgr_assoc_new_policy_rev(Config),

    Context = chef_test_suite_helper:context(),
    Actor = chef_test_suite_helper:actor_id(),
    Result = oc_chef_policy_group_revision_association:update_association(Assoc, Context, Actor),
    ?assertEqual(ok, Result),

    Returned = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(Assoc, Context),
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER, serialized_object = ExpectedObject} = hd(policy_revisions_in_policy1(Config)),
    assert_pgr_associations_match(Assoc, ExpectedObject, Returned),
    % clean up so we don't have to worry about violating unique constraints in
    % other insert tests:
    DeleteResult = oc_chef_policy_group_revision_association:delete_association(Assoc, Context),
    ?assertEqual(1, DeleteResult),
    DeleteRevResult = chef_test_suite_helper:delete_record(Assoc#oc_chef_policy_group_revision_association.policy_revision),
    ?assertEqual({ok, 1}, DeleteRevResult).

% not all fields are returned by the join query so pick them out to compare:
assert_pgr_associations_match(Expected, _ExpectedObject, Actual) ->
    #oc_chef_policy_group_revision_association{
        id = ActualId,
        org_id = ActualOrgId,
        policy_revision_revision_id = ActualPolicyRevisionRevisionId,
        policy_revision_name = ActualPolicyRevisionName,
        policy_authz_id = ActualPolicyAuthzID,
        policy_group_name = ActualPolicyGroupName,
        policy_group_authz_id = ActualPolicyGroupAuthzID,
        serialized_object = ActualObject} = Actual,

    ActualFields = {ActualId, ActualOrgId, ActualPolicyRevisionRevisionId,
                    ActualPolicyRevisionName, ActualPolicyAuthzID, ActualPolicyGroupName,
                    ActualPolicyGroupAuthzID, ActualObject},

    #oc_chef_policy_group_revision_association{
        id = ExpectedId,
        org_id = ExpectedOrgId,
        policy_revision_revision_id = ExpectedPolicyRevisionRevisionId,
        policy_revision_name = ExpectedPolicyRevisionName,
        policy_authz_id = ExpectedPolicyAuthzID,
        policy_group_name = ExpectedPolicyGroupName,
        policy_group_authz_id = ExpectedPolicyGroupAuthzID,
        policy_revision = ExpectedPolicyRevision} = Expected,
    #oc_chef_policy_revision{serialized_object = ExpectedObject} = ExpectedPolicyRevision,


    ExpectedFields = {ExpectedId, ExpectedOrgId, ExpectedPolicyRevisionRevisionId,
                      ExpectedPolicyRevisionName, ExpectedPolicyAuthzID, ExpectedPolicyGroupName,
                      ExpectedPolicyGroupAuthzID, ExpectedObject},

    ?assertEqual(ExpectedFields, ActualFields).

insert_policy_revision_data(Config) ->
    Revisions = policy_revisions_in_policy1(Config) ++
                policy_revisions_in_policy2(Config) ++
                policy_revisions_in_policy4(Config),
    Results = [ chef_test_suite_helper:create_record(Revision) || Revision <- Revisions ],
    Expected = lists:duplicate(length(Revisions), {ok, 1}),
    ?assertEqual(Expected, Results).

list_policy_revisions_by_name(Config) ->
    PolicyRevisionsInPolicy1 = policy_revisions_in_policy1(Config),
    RevisionIDs = [ [ PolicyRevision#oc_chef_policy_revision.revision_id ] || PolicyRevision <- PolicyRevisionsInPolicy1],
    Expected = RevisionIDs,
    Actual = chef_test_suite_helper:list_records(hd(PolicyRevisionsInPolicy1)),
    ?assertEqual(Expected, Actual),
    OrgId = proplists:get_value(org_id, Config),
    PolicyRevisionInOtherOrg = hd(policy_revisions_in_policy4(Config)),
    PolicyRevisionInWrongOrg = PolicyRevisionInOtherOrg#oc_chef_policy_revision{org_id = OrgId},
    Results = chef_test_suite_helper:list_records(PolicyRevisionInWrongOrg),
    ?assertEqual([], Results).

find_policy_revisions(Config) ->
    PolicyToFind = hd(policy_revisions_in_policy1(Config)),
    {ok, CompresedRow} = chef_test_suite_helper:fetch_record(PolicyToFind),
    Got = oc_chef_policy_revision:decompress_record(CompresedRow),
    ?assertEqual(PolicyToFind, Got).

delete_policy_revisions(Config) ->
    Revisions = policy_revisions_in_policy1(Config) ++
                policy_revisions_in_policy2(Config) ++
                policy_revisions_in_policy4(Config),
    Results = [ chef_test_suite_helper:delete_record(PolicyRevision) || PolicyRevision <- Revisions ],
    Expected = lists:duplicate(length(Revisions), {ok, 1}),
    ?assertEqual(Expected, Results),
    ListResult = chef_test_suite_helper:list_records(hd(Revisions)),
    ?assertEqual([], ListResult),
    OtherOrgList = chef_test_suite_helper:list_records(lists:last(Revisions)),
    ?assertEqual([], OtherOrgList).

policy_revisions_in_policy1(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    [make_policy_revision(<<"11">>, <<"1">>, OrgId),
     make_policy_revision(<<"12">>, <<"1">>, OrgId),
     make_policy_revision(<<"13">>, <<"1">>, OrgId)].

policy_revisions_in_policy2(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    [make_policy_revision(<<"21">>, <<"2">>, OrgId),
     make_policy_revision(<<"22">>, <<"2">>, OrgId)].

policy_revisions_in_policy4(Config) ->
    OtherOrgId = proplists:get_value(other_org_id, Config),
    [make_policy_revision(<<"41">>, <<"4">>, OtherOrgId)].


make_policy_revision(Prefix, PolicyPrefix, OrgId) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    PolicyAzId = chef_test_suite_helper:make_az_id(PolicyPrefix),
    PolicyName = <<"policy", PolicyPrefix/binary>>,
    RevisionId = <<"policy_revision_id", Prefix/binary>>,
    SerializedObject = <<"policy_revision_serialized_object_", Prefix/binary>>,
    #oc_chef_policy_revision{server_api_version = ?API_MIN_VER,
                             id = Id, org_id = OrgId, name = PolicyName,
                             policy_authz_id = PolicyAzId,
                             revision_id = RevisionId,
                             last_updated_by = chef_test_suite_helper:actor_id(),
                             serialized_object = SerializedObject}.

