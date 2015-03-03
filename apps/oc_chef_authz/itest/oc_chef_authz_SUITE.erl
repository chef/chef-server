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

-module(oc_chef_authz_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("../../include/oc_chef_authz.hrl").
-include("../../include/oc_chef_types.hrl").

-compile([export_all]).

%% Note: this is also defined in the schema test data
%% TODO: this is NOT the id of the org we make in init_per_suite/1, which is
%% probably confusing... this seems unused, delete it?
-define(ORG_ID, <<"33330000000000000000000000000000">>).

all() -> [fetch_container_sql,
          policy_ops
         ].

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db(LastConfig, "oc_chef_authz_itests"),
    suite_helper:start_server(Config),
    OrgsConfig = chef_test_suite_helper:make_orgs(),
    OrgsConfig ++ Config.

end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, suite_helper:needed_apps()).

fetch_container_sql(_Config) ->
    ContainerName = <<"nodes">>,
    ReqId = <<"test-1-req-id">>,
    Ctx = oc_chef_authz:make_context(ReqId, darklaunch_stub),
    case oc_chef_authz_db:fetch_container_sql(Ctx, ?ORG_ID, ContainerName) of
        #chef_container{name = <<"nodes">>} = C ->
            ct:pal("Found container: ~p", [C]),
            ok;
        Bad ->
            erlang:error({unexpected_result, Bad})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB Statement Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

policy_ops(Config) ->
    insert_policy_data(Config),
    list_policies(Config),
    show_policy(Config),
    delete_policies(Config).

insert_policy_data(Config) ->
    Policies = primary_org_policy_fixtures(Config) ++ other_org_policy_fixtures(Config),
    Results = [ create_record(Policy) || Policy <- Policies ],
    Expected = lists:duplicate(length(Policies), {ok, 1}),
    ?assertEqual(Expected, Results).

list_policies(Config) ->
    ExistingPolicies = primary_org_policy_fixtures(Config),
    Names = [ [ Policy#oc_chef_policy.name ] || Policy <- ExistingPolicies],
    Expected = Names,
    Actual = list_records(hd(ExistingPolicies)),
    ?assertEqual(Expected, Actual).

show_policy(Config) ->
    PolicyFixture = hd(primary_org_policy_fixtures(Config)),
    {ok, Got} = fetch_record(PolicyFixture),
    ?assertEqual(PolicyFixture, Got).

delete_policies(Config) ->
    Policies = primary_org_policy_fixtures(Config),
    Results = [ delete_record(Policy) || Policy <- Policies ],
    Expected = lists:duplicate(length(Policies), {ok, 1}),
    ?assertEqual(Expected, Results),
    ListResult = list_records(hd(Policies)),
    ?assertEqual([], ListResult).


make_policy(Prefix, OrgId) ->
    Id = chef_test_suite_helper:make_id(Prefix),
    AzId = chef_test_suite_helper:make_az_id(Prefix),
    Name = <<"policy", Prefix/binary>>,
    #oc_chef_policy{id = Id, authz_id = AzId, org_id = OrgId, name = Name,
                    last_updated_by = chef_test_suite_helper:actor_id()}.

primary_org_policy_fixtures(Config) ->
    OrgId = proplists:get_value(org_id, Config),
    [make_policy(<<"1">>, OrgId),
     make_policy(<<"2">>, OrgId),
     make_policy(<<"3">>, OrgId)].

other_org_policy_fixtures(Config) ->
    OtherOrgId = proplists:get_value(other_org_id, Config),
    [make_policy(<<"4">>, OtherOrgId)].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO: This is all copy-pasta from chef_db/itest/itest_util.erl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_record(Record) ->
    Query = chef_object:create_query(Record),
    FlattenedRecord = chef_object:flatten(Record),
    chef_sql:create_object(Query, FlattenedRecord).

fetch_record(Record) ->
    chef_sql:fetch_object(
      chef_object:fields_for_fetch(Record),
      element(1, Record),
      chef_object:find_query(Record),
      chef_object:record_fields(Record)
     ).

update_record(Record) ->
    chef_sql:do_update(chef_object:update_query(Record), chef_object:fields_for_update(Record)).

delete_record(Record) ->
    ct:pal("delete record arg: ~p", [Record]),
    Query = chef_object:delete_query(Record),
    Id = chef_object:id(Record),

    ct:pal("delete object args: ~p~p~n", [{query, Query},{id, Id}]),

    chef_sql:delete_object(Query, Id).

list_records(Record) ->
    chef_sql:fetch_object_names(Record).

%% END COPYPASTA

