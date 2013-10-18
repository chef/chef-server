%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <>
%%% @copyright (C) 2013, Oliver Ferrigni
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2013 by Oliver Ferrigni <>
%%%-------------------------------------------------------------------
-module(oc_chef_group_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("oc_chef_authz/include/oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CTX, {context, <<"req_id">>, otto, undefined}).
-define(AUTHZ, <<"00000000000000000000000000000001">>).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    [{tables, ["groups"]} | suite_helper:init(Config)].

end_per_suite(Config) ->
    suite_helper:stop(Config).

init_per_testcase(_, Config) ->
    suite_helper:cleanup_test_case(Config),
    Config.

 
all() -> 
    [create_should_create_new_group,
     fetch_group_sql, list_should_return_empty_list_when_no_groups,
     list_should_return_group_name_when_group_exists,
     delete_should_delete_group_form_org
    ].

create_should_create_new_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"group_name">>,
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    create_group(OrgId, GroupName),
    CreatedGroup = fetch_group(OrgId, GroupName),
    ?assertMatch(#oc_chef_group{org_id = OrgId, name = GroupName}, CreatedGroup),
    ok.
    

list_should_return_empty_list_when_no_groups(_Config) ->
    OrgId = <<"77770000000000000000000000000001">>,
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.

list_should_return_group_name_when_group_exists(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    GroupName = <<"admins">>,
    create_group(OrgId, GroupName),
    ?assertEqual([GroupName], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.

fetch_group_sql(_Config) ->
    OrgId = <<"77770000000000000000000000000000">>,
    Name = <<"admins">>,
    create_group(OrgId, Name),
    ?assertMatch(
       #oc_chef_group{
          org_id = OrgId,
          authz_id = ?AUTHZ,
                      name = Name},
       chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = Name})),
    ok.

delete_should_delete_group_form_org(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"delete_group_name">>,
    create_group(OrgId, GroupName),
    ?assertEqual([GroupName], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    Group = fetch_group(OrgId, GroupName),

    chef_db:delete(Group, ?CTX),
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.
    


create_group(OrgId, GroupName) ->
    ?assertEqual(ok, chef_db:create(oc_chef_group:new_record(OrgId, ?AUTHZ, {[{<<"groupname">>,GroupName}]}), ?CTX, ?AUTHZ)).

fetch_group(OrgId, GroupName) ->
    chef_db:fetch(#oc_chef_group{org_id = OrgId, name = GroupName}, ?CTX).

