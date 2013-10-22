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
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CTX, {context, <<"req_id">>, otto, undefined}).
-define(DEFAULT_HEADERS, []).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    [{tables, ["groups", "clients", "users"]} | suite_helper:init(Config)].

end_per_suite(Config) ->
    suite_helper:stop(Config).

init_per_testcase(_, Config) ->
    suite_helper:cleanup_test_case(Config),
    meck:new(oc_chef_authz_http),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(oc_chef_authz_http),
    Config.
 
all() -> 
    [create_should_create_new_group,
     fetch_group_sql, list_should_return_empty_list_when_no_groups,
     list_should_return_group_name_when_group_exists,
     delete_should_delete_group_form_org,
     update_group_with_client,
     update_group_with_user,
     update_group_with_group,
     update_group_with_groupname,
     delete_client_from_group,
     delete_user_from_group,
     delete_group_from_group,
     update_group_with_clients_users_groups,
     delete_clients_users_groups_from_group,
     fetch_group_with_forward_lookup_clients_users_groups,
     update_group_with_rename
    ].

create_should_create_new_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"group_name">>,
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    create_group(OrgId, GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], GroupName),
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
    AuthzId = suite_helper:make_az_id("admins"),
    expect_get_group(suite_helper:make_az_id(Name), [], [], Name),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = Name}),
    ?assertMatch(
       #oc_chef_group{
          org_id = OrgId,
          authz_id = AuthzId,
          name = Name},
       Group),
    ok.

delete_should_delete_group_form_org(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"delete_group_name">>,
    create_group(OrgId, GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], GroupName),

    ?assertEqual([GroupName], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    Group = fetch_group(OrgId, GroupName),

    chef_db:delete(Group, ?CTX),
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    ok.
fetch_group_with_forward_lookup_clients_users_groups(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    insert_user(<<"user1">>),
    insert_user(<<"user2">>),
    insert_client(OrgId, <<"client1">>),
    insert_client(OrgId, <<"client2">>),
    create_group(OrgId, <<"test-group">>),
    create_group(OrgId, <<"group1">>),
    create_group(OrgId, <<"group2">>),
    ClientAuthzIds = [suite_helper:make_az_id(Val) || Val <- ["client1", "client2"]],
    UserAuthzIds = [suite_helper:make_az_id(Val) || Val <- ["user1", "user2"]],
    Actors = ClientAuthzIds ++ UserAuthzIds,
    Groups = [suite_helper:make_az_id(Val) || Val <- ["group1", "group2"]],
    GroupName = <<"test-group">>,
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, Actors, Groups, GroupName),
    {GroupRecord, ClientNames, Usernames, Groupnames} = chef_sql:fetch(#oc_chef_group{org_id = OrgId, name = GroupName, authz_id = RootGroupAuthzId}),
    ?assertMatch(#oc_chef_group{name = GroupName, org_id = OrgId, authz_id = RootGroupAuthzId}, GroupRecord),
    ?assertEqual([<<"client1">>, <<"client2">>], ClientNames),
    ?assertEqual([<<"user1">>, <<"user2">>], Usernames),
    ?assertEqual([<<"group1">>, <<"group2">>], Groupnames),
    ok.

update_group_with_client(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    ClientName = <<"test-client">>,
    create_group(OrgId, GroupName),
    insert_client(OrgId, ClientName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, [], [], GroupName),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName}),
    expect_put_group(RootGroupAuthzId, [suite_helper:make_az_id(ClientName)], [], GroupName),
    expect_get_group(RootGroupAuthzId, [], [], GroupName),
    Result = oc_chef_authz_db:update_group(Group, [ClientName], [],[]),
    ?assertEqual(ok, Result),
    ok.

update_group_with_user(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    UserName = <<"test-user">>,
    create_group(OrgId, GroupName),
    insert_user(UserName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], GroupName),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName}),
    expect_put_group(RootGroupAuthzId, [suite_helper:make_az_id(UserName)], [], GroupName),
    Result = oc_chef_authz_db:update_group(Group, [], [UserName],[]),
    ?assertEqual(ok, Result),
    ok.

update_group_with_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-added">>,
    create_group(OrgId, GroupName),
    create_group(OrgId, TestGroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], GroupName),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName}),
    expect_put_group(RootGroupAuthzId, [], [suite_helper:make_az_id(TestGroupName)], GroupName),
    Result = oc_chef_authz_db:update_group(Group, [], [], [TestGroupName]),
    ?assertEqual(ok, Result),
    ok.

update_group_with_rename(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-added">>,
    create_group(OrgId, GroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], GroupName),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName}),
    meck:delete(oc_chef_authz_http, request, 5),
    expect_get_group(suite_helper:make_az_id(GroupName), [], [], TestGroupName),
    Result = oc_chef_authz_db:update_group(Group#oc_chef_group{name = TestGroupName}, [], [], []),
    {GroupUpdated, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = TestGroupName}),
    ?assertEqual(Group#oc_chef_group.id, GroupUpdated#oc_chef_group.id),
    ?assertEqual(GroupUpdated#oc_chef_group.name, TestGroupName),
    ?assertEqual(ok, Result),

    ok.
    
delete_client_from_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    ClientName = <<"test-client">>,
    insert_client(OrgId, ClientName),
    create_group(OrgId, GroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [ClientName], [], GroupName),
    {Group, _, _, _ } = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName}),
    expect_delete_group(suite_helper:make_az_id(GroupName), [ClientName],[], GroupName),
    Result = oc_chef_authz_db:update_group(Group, [], [], []),
    ?assertEqual(ok, Result),
    ok.

delete_user_from_group(_Config) ->
    ok.

delete_group_from_group(_Config) ->
    ok.

update_group_with_clients_users_groups(_Config) ->
    ok.

delete_clients_users_groups_from_group(_Config) ->
    ok.

update_group_with_groupname(_Config) ->
    ok.

expect_delete_group(GroupAuthzId, Actors, Groups, GroupName) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, delete, _, _, AzId) ->
                        ActorBasePath = Path ++ "/actors/",
                        GroupBasePath = Path ++ "/groups/",
                        PossiblePaths = convert_to_path(ActorBasePath, Actors) ++ convert_to_path(GroupBasePath, Groups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                              Elem =:= InputPath
                                         end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                    ?assertEqual(AzId, suite_helper:make_az_id(GroupName)),
                    {ok, "200", [], prepare_group_body(Actors, Groups)}
                end).

expect_put_group(GroupAuthzId, Actors, Groups, GroupName) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, put, _, _, AzId) ->
                        ActorBasePath = Path ++ "/actors/",
                        GroupBasePath = Path ++ "/groups/",
                        PossiblePaths = convert_to_path(ActorBasePath, Actors) ++ convert_to_path(GroupBasePath, Groups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                              Elem =:= InputPath
                                         end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                    ?assertEqual(AzId, suite_helper:make_az_id(GroupName)),
                    {ok, "200", [], prepare_group_body(Actors, Groups)}
                end).

expect_get_group(GroupAuthzId, Actors, Groups, GroupName) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, get,  _, _, AzId) ->
                    ?assertEqual(AzId, GroupAuthzId),
                    ?assertEqual(Path, InputPath),
                    {ok, "200", [], prepare_group_body(Actors, Groups)}
                end).

convert_to_path(BasePath, Elements) ->
   [BasePath ++ binary_to_list(Elem) || Elem <- Elements].
    

insert_user(Username) ->
    UserRecord = chef_user_record(Username, suite_helper:make_az_id(Username), true),
    [_| Values] = tuple_to_list(UserRecord),
    FieldNames = record_info(fields, chef_user),
    Input = [{pubkey_version,1} | lists:zip(FieldNames, Values)],
    ?assertEqual({ok, 1}, sqerl:adhoc_insert(users, [Input])).        

insert_client(OrgId, Clientname) ->
    ClientRecord = chef_object:new_record(chef_client,
                                          OrgId,
                                          suite_helper:make_az_id(Clientname),
                                          {[{<<"name">>, Clientname},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),


    ?assertEqual(ok, chef_db:create(ClientRecord, ?CTX, suite_helper:make_az_id("root"))).



prepare_group_body(Actors, Groups) ->
    ejson:encode({[{<<"actors">>, Actors}, {<<"groups">>, Groups}]}).

create_group(OrgId, GroupName) ->
    ?assertEqual(ok, chef_db:create(oc_chef_group:new_record(OrgId, suite_helper:make_az_id(GroupName), {[{<<"groupname">>,GroupName}]}), ?CTX, suite_helper:make_az_id("root"))).

fetch_group(OrgId, GroupName) ->
    {Group, _,_,_} = chef_db:fetch(#oc_chef_group{org_id = OrgId, name = GroupName}, ?CTX),
    Group.

chef_user_record(Username, AzId, Admin) ->
  #chef_user{
    id = AzId,
    authz_id = AzId,
    username = Username,
    email = AzId,
    public_key =
    <<"MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAwxOFcrbsV7bEbqzOvW5u"
	      "W5lyB23qsenlUdIGyRttqzGEaki01s7X+PpYy4BLfmVVmA6A6FCbL38CzzTUFX1a"
	      "p6LYQR2Pb1tYjBjZZMUiVnjEgl12Zd1JF8dsPMj2BgPggx5GaGLvCOsajZ0YCDgW"
	      "WkoO/HAEbztFIx2jdSCyD0ZH0ep4fSGDjmkN+5XurS0dBH8J5qPeJjriA/s/RzUb"
	      "ULjr3gvfg49onHxr/kTKbhc78GBOfKSH1ftECCoWnidadW7/lfKbAZ3xiSjLsIxS"
	      "KxavHMeCuSgyReDZpsFOn2Saie26jvLxWrGyn870yIh36wMvCvWKwUQPnluSnstJ"
	      "xwIDAQAB">>,
    hashed_password = <<"secretHaxorz">>,
    salt = <<"kosher">>,
    hash_type = <<"bcrypt">>,
    last_updated_by = suite_helper:actor_id(),
    created_at = <<"2011-10-1 16:47:46">>,
    updated_at = <<"2011-10-1 16:47:46">>,
    external_authentication_uid = <<"an open id of some kind">>,
    recovery_authentication_enabled = false,
    admin = Admin
  }.
