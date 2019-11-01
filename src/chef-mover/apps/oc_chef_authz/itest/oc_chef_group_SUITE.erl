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
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include("oc_chef_types.hrl").
-include("chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CTX, {context, <<"req_id">>, otto, undefined}).
-define(DEFAULT_HEADERS, []).
-define(AUTHZ, suite_helper:make_az_id(<<"actorid">>)).

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
     delete_client_from_group,
     delete_user_from_group,
     delete_group_from_group,
     fetch_group_with_forward_lookup_clients_users_groups,
     update_group_with_rename,
     update_group_with_insufficient_privs,
     get_should_cleanup_orphaned_authz
    ].

create_should_create_new_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"group_name">>,
    ?assertEqual([], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    create_group(OrgId, GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], []),
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
    expect_get_group(suite_helper:make_az_id(Name), [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = Name, for_requestor_id = ?AUTHZ}),
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
    expect_get_group(suite_helper:make_az_id(GroupName), [], []),

    ?assertEqual([GroupName], chef_sql:fetch_object_names(#oc_chef_group{org_id = OrgId})),
    Group = fetch_group(OrgId, GroupName),


    expect_delete_group(Group#oc_chef_group.authz_id),

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
    expect_get_group(RootGroupAuthzId, Actors, Groups),
    #oc_chef_group{ clients = ClientNames, users = Usernames, groups = Groupnames} = GroupRecord = chef_sql:fetch(#oc_chef_group{org_id = OrgId, name = GroupName, authz_id = RootGroupAuthzId, for_requestor_id = ?AUTHZ}),
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
    expect_get_group(RootGroupAuthzId, [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    expect_put_group(RootGroupAuthzId, [suite_helper:make_az_id(ClientName)], []),
    Result = chef_db:update(Group#oc_chef_group{clients = [ClientName], users =  [], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

update_group_with_user(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    UserName = <<"test-user">>,
    create_group(OrgId, GroupName),
    insert_user(UserName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    expect_put_group(RootGroupAuthzId, [suite_helper:make_az_id(UserName)], []),
    Result = chef_db:update(Group#oc_chef_group{ clients = [], users = [UserName], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

update_group_with_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-added">>,
    create_group(OrgId, GroupName),
    create_group(OrgId, TestGroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    expect_put_group(RootGroupAuthzId, [], [suite_helper:make_az_id(TestGroupName)]),
    Result = chef_db:update(Group#oc_chef_group{clients = [], users = [], groups =  [TestGroupName]}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

update_group_with_rename(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-added">>,
    create_group(OrgId, GroupName),
    expect_get_group(suite_helper:make_az_id(GroupName), [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),

    expect_get_group(suite_helper:make_az_id(GroupName), [], []),
    Result = chef_db:update(Group#oc_chef_group{name = TestGroupName, clients = [], users = [], groups = []}, ?CTX, ?AUTHZ),
    GroupUpdated = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = TestGroupName, for_requestor_id = ?AUTHZ}),
    ?assertEqual(Group#oc_chef_group.id, GroupUpdated#oc_chef_group.id),
    ?assertEqual(GroupUpdated#oc_chef_group.name, TestGroupName),
    ?assertEqual(ok, Result),

    ok.

update_group_with_insufficient_privs(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    ClientName = <<"test-client">>,
    create_group(OrgId, GroupName),
    insert_client(OrgId, ClientName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, [], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    expect_put_group({error, forbidden}),
    Result = chef_db:update(Group#oc_chef_group{clients = [ClientName], users =  [], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual({error, error_in_bifrost }, Result),
    ok.

delete_client_from_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    ClientName = <<"test-client">>,
    insert_client(OrgId, ClientName),
    create_group(OrgId, GroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, [suite_helper:make_az_id(ClientName)], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    expect_delete_group(RootGroupAuthzId, [suite_helper:make_az_id(ClientName)],[]),
    Result = chef_db:update(Group#oc_chef_group{auth_side_actors = [suite_helper:make_az_id(ClientName)], clients = [], users = [], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

delete_user_from_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    UserName = <<"test-user">>,
    insert_user(UserName),
    create_group(OrgId, GroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, [suite_helper:make_az_id(UserName)], []),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    UserAuthzId = suite_helper:make_az_id(UserName),
    expect_delete_group(RootGroupAuthzId, [UserAuthzId],[]),
    Result = chef_db:update(Group#oc_chef_group{auth_side_actors = [UserAuthzId], users = [], clients = [], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

delete_group_from_group(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-name">>,
    create_group(OrgId, TestGroupName),
    create_group(OrgId, GroupName),
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    expect_get_group(RootGroupAuthzId, [], [suite_helper:make_az_id(TestGroupName)]),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    TestGroupAuthzId = suite_helper:make_az_id(TestGroupName),
    expect_delete_group(RootGroupAuthzId, [],[TestGroupAuthzId]),
    Result = chef_db:update(Group#oc_chef_group{auth_side_groups = [TestGroupAuthzId], clients = [], users = [], groups = []}, ?CTX, ?AUTHZ),
    ?assertEqual(ok, Result),
    ok.

get_should_cleanup_orphaned_authz(_Config) ->
    OrgId = <<"GGGG0000000000000000000000000000">>,
    GroupName = <<"test-group">>,
    TestGroupName = <<"test-group-name">>,
    TestActorName = <<"actor-name">>,
    RootGroupAuthzId = suite_helper:make_az_id(GroupName),
    GroupAzId = [suite_helper:make_az_id(TestGroupName)],
    ActorAzId = [suite_helper:make_az_id(TestActorName)],
    create_group(OrgId, GroupName),
    expect_get_group(RootGroupAuthzId, ActorAzId, GroupAzId, ActorAzId, GroupAzId),
    Group = chef_sql:fetch(#oc_chef_group{org_id = OrgId,name = GroupName, for_requestor_id = ?AUTHZ}),
    ?assertMatch(#oc_chef_group{org_id = OrgId, name = GroupName}, Group),
    {ResultActorSet, ResultGroupSet} = oc_chef_authz_cleanup:get_authz_ids(),
    ?assertEqual(sets:from_list(ActorAzId), ResultActorSet),
    ?assertEqual(sets:from_list(GroupAzId), ResultGroupSet),
    ok.

expect_delete_group(GroupAuthzId) ->
    Path = "groups/" ++ binary_to_list(GroupAuthzId),
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request, fun(InputPath, Method, _, _, _AzId) ->
                                                     ?assertEqual(InputPath, Path),
                                                     ?assertEqual(delete, Method)
                                                         end).

expect_delete_group(GroupAuthzId, Actors, Groups) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, delete, _, _, AzId) ->
                        ActorBasePath = Path ++ "/actors/",
                        GroupBasePath = Path ++ "/groups/",
                        PossiblePaths = convert_to_path(ActorBasePath, Actors) ++ convert_to_path(GroupBasePath, Groups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                              Elem =:= InputPath
                                         end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                    ?assertEqual(AzId, ?AUTHZ),
                    ok
                end).

expect_put_group(GroupAuthzId, Actors, Groups) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, put, _, _, AzId) ->
                        ActorBasePath = Path ++ "/actors/",
                        GroupBasePath = Path ++ "/groups/",
                        PossiblePaths = convert_to_path(ActorBasePath, Actors) ++ convert_to_path(GroupBasePath, Groups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                              Elem =:= InputPath
                                         end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                    ?assertEqual(AzId, ?AUTHZ),
                    ok
                end).

expect_put_group( Response) ->
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request,
                fun(_InputPath, put, _, _, _AzId) ->
                    Response
                end).

expect_get_group(GroupAuthzId, Actors, Groups) ->
    expect_get_group(GroupAuthzId, Actors, Groups, [], []).

expect_get_group(GroupAuthzId, Actors, Groups, DefunctActors, DefunctGroups) ->
    Path = "/groups/" ++ binary_to_list(GroupAuthzId),
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, get,  _, _, AzId) ->
                    ?assertEqual(AzId, ?AUTHZ),
                    ?assertEqual(Path, InputPath),
                        {ok, prepare_group_body(Actors, Groups)};
                   (InputPath, delete, _, _, AzId)  ->
                        ActorBasePath = "actors/",
                        GroupBasePath = "groups/",
                        PossiblePaths = convert_to_path(ActorBasePath, DefunctActors) ++ convert_to_path(GroupBasePath, DefunctGroups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                              Elem =:= InputPath
                                         end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                        ?assertEqual(AzId, ?AUTHZ),
                        ok
                end).

convert_to_path(BasePath, Elements) ->
   [BasePath ++ binary_to_list(Elem) || Elem <- Elements].


insert_user(Username) ->
    UserRecord = chef_user_record(Username, suite_helper:make_az_id(Username), true),
    [_| Values] = tuple_to_list(UserRecord),
    FieldNames = record_info(fields, chef_user),
    Input = lists:zip(FieldNames, Values),
    ?assertEqual({ok, 1}, sqerl:adhoc_insert(users, [Input])).

insert_client(OrgId, Clientname) ->
    ClientRecord = chef_object:new_record(chef_client,
                                          OrgId,
                                          suite_helper:make_az_id(Clientname),
                                          {[{<<"name">>, Clientname},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),


    ?assertEqual(ok, chef_db:create(ClientRecord, ?CTX, suite_helper:make_az_id("root"))),
    ClientRecord.



prepare_group_body(Actors, Groups) ->
    {[{<<"actors">>, Actors}, {<<"groups">>, Groups}]}.

create_group(OrgId, GroupName) ->
    ?assertEqual(ok, chef_db:create(oc_chef_group:new_record(OrgId, suite_helper:make_az_id(GroupName), {[{<<"groupname">>,GroupName}]}), ?CTX, ?AUTHZ)).

fetch_group(OrgId, GroupName) ->
    chef_db:fetch(#oc_chef_group{org_id = OrgId, name = GroupName, for_requestor_id = ?AUTHZ}, ?CTX).


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
    pubkey_version = 1,
    hashed_password = <<"secretHaxorz">>,
    salt = <<"kosher">>,
    hash_type = <<"bcrypt">>,
    last_updated_by = ?AUTHZ,
    created_at = <<"2011-10-1 16:47:46">>,
    updated_at = <<"2011-10-1 16:47:46">>,
    external_authentication_uid = <<"an open id of some kind">>,
    recovery_authentication_enabled = false,
    admin = Admin
  }.
