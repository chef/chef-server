%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_policies_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

%%-compile([export_all, {parse_transform, lager_transform}]).
-compile([export_all, nowarn_export_all]).

-define(ORG_ID, <<"10000000000000000000000000000001">>).
-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000003">>).
-define(AUTHZ_ID4, <<"00000000000000000000000000000004">>).
-define(AUTHZ_ID5, <<"00000000000000000000000000000005">>).
-define(AUTHZ_ID6, <<"00000000000000000000000000000006">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(ORG_NAME, <<"org1">>).

-define(ALTERNATE_REV_ID_1, <<"1352e6cedfed34ebc3828762d521742b9fa677b7">>).
-define(ALTERNATE_REV_ID_2, <<"2a50600f68ede3463f0b490a8816e71c605ca997">>).
-define(ALTERNATE_REV_ID_3, <<"3b0c789fa7305a147404364e4b14324814e82925">>).

-define(POLICY_FILE_CANONICAL_EXAMPLE, <<"
    {
      \"revision_id\": \"909c26701e291510eacdc6c06d626b9fa5350d25\",
      \"name\": \"some_policy_name\",
      \"run_list\": [
        \"recipe[policyfile_demo::default]\"
      ],
      \"named_run_lists\": {
        \"update_jenkins\": [
          \"recipe[policyfile_demo::other_recipe]\"
        ]
      },
      \"cookbook_locks\": {
        \"policyfile_demo\": {
          \"version\": \"0.1.0\",
          \"identifier\": \"f04cc40faf628253fe7d9566d66a1733fb1afbe9\",
          \"dotted_decimal_identifier\": \"67638399371010690.23642238397896298.25512023620585\",
          \"source\": \"cookbooks/policyfile_demo\",
          \"cache_key\": null,
          \"scm_info\": {
            \"scm\": \"git\",
            \"remote\": \"git@github.com:danielsdeleo/policyfile-jenkins-demo.git\",
            \"revision\": \"edd40c30c4e0ebb3658abde4620597597d2e9c17\",
            \"working_tree_clean\": false,
            \"published\": false,
            \"synchronized_remote_branches\": [

            ]
          },
          \"source_options\": {
            \"path\": \"cookbooks/policyfile_demo\"
          }
        }
      },
      \"solution_dependencies\": {
        \"Policyfile\": [
          [ \"policyfile_demo\", \">= 0.0.0\" ]
        ],
        \"dependencies\": {
          \"policyfile_demo (0.1.0)\": []
        }
      }
    }">>).

-define(POLICY_FILE_MINIMAL_EXAMPLE, <<"
    {
      \"revision_id\": \"909c26701e291510eacdc6c06d626b9fa5350d25\",
      \"name\": \"some_policy_name\",
      \"run_list\": [
        \"recipe[policyfile_demo::default]\"
      ],
      \"cookbook_locks\": {
        \"policyfile_demo\": {
          \"identifier\": \"f04cc40faf628253fe7d9566d66a1733fb1afbe9\",
          \"version\": \"1.2.3\"
        }
      }
    }
    ">>).

init_per_suite(Config) ->
    setup_helper:base_init_per_suite([{org_name, ?ORG_NAME},
                                   {org_authz_id, ?ORG_AUTHZ_ID},
                                   {authz_id, ?AUTHZ_ID},
                                   {client_name, ?CLIENT_NAME}
                                   | Config]).

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

all() ->
    [functional_tests,

     list_when_no_policies,
     list_when_created_policies,
     create_policy,
     update_policy,
     create_with_bad_name,
     delete_policy,
     fetch_non_existant_policy,
     fetch_existant_policy,
     put_when_non_existant_policy_should_create,
     validate_run_list,
     validate_cookbook_versions,
     validate_identifiers
    ].

init_per_testcase(_, Config) ->
    setup_helper:mock_authz(?AUTHZ_ID),
    delete_all_policy_data(),
    Config.

end_per_testcase(_, _Config) ->
    setup_helper:unmock_authz(),
    ok.

delete_all_policy_data() ->
    nuke_table_data("policies"),
    nuke_table_data("policy_groups").

nuke_table_data(Table) ->
    case sqerl:adhoc_delete(Table, all) of
        {ok, Count} ->
            Count;
        Error ->
            throw(Error)
    end,
    ok.

%% "functional" testing - poke functions individually.

functional_tests(Config) ->
    test_init(Config),
    test_validate_json(Config),
    test_policy_permissions_get_404(Config),
    test_policy_permissions_delete_404(Config),
    test_policy_permissions_put_404_prereqs_exist(Config),
    test_policy_permissions_put_404_prereqs_dont_exist(Config),
    test_policy_permissions_get_when_exists(Config),
    test_policy_permissions_put_when_exists(Config),
    ok.

test_init(_Config) ->
    {OkOrError, _State} = oc_chef_wm_policy_group_policy_rev:init([]),
    ?assertEqual(ok, OkOrError).

test_validate_json(_Config) ->
    Result = oc_chef_wm_policy_group_policy_rev:validate_json(?POLICY_FILE_MINIMAL_EXAMPLE, "some_policy_name"),
    Expected = jiffy:decode(?POLICY_FILE_MINIMAL_EXAMPLE),
    ?assertEqual(Expected, Result),
    Result2 = oc_chef_wm_policy_group_policy_rev:validate_json(?POLICY_FILE_CANONICAL_EXAMPLE, "some_policy_name"),
    Expected2 = jiffy:decode(?POLICY_FILE_CANONICAL_EXAMPLE),
    ?assertEqual(Expected2, Result2),
    ok.

make_query_record_404(Config) ->
    OrgID = proplists:get_value(org_id, Config),
    #oc_chef_policy_group_revision_association{
        org_id = OrgID,
        policy_revision_name = <<"nope">>,
        policy_group_name = <<"nope">>,

        policy = #oc_chef_policy{ name = <<"nope">> },
        policy_group = #oc_chef_policy_group{ name = <<"nope">> }
    }.

test_policy_permissions_get_404(Config) ->
    DbContext = proplists:get_value(context, Config),
    QueryRecord = make_query_record_404(Config),
    % 404s
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('GET', not_found, QueryRecord, DbContext),
    Expected = {halt, 404, {[{<<"error">>, [<<"Cannot load policy nope in policy group nope">>]}]}},
    ?assertEqual(Expected, Result),
    ok.

test_policy_permissions_delete_404(Config) ->
    DbContext = proplists:get_value(context, Config),
    QueryRecord = make_query_record_404(Config),
    % 404s
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('DELETE', not_found, QueryRecord, DbContext),
    Expected = {halt, 404, {[{<<"error">>, [<<"Cannot load policy nope in policy group nope">>]}]}},
    ?assertEqual(Expected, Result),
    ok.

test_policy_permissions_put_404_prereqs_dont_exist(Config) ->
    DbContext = proplists:get_value(context, Config),
    QueryRecord = make_query_record_404(Config),
    % 404s
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('PUT', not_found, QueryRecord, DbContext),
    Expected = [{create_in_container, policy_group}, {create_in_container, policies}],
    ?assertEqual(Expected, Result),
    ok.

make_prereq_records(Config) ->
    OrgID = proplists:get_value(org_id, Config),
    DbContext = proplists:get_value(context, Config),
    PolicyName = <<"some_policy_name">>,
    PolicyGroupName = <<"some_policy_group_name">>,
    PolicyRevisionID = <<"909c26701e291510eacdc6c06d626b9fa5350d25">>,

    PolicyDbId = chef_object_base:make_org_prefix_id(OrgID, PolicyName),
    PolicyGroupDbId = chef_object_base:make_org_prefix_id(OrgID, PolicyGroupName),
    PolicyRevDbId = chef_object_base:make_org_prefix_id(OrgID, PolicyRevisionID),

    Policy = #oc_chef_policy{
            org_id = OrgID,
            id = PolicyDbId,
            name = PolicyName,
            authz_id = ?AUTHZ_ID4
            },
    PolicyGroup = #oc_chef_policy_group{
            org_id = OrgID,
            id = PolicyGroupDbId,
            name = PolicyGroupName,
            authz_id = ?AUTHZ_ID5
            },

    PolicyRevision = #oc_chef_policy_revision{
            id = PolicyRevDbId,
            org_id = OrgID,

            revision_id = PolicyRevisionID,
            name = PolicyName,
            policy_authz_id = ?AUTHZ_ID4,
            serialized_object = ?POLICY_FILE_MINIMAL_EXAMPLE
            },
    ok = chef_db:create(Policy, DbContext, ?AUTHZ_ID),
    ok = chef_db:create(PolicyGroup, DbContext, ?AUTHZ_ID),
    ok = chef_db:create(PolicyRevision, DbContext, ?AUTHZ_ID),
    {Policy, PolicyGroup, PolicyRevision}.

make_query_record_exists(Config) ->
    OrgID = proplists:get_value(org_id, Config),
    DbContext = proplists:get_value(context, Config),
    PolicyName = <<"some_policy_name">>,
    PolicyGroupName = <<"some_policy_group_name">>,
    PolicyRevisionID = <<"909c26701e291510eacdc6c06d626b9fa5350d25">>,

    {Policy, PolicyGroup, PolicyRevision} = make_prereq_records(Config),

    PolicyAssocDbId = chef_object_base:make_org_prefix_id(OrgID, <<PolicyGroupName/binary, PolicyName/binary, PolicyRevisionID/binary>>),

    PolicyRevisionAssoc = #oc_chef_policy_group_revision_association{
            org_id = OrgID,
            id = PolicyAssocDbId,
            policy_group_authz_id = ?AUTHZ_ID5,
            policy_revision_revision_id = PolicyRevisionID,
            policy_revision_name = PolicyName,
            policy_group_name = PolicyGroupName,

            policy = Policy,
            policy_group = PolicyGroup,
            policy_revision = PolicyRevision
            },

    Result = oc_chef_policy_group_revision_association:insert_association(PolicyRevisionAssoc, DbContext, ?AUTHZ_ID),
    ?assertEqual(ok, Result),
    PolicyRevisionAssoc.

test_policy_permissions_put_404_prereqs_exist(Config) ->
    DbContext = proplists:get_value(context, Config),
    {Policy, PolicyGroup, _PolicyRevision} = make_prereq_records(Config),
    QueryAssoc = #oc_chef_policy_group_revision_association{
            policy = Policy,
            policy_group = PolicyGroup
            },
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('PUT', not_found, QueryAssoc, DbContext),
    % These get reversed by list head-tail stuff compared to the asssociation exists case
    Expected = [{policy_group, ?AUTHZ_ID5}, {policy, ?AUTHZ_ID4}],
    ?assertEqual(Expected, Result),
    delete_all_policy_data().

test_policy_permissions_get_when_exists(Config) ->
    DbContext = proplists:get_value(context, Config),
    CreatedAssocWithObjects = make_query_record_exists(Config),
    ReturnedAssoc = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(CreatedAssocWithObjects, DbContext),
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('GET', ReturnedAssoc, CreatedAssocWithObjects, DbContext),
    Expected = [{policy, ?AUTHZ_ID4}, {policy_group, ?AUTHZ_ID5}],
    ?assertEqual(Expected, Result),
    delete_all_policy_data().

test_policy_permissions_put_when_exists(Config) ->
    DbContext = proplists:get_value(context, Config),
    CreatedAssocWithObjects = make_query_record_exists(Config),
    ReturnedAssoc = oc_chef_policy_group_revision_association:find_policy_revision_by_orgid_name_group_name(CreatedAssocWithObjects, DbContext),
    Result = oc_chef_wm_policy_group_policy_rev:policy_permissions_objects('PUT', ReturnedAssoc, CreatedAssocWithObjects, DbContext),
    Expected = [{policy, ?AUTHZ_ID4}, {policy_group, ?AUTHZ_ID5}],
    ?assertEqual(Expected, Result),
    delete_all_policy_data().
%% HTTP round-trip tests

list_when_no_policies(_) ->
    Result = http_list_policies(),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

list_when_created_policies(_) ->
    Policies = ["foo", "bar"],
    [ ?assertMatch({ok, "201", _, _}, http_create_modified_policy(Policy)) || Policy <- Policies ],
    {ok, ResponseCode, _, ResponseBody} = http_list_policies(),
    ?assertEqual("200", ResponseCode),
    Ejson = chef_json:decode(ResponseBody),
    ExpectedBarRevisions = {[{<<"909c26701e291510eacdc6c06d626b9fa5350d25">>, {[]}}]},
    ActualBarRevisions = ej:get({"bar", "revisions"}, Ejson),
    ?assertEqual(ExpectedBarRevisions, ActualBarRevisions),
    ExpectedFooRevisions = {[{<<"909c26701e291510eacdc6c06d626b9fa5350d25">>, {[]}}]},
    ActualFooRevisions = ej:get({"foo", "revisions"}, Ejson),
    ?assertEqual(ExpectedFooRevisions, ActualFooRevisions).

create_policy(_) ->
	{ok, ResultCode, _, _} = http_create_modified_policy("foo"),
    {ok, _, _, ResponseBody} = http_fetch_policy("foo"),
	ExpectedJson = canonical_example_policy_json("foo"),
	?assertEqual("201", ResultCode),
	?assertEqual(ExpectedJson, jiffy:decode(ResponseBody)),
    ok.

update_policy(_) ->
	{ok, CreateResultCode, _, _} = http_create_modified_policy("foo"),
	?assertEqual("201", CreateResultCode),
    {ok, ResultCode, _, ResponseBody} = http_create_modified_policy("foo", ?ALTERNATE_REV_ID_1),
    ?assertEqual("200", ResultCode),
    ExpectedEJSON = canonical_example_policy_json("foo", ?ALTERNATE_REV_ID_1),
    ActualEJSON = jiffy:decode(ResponseBody),
    ?assertEqual(ExpectedEJSON, ActualEJSON),
    {ok, FetchResponseCode, _, FetchResponseBody} = http_fetch_policy("foo"),
	?assertEqual("200", FetchResponseCode),
	?assertEqual(ExpectedEJSON, jiffy:decode(FetchResponseBody)),
    ok.

create_with_bad_name(_) ->
    Result = http_create_modified_policy("name with spaces"),
    ?assertMatch({ok, "400", _, _}, Result),
    ok.

delete_policy(_) ->
    http_create_modified_policy("foo"),
    Result = http_delete_policy("foo"),
    ?assertMatch({ok, "200", _, _} , Result),
	?assertMatch({ok, "404", _, _}, http_fetch_policy("foo")),
    ok.

fetch_non_existant_policy(_) ->
    Result = {ok, _, _, ResponseBody} = http_fetch_policy("bar"),
    ?assertMatch({ok, "404", _, ResponseBody} , Result),
    ?assertEqual([<<"Cannot load policy bar in policy group group_name">>], ej:get({"error"}, chef_json:decode(ResponseBody))),
    ok.

fetch_existant_policy(_) ->
    http_create_modified_policy("foo"),
    {ok, ResponseCode, _, ResponseBody} = http_fetch_policy("foo"),
    ?assertEqual("200", ResponseCode),
	ExpectedJson = canonical_example_policy_json("foo"),
	?assertEqual(ExpectedJson, chef_json:decode(ResponseBody)).

put_when_non_existant_policy_should_create(_) ->
	Name = "foo",
    Ejson = minimal_example_policy_json(Name),
    {ok, ResponseCode, _, _ResponseBody} = http_update_policy(Name, Ejson),
	?assertEqual("201", ResponseCode).

validate_run_list(_) ->
    Name = "foo",
    BaseJson = canonical_example_policy_json(Name),
    CorruptJson = ej:set({<<"run_list">>}, BaseJson, <<>>),
    {ok, ResponseCode, _, _} = http_create_policy(Name, CorruptJson),
    ?assertEqual("400", ResponseCode).

validate_cookbook_versions(_) ->
    [ begin
        ?assertEqual("400",
                     http_create_with_modified_cookbook_lock("cookbook_versions",
                                                             <<"dotted_decimal_identifier">>,
                                                             Version))
     end || Version <- [<<"2.8.2.8">>, <<"9223372036854775808.1.1">>]].

validate_identifiers(_) ->
    [ begin
        ?assertEqual("400",
                     http_create_with_modified_cookbook_lock("cookbook_identifiers",
                                                             <<"identifier">>,
                                                             Identifier))
     end || Identifier <- [
                           % Empty is invalid
                           <<"">>,
                           % 255 is the maximum size
                           <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                           <<"bang!is!invalid">>,
                           <<"plus+is+invalid">>,
                           <<"spaces are invalid">>]].

%% Helper fun to post a new policy file with the first cookbook_lock
%% being modified with given key and value
%% Returns the response code
http_create_with_modified_cookbook_lock(PolicyName, KeyName, KeyValue) ->
    BaseJson = canonical_example_policy_json(PolicyName),
    {[{FirstName, FirstData} | Rest]} = ej:get({<<"cookbook_locks">>}, BaseJson),
    NewCookbookData = ej:set({KeyName},
                              FirstData,
                              KeyValue),
    NewCookbooksLocks = {[{FirstName, NewCookbookData} | Rest]},
    NewJson = ej:set({<<"cookbook_locks">>}, BaseJson, NewCookbooksLocks),
    {ok, ResponseCode, _, _} = http_create_policy(PolicyName, NewJson),
    ResponseCode.

http_list_policies() ->
    http_request(get, "/policies", <<>>).

http_fetch_policy(Name) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(get, "/policy_groups/group_name/policies/" ++ UrlEncodedName, <<>>).

http_create_modified_policy(Name) ->
    http_create_policy(Name, canonical_example_policy_json(Name)).

http_create_modified_policy(Name, RevisionID) ->
    http_create_policy(Name, canonical_example_policy_json(Name, RevisionID)).

http_create_policy(Name, Json) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(put, "/policy_groups/group_name/policies/" ++ UrlEncodedName, chef_json:encode(Json)).

http_delete_policy(Name) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(delete, "/policy_groups/group_name/policies/" ++ UrlEncodedName, <<>>).

http_update_policy(Name, Ejson) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(put, "/policy_groups/group_name/policies/" ++ UrlEncodedName, chef_json:encode(Ejson)).

http_request(Method, RouteSuffix, Body) ->
    OrgStr = erlang:binary_to_list(?ORG_NAME),
    Url = "http://localhost:8000/organizations/" ++ OrgStr ++ RouteSuffix,
    ibrowse:send_req(Url,
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}
                     ], Method, Body).

canonical_example_policy_json(Name) ->
    BaseJson = chef_json:decode(?POLICY_FILE_CANONICAL_EXAMPLE),
    ej:set({<<"name">>}, BaseJson, iolist_to_binary(Name)).

canonical_example_policy_json(Name, RevisionID) ->
    BaseJson = chef_json:decode(?POLICY_FILE_CANONICAL_EXAMPLE),
    WithNewName = ej:set({<<"name">>}, BaseJson, iolist_to_binary(Name)),
    ej:set({<<"revision_id">>}, WithNewName, iolist_to_binary(RevisionID)).

minimal_example_policy_json(Name) ->
    BaseJson = chef_json:decode(?POLICY_FILE_MINIMAL_EXAMPLE),
    ej:set({<<"name">>}, BaseJson, list_to_binary(Name)).
