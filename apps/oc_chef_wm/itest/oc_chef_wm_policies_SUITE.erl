%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_policies_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../../../include/chef_types.hrl").
-include("../../../include/oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000002">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000003">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(ORG_NAME, <<"org1">>).

init_per_suite(LastConfig) ->
    Config = chef_test_db_helper:start_db(LastConfig, "oc_chef_wm_itests"),
    Config2 = setup_helper:start_server(Config),

    OrganizationRecord = chef_object:new_record(oc_chef_organization,
                                                nil,
                                                ?ORG_AUTHZ_ID,
                                                {[{<<"name">>, ?ORG_NAME},
                                                  {<<"full_name">>, ?ORG_NAME}]}),
    Result2 = chef_db:create(OrganizationRecord,
                   #context{reqid = <<"fake-req-id">>},
                   ?AUTHZ_ID),
    io:format("Organization Create Result ~p~n", [Result2]),

    % get the OrgId from the database that was generated during Org object creation
    % so we can associate the client with the org.
    {ok, OrgObject} = chef_sql:fetch_object(chef_object:fields_for_fetch(OrganizationRecord),
                                element(1, OrganizationRecord),
                                chef_object:find_query(OrganizationRecord),
                                chef_object:record_fields(OrganizationRecord)
                               ),
    OrgId = OrgObject#oc_chef_organization.id,

    %% create the test client
    ClientRecord = chef_object:new_record(chef_client,
                                          OrgId,
                                          ?AUTHZ_ID,
                                          {[{<<"name">>, ?CLIENT_NAME},
                                            {<<"validator">>, true},
                                            {<<"admin">>, true},
                                            {<<"public_key">>, <<"stub-pub">>}]}),
    io:format("ClientRecord ~p~n", [ClientRecord]),
    Result = chef_db:create(ClientRecord,
                   #context{reqid = <<"fake-req-id">>},
                   ?AUTHZ_ID),

    io:format("Client Create Result ~p~n", [Result]),
    Config2.

end_per_suite(Config) ->
    chef_test_suite_helper:stop_server(Config, setup_helper:needed_apps()).

all() ->
    [
     list_when_no_policies,
     list_when_created_policies,
     create_policy,
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
    delete_all_policies(),
    Config.

delete_all_policies() ->
    Result = case sqerl:adhoc_delete("policies", all) of
        {ok, Count} ->
            Count;
        Error ->
            throw(Error)
    end,
    lager:info("Delete policies: ~p", [Result]),
    ok.

list_when_no_policies(_) ->
    Result = http_list_policies(),
    ?assertMatch({ok, "200", _, _} , Result),
    ok.

list_when_created_policies(_) ->
    Policies = ["foo", "bar"],
    [ ?assertMatch({ok, "201", _, _}, http_create_policy(Policy)) || Policy <- Policies ],
    {ok, ResponseCode, _, ResponseBody} = http_list_policies(),
    ?assertEqual("200", ResponseCode),
    Ejson = ejson:decode(ResponseBody),
    {PolicyList} = Ejson,
    ?assertEqual( [{list_to_binary(Policy), <<"group_name">>} || Policy <- Policies ],
                  PolicyList).

create_policy(_) ->
	{ok, ResultCode, _, _} = http_create_policy("foo"),
    {ok, _, _, ResponseBody} = http_fetch_policy("foo"),
	ExpectedJson = canonical_example_policy_json("foo"),
	?assertEqual("201", ResultCode),
	?assertEqual(ExpectedJson, jiffy:decode(ResponseBody)),
    ok.

create_with_bad_name(_) ->
    Result = http_create_policy("name with spaces"),
    ?assertMatch({ok, "400", _, _}, Result),
    ok.

delete_policy(_) ->
    http_create_policy("foo"),
    Result = http_delete_policy("foo"),
    ?assertMatch({ok, "200", _, _} , Result),
	?assertMatch({ok, "404", _, _}, http_fetch_policy("foo")),
    ok.

fetch_non_existant_policy(_) ->
    Result = {ok, _, _, ResponseBody} = http_fetch_policy("bar"),
    ?assertMatch({ok, "404", _, ResponseBody} , Result),
    ?assertEqual([<<"Cannot load policy bar">>], ej:get({"error"}, ejson:decode(ResponseBody))),
    ok.

fetch_existant_policy(_) ->
    http_create_policy("foo"),
    {ok, ResponseCode, _, ResponseBody} = http_fetch_policy("foo"),
    ?assertEqual("200", ResponseCode),
	ExpectedJson = canonical_example_policy_json("foo"),
    Ejson = ejson:decode(ResponseBody),
	?assertEqual(ExpectedJson, jiffy:decode(ResponseBody)).

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
     end || Identifier <- [<<"f04cc40faf628253fe7d9566d66a1733fb1afbe">>,
                           <<"f04cc40faf628253fe7d9566d66a1733fb1afbe9a">>,
                           <<"G04cc40faf628253fe7d9566d66a1733fb1afbe9">>]].

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
    http_request(get, "", <<>>).

http_fetch_policy(Name) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(get, "/group_name/" ++ UrlEncodedName, <<>>).

http_create_policy(Name, Json) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(post, "/group_name/" ++ UrlEncodedName, ejson:encode(Json)).

http_create_policy(Name) ->
    http_create_policy(Name, canonical_example_policy_json(Name)).

http_delete_policy(Name) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(delete, "/group_name/" ++ UrlEncodedName, <<>>).

http_update_policy(Name, Ejson) ->
    UrlEncodedName = ibrowse_lib:url_encode(Name),
    http_request(put, "/group_name/" ++ UrlEncodedName, ejson:encode(Ejson)).

http_request(Method, RouteSuffix, Body) ->
    Url = "http://localhost:8000/organizations/org1/policies" ++ RouteSuffix,
    ibrowse:send_req(Url,
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}
                     ], Method, Body).

-define(POLICY_FILE_CANONICAL_EXAMPLE, <<"
    {
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
      \"name\": \"some_policy_name\",
      \"run_list\": [
        \"recipe[policyfile_demo::default]\"
      ],
      \"cookbook_locks\": {
        \"policyfile_demo\": {
          \"identifier\": \"f04cc40faf628253fe7d9566d66a1733fb1afbe9\",
          \"dotted_decimal_identifier\": \"67638399371010690.23642238397896298.25512023620585\"
        }
      }
    }
    ">>).

canonical_example_policy_json(Name) ->
    BaseJson = ejson:decode(?POLICY_FILE_CANONICAL_EXAMPLE),
    ej:set({<<"name">>}, BaseJson, list_to_binary(Name)).

minimal_example_policy_json(Name) ->
    BaseJson = ejson:decode(?POLICY_FILE_MINIMAL_EXAMPLE),
    ej:set({<<"name">>}, BaseJson, list_to_binary(Name)).
