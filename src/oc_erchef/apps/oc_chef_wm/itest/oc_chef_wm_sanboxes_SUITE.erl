%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_wm_sanboxes_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000003">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000004">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(ORG_NAME, <<"org-sanbox-test">>).

-define(CHECKSUMS, [<<"385ea5490c86570c7de71070bce9384a">>,
                    <<"4d5cd68c38a0a5e4078ac247f75e3ab9">>]).

init_per_suite(Config) ->
    setup_helper:base_init_per_suite([{org_name, ?ORG_NAME},
                                      {org_authz_id, ?ORG_AUTHZ_ID},
                                      {authz_id, ?AUTHZ_ID},
                                      {client_name, ?CLIENT_NAME}
                                      | Config]).

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

init_per_testcase(TestName, Config) ->
    %% we don't have bookshelf around, and don't want to pollute
    %% some S3 bucket, so let's mock that part out
    %% and as usual, mocks need to happen in the same thread as the
    %% actual test
    ok = meck:new(chef_s3),
    ok = meck:expect(chef_s3, generate_presigned_url, 5,
                     fun(_, _, _, Checksum, _) ->
                         url_for_checksum(Checksum)
                     end),
    ok = meck:expect(chef_s3, check_checksums, 2,
                     fun(_, _) ->
                         case TestName =:= commit_sandbox_before_uploading of
                             true ->
                                 [First | Others] = ?CHECKSUMS,
                                 {{ok, Others},
                                  {missing, [First]},
                                  {timeout, []},
                                  {error, []}};
                             false ->
                                 {{ok, ?CHECKSUMS},
                                  {missing, []},
                                  {timeout, []},
                                  {error, []}}
                         end
                     end),
    Config.

end_per_testcase(_, Config) ->
    ok = meck:unload(chef_s3),
    Config.

all() ->
    [
        commit_non_existing_sandbox,
        create_sandbox,
        commit_sandbox_before_uploading,
        commit_sandbox
    ].

commit_non_existing_sandbox(_) ->
    NonExistingId = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    ?assertMatch({"404", _},
                 http_commit(NonExistingId)).

create_sandbox(_) ->
    Json = {[{<<"checksums">>,
              {[{C, null} || C <- ?CHECKSUMS ]}}]},
    {RespCode, RespJson} = http_request(post, "", Json),
    ?assertEqual("201", RespCode),
    %% check the body is well formed
    SandboxId = ej:get({<<"sandbox_id">>}, RespJson),
    ExpectedJson = {[{<<"sandbox_id">>, SandboxId},
                     {<<"uri">>, erlang:iolist_to_binary([url_for("/"), SandboxId])},
                     {<<"checksums">>,
                      {[{Checksum,
                         {[{<<"url">>, url_for_checksum(Checksum)},
                           {<<"needs_upload">>, true}]}}
                        || Checksum <- ?CHECKSUMS]}}]},
    ?assertEqual(ExpectedJson, RespJson),

    {save_config, [{sandbox_id, SandboxId}]}.

commit_sandbox_before_uploading(Config) ->
    {create_sandbox, SavedConfig} = ?config(saved_config, Config),
    SandboxId = erlang:binary_to_list(?config(sandbox_id, SavedConfig)),

    ?assertMatch({"503", _},
                 http_commit(SandboxId)),

    {save_config, SavedConfig}.

commit_sandbox(Config) ->
    {commit_sandbox_before_uploading, SavedConfig} = ?config(saved_config, Config),
    SandboxId = erlang:binary_to_list(?config(sandbox_id, SavedConfig)),

    ?assertMatch({"200", _},
                 http_commit(SandboxId)).

http_commit(SandboxId) ->
    http_request(put, "/" ++ SandboxId, {[{<<"is_completed">>, true}]}).

http_request(Method, RouteSuffix, Json) ->
    {ok, RespCode, _, RespBody} = ibrowse:send_req(url_for(RouteSuffix),
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}],
                     Method, chef_json:encode(Json)),
    {RespCode, chef_json:decode(RespBody)}.

url_for(RouteSuffix) ->
    OrgNameStr = erlang:binary_to_list(?ORG_NAME),
    "http://localhost:8000/organizations/" ++ OrgNameStr
      ++ "/sandboxes" ++ RouteSuffix.

url_for_checksum(Checksum) ->
    <<"http://fake.url/for/", Checksum/binary>>.
