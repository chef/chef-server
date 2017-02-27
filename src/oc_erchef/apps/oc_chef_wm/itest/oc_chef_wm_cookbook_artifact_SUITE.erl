%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.

-module(oc_chef_wm_cookbook_artifact_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("chef_types.hrl").
-include("oc_chef_types.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, {parse_transform, lager_transform}]).

-define(ORG_AUTHZ_ID, <<"10000000000000000000000000000004">>).
-define(AUTHZ_ID, <<"00000000000000000000000000000005">>).
-define(CLIENT_NAME, <<"test-client">>).
-define(ORG_NAME, <<"cookbook-artifact-test">>).


init_per_suite(Config) ->
    Config2 = setup_helper:base_init_per_suite([{org_name, ?ORG_NAME},
                                                {org_authz_id, ?ORG_AUTHZ_ID},
                                                {authz_id, ?AUTHZ_ID},
                                                {client_name, ?CLIENT_NAME}
                                                | Config]),

    %% we need to create a few checksums in the DB
    %% to be used in our cookbook artifacts
    OrgId = ?config(org_id, Config2),
    ok = chef_sql:mark_checksums_as_uploaded(OrgId,
                                             canonical_example_checksums()),

    Config2.

end_per_suite(Config) ->
    setup_helper:base_end_per_suite(Config).

init_per_testcase(Case, Config) ->
    setup_helper:mock_authz(?AUTHZ_ID),
    init_per_testcase0(Case, Config).

init_per_testcase0(http_delete, Config) ->
    OrgId = ?config(org_id, Config),
    NewChecksum = <<"b804944c17edc107073d6a1f27aa3842">>,
    ok = chef_sql:mark_checksums_as_uploaded(OrgId,
                                             [NewChecksum]),

    mock_chef_s3(true, [{new_checksum, NewChecksum} | Config]);
init_per_testcase0(http_delete_then_fetch_all_cookbook_artifacts, Config) ->
    mock_chef_s3(true, Config);
init_per_testcase0(_, Config) ->
    mock_chef_s3(false, Config).

mock_chef_s3(MockChefS3Delete, Config) ->
    OrgId = ?config(org_id, Config),

    ok = meck:new(chef_s3),
    ok = meck:expect(chef_s3, generate_presigned_url, 5,
                     fun(_, _, _, Checksum, _) ->
                         <<"http://fake.url/for/", Checksum/binary>>
                     end),
    case MockChefS3Delete of
        true ->
            ok = meck:expect(chef_s3, delete_checksums, 2,
                             fun(ArgOrgId, ArgChecksums) ->
                                 ?assertEqual(OrgId, ArgOrgId),
                                 {{ok, ArgChecksums},
                                  {missing, []},
                                  {timeout, []},
                                  {error, []}}
                              end);
        false ->
            ok
    end,
    Config.

end_per_testcase(http_delete_then_fetch_all_cookbook_artifacts, Config) ->
    OrgId = ?config(org_id, Config),
    %% we restore those for subsequent tests
    ok = chef_sql:mark_checksums_as_uploaded(OrgId,
                                             canonical_example_checksums()),
    end_per_testcase(generic, Config);
end_per_testcase(_, Config) ->
    setup_helper:unmock_authz(),
    ok = meck:unload(chef_s3),
    Config.

all() ->
    [
      db_round_trip,
      db_missing_checksum,
      http_round_trip,
      http_get_nonexistant,
      http_post_conflict,
      http_missing_checksum,
      http_delete,
      http_get_cookbook_artifact,
      http_get_cookbook_artifacts,
      http_get_missing_cookbook_artifact,
      http_delete_then_fetch_all_cookbook_artifacts,
      filter_checksums_to_delete
    ].

%% this tests that low level DB ops (create and fetch)
%% work as expected
%% granted, it's not testing the WM module, so maybe it should
%% live somewhere else?
db_round_trip(Config) ->
    Context = ?config(context, Config),

    Name = "db_round_trip_name",
    Identifier = "db_round_trip_identifier",
    Ejson = canonical_example(Name, Identifier),

    %% let's create a cookbook artifact version in the DB
    CBAVRecord = artifact_version_rec_from_json(Config, Ejson),
    ?assertEqual(ok, chef_db:create(CBAVRecord, Context, ?AUTHZ_ID)),

    %% and then fetch it
    RecFromDB = chef_db:fetch(CBAVRecord, Context),

    %% the one from the DB should have an ID
    RecIDFromDB = oc_chef_cookbook_artifact_version:id(RecFromDB),
    ?assert(erlang:is_integer(RecIDFromDB)),

    %% and modulo a few things, it should be equal to what we inserted
    ChecksumsFromDB = RecFromDB#oc_chef_cookbook_artifact_version.checksums,
    ModifiedRecFromDB = RecFromDB#oc_chef_cookbook_artifact_version{id = undefined,
                                                                    server_api_version = ?API_MIN_VER,
                                                                    %% they get reversed when saving
                                                                    checksums = lists:reverse(ChecksumsFromDB)},
    OriginalCreatedAt = CBAVRecord#oc_chef_cookbook_artifact_version.created_at,
    DateTimeCreatedAt = {datetime, ec_date:parse(erlang:binary_to_list(OriginalCreatedAt))},
    ModifiedCBAVRecord = CBAVRecord#oc_chef_cookbook_artifact_version{created_at = DateTimeCreatedAt,
                                                                      created_by = ?AUTHZ_ID},
    ?assertEqual(ModifiedCBAVRecord,
                 ModifiedRecFromDB).

%% this is also a low-level test; it tests that creating
%% an artifact with a missing checksum yields an explicit
%% DB error tuple
db_missing_checksum(Config) ->
    Context = ?config(context, Config),

    CorruptJson = missing_checksum_example("db_missing_checksum_name",
                                           "db_missing_checksum_identifier"),
    CBAVRecord = artifact_version_rec_from_json(Config, CorruptJson),

    ?assertEqual({error, invalid_checksum},
                 chef_db:create(CBAVRecord, Context, ?AUTHZ_ID)).

%% creates and then retrieves a cookbook artifact version
%% using the HTTP endpoint
http_round_trip(Config) ->
    OrgId = ?config(org_id, Config),

    Name = "http_round_trip_name",
    Identifier = "http_round_trip_identifier",

    %% let's create the artifact
    CreateEjson = canonical_example(Name, Identifier),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact_version(Name, Identifier, CreateEjson)),

    %% and then we retrieve it
    {GetCode, GetBody} = http_fetch_cookbook_artifact_version(Name, Identifier),
    ?assertEqual("200", GetCode),

    %% and the only difference with the original JSON that we sent should
    %% be the download URLs on each file
    ExpectedGetBody = chef_cookbook_version:annotate_with_s3_urls(CreateEjson, OrgId, ""),
    ?assertEqual(ok, ej:valid(ExpectedGetBody, GetBody)).

http_get_nonexistant(_Config) ->
    Name = Identifier = "i_dont_exist",
    ?assertMatch({"404", _},
                 http_fetch_cookbook_artifact_version(Name, Identifier)).

http_post_conflict(_Config) ->
    Name = "http_post_conflict_name",
    Identifier = "http_post_conflict_identifier",

    CreateEjson = canonical_example(Name, Identifier),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact_version(Name, Identifier, CreateEjson)),
    ?assertMatch({"409", _},
                 http_create_cookbook_artifact_version(Name, Identifier, CreateEjson)).

http_missing_checksum(_Config) ->
    Name = "http_missing_checksum_name",
    Identifier = "http_missing_checksum_identifier",

    CorruptJson = missing_checksum_example(Name, Identifier),

    ExpectedErrorJson = {[{<<"error">>,
                           [<<"Manifest has a checksum that hasn't been uploaded.">>]}]},
    ?assertEqual({"400", ExpectedErrorJson},
                 http_create_cookbook_artifact_version(Name, Identifier, CorruptJson)).

http_delete(Config) ->
    OrgId = ?config(org_id, Config),
    NewChecksum = ?config(new_checksum, Config),

    Name = "http_delete_name",
    Identifier = "http_delete_identifier",

    %% first, create it, with a different checksum
    %% that no other version references
    CreateEjson = change_first_checksum(Name, Identifier, NewChecksum),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact_version(Name, Identifier, CreateEjson)),

    %% then delete it
    ?assertMatch({"200", _},
                 http_delete_cookbook_artifact_version(Name, Identifier)),
    %% which should have triggered a call to `chef_s3:delete_checksums'
    ?assertEqual(1,
                 meck:num_calls(chef_s3, delete_checksums,
                                [OrgId, [NewChecksum]])),

    %% now both DELETE and GET should return 404
    ?assertMatch({"404", _},
                 http_fetch_cookbook_artifact_version(Name, Identifier)),
    ?assertMatch({"404", _},
                 http_delete_cookbook_artifact_version(Name, Identifier)).

%% Tests the GET BASE_URL/cookbook_artifacts/NAME endpoint
http_get_cookbook_artifact(_Config) ->
    %% first let's create an artifact with a few versions
    Name = "http_get_cookbook_artifact_name",
    BaseIdentifier = "http_get_cookbook_artifact_identifier",
    NumVersions = 3,
    http_create_cookbook_artifact_versions(Name, BaseIdentifier, NumVersions),

    %% now to the actual test
    {RespCode, RespJson} = http_fetch_cookbook_artifact(Name),
    ?assertEqual("200", RespCode),

    NameBin = erlang:iolist_to_binary(Name),
    ArtifactLists = ej:get({NameBin, <<"versions">>}, RespJson),
    ?assertEqual(NumVersions, erlang:length(ArtifactLists)).

%% 404 on GET BASE_URL/cookbook_artifacts/NAME
http_get_missing_cookbook_artifact(_Config) ->
    Name = "http_get_missing_cookbook_artifact_name",
    ?assertMatch({"404", _},
                 http_fetch_cookbook_artifact(Name)).

%% Tests the GET BASE_URL/cookbook_artifacts endpoint
http_get_cookbook_artifacts(_Config) ->
    ?assertMatch({"200", _}, http_fetch_cookbook_artifacts()).

%% Tests that GET BASE_URL/cookbook_artifacts endpoint
%% works as expected when there are none
http_delete_then_fetch_all_cookbook_artifacts(_Config) ->
    %% first, let's delete all the artifacts we've created so
    %% far (which is a good test, too!)
    {"200", {ArtifactList}} = http_fetch_cookbook_artifacts(),
    lists:foreach(
        fun({ArtifactName, ArtifactEjson}) ->
            lists:foreach(
                fun(VersionEjson) ->
                    Identifier = ej:get({<<"identifier">>}, VersionEjson),
                    NameStr = erlang:binary_to_list(ArtifactName),
                    IdentifierStr = erlang:binary_to_list(Identifier),
                    ?assertMatch({"200", _},
                                 http_delete_cookbook_artifact_version(NameStr, IdentifierStr))
                end,
                ej:get({<<"versions">>}, ArtifactEjson)
            )
        end,
        ArtifactList
    ),

    %% now to the actual test
    ?assertEqual({"200", {[]}},
                 http_fetch_cookbook_artifacts()).

%% This tests `oc_chef_cookbook_artifact:filter_checksums_to_delete/2'
%% It's more of a unit test, but it still needs the DB around...
filter_checksums_to_delete(Config) ->
    OrgId = ?config(org_id, Config),

    ReferencedChecksums = canonical_example_checksums(),
    %% let's make sure that they are indeed still referenced
    %% regardless of the tests above
    Name = "filter_checksums_to_delete_name",
    Identifier = "filter_checksums_to_delete_identifier",

    CreateEjson = canonical_example(Name, Identifier),
    ?assertMatch({"201", _},
                 http_create_cookbook_artifact_version(Name, Identifier, CreateEjson)),

    %% then let's select a few of them
    %% (each with probability 80%)
    RandomReferencedChecksums = lists:foldl(
        fun(Candidate, Acc) ->
            case random:uniform(5) =:= 1 of
                true -> Acc;
                false -> [Candidate | Acc]
            end
        end,
        [],
        ReferencedChecksums
    ),
    %% now let's build a random list of 20 checksums
    RandomChecksums = lists:map(
        fun(_) -> chef_object_base:make_guid() end,
        lists:seq(1, 20)
    ),
    %% then let's shuffle em all
    AllChecksums = RandomChecksums ++ RandomReferencedChecksums,
    ChecksumsToDelete = [X || {_, X} <- lists:sort([ {random:uniform(), C} || C <- AllChecksums ])],
    ActualResult = oc_chef_cookbook_artifact:filter_checksums_to_delete(OrgId, ChecksumsToDelete),
    %% and these 2 should be equal modulo the order!
    ?assertEqual(lists:sort(RandomChecksums),
                 lists:sort(ActualResult)).

%%% Helper funs

artifact_version_rec_from_json(Config, Json) ->
    OrgId = ?config(org_id, Config),
    AuthzId = chef_object_base:make_guid(),
    oc_chef_cookbook_artifact_version:new_record(?API_MIN_VER, OrgId, AuthzId, Json).

%% creates N versions for the same artifact,
%% using the canonical example
%% returns the idenfifiers
http_create_cookbook_artifact_versions(Name, BaseIdentifier, N) ->
    Identifiers = [BaseIdentifier ++ "_" ++ erlang:integer_to_list(I)
                   || I <- lists:seq(1, N)],
    [begin
        CreateEjson = canonical_example(Name, Identifier),
        ?assertMatch({"201", _},
                     http_create_cookbook_artifact_version(Name, Identifier, CreateEjson))
     end || Identifier <- Identifiers],
    Identifiers.

http_create_cookbook_artifact_version(Name, Identifier, Ejson) ->
    http_request(put, route_suffix_for(Name, Identifier), Ejson).

http_fetch_cookbook_artifact_version(Name, Identifier) ->
    http_request(get, route_suffix_for(Name, Identifier), null).

http_delete_cookbook_artifact_version(Name, Identifier) ->
    http_request(delete, route_suffix_for(Name, Identifier), null).

http_fetch_cookbook_artifact(Name) ->
    http_request(get, route_suffix_for(Name), null).

http_fetch_cookbook_artifacts() ->
    http_request(get, "", null).

route_suffix_for(Name) ->
    "/" ++ Name.
route_suffix_for(Name, Identifier) ->
    route_suffix_for(Name) ++ "/" ++ Identifier.

http_request(Method, RouteSuffix, JsonOrNull) ->
    {ok, RespCode, _, RespBody} = ibrowse:send_req(url_for(RouteSuffix),
                     [{"x-ops-userid", "test-client"},
                      {"accept", "application/json"},
                      {"content-type", "application/json"}],
                     Method, body(JsonOrNull)),
    {RespCode, chef_json:decode(RespBody)}.

url_for(RouteSuffix) ->
    OrgNameStr = erlang:binary_to_list(?ORG_NAME),
    "http://localhost:8000/organizations/" ++ OrgNameStr
      ++ "/cookbook_artifacts" ++ RouteSuffix.

body(null) -> <<>>;
body(Json) -> chef_json:encode(Json).

-define(COOKBOOK_ARTIFACT_VERSION_CANONICAL_EXAMPLE, <<"
    {
      \"definitions\": [
        {
          \"name\": \"unicorn_config.rb\",
          \"checksum\": \"c92b659171552e896074caa58dada0c2\",
          \"path\": \"definitions/unicorn_config.rb\",
          \"specificity\": \"default\"
        }
      ],
      \"attributes\": [],
      \"files\": [],
      \"providers\": [],
      \"metadata\": {
        \"dependencies\": {\"ruby\": [], \"rubygems\": []},
        \"name\": \"unicorn\",
        \"maintainer_email\": \"ops@chef.io\",
        \"attributes\": {},
        \"license\": \"Apache 2.0\",
        \"suggestions\": {},
        \"platforms\": {},
        \"maintainer\": \"Opscode, Inc\",
        \"long_description\": \"= LICENSE AND AUTHOR:\\n\\nAuthor:: Adam Jacob...\",
        \"recommendations\": {},
        \"version\": \"0.1.2\",
        \"conflicting\": {},
        \"recipes\": {\"unicorn\": \"Installs unicorn rubygem\"},
        \"groupings\": {},
        \"replacing\": {},
        \"description\": \"Installs/Configures unicorn\",
        \"providing\": {}
      },
      \"libraries\": [],
      \"templates\": [
        {
          \"name\": \"unicorn.rb.erb\",
          \"checksum\": \"36a1cc1b225708db96d48026c3f624b2\",
          \"path\": \"templates/default/unicorn.rb.erb\",
          \"specificity\": \"default\"
        }
      ],
      \"resources\": [],
      \"name\": \"unicorn\",
      \"identifier\": \"a_really_awesome_identifier\",
      \"recipes\": [
        {
          \"name\": \"default.rb\",
          \"checksum\": \"ba0dadcbca26710a521e0e3160cc5e20\",
          \"path\": \"recipes/default.rb\",
          \"specificity\": \"default\"
        }
      ],
      \"root_files\": [
        {
          \"name\": \"README.rdoc\",
          \"checksum\": \"d18c630c8a68ffa4852d13214d0525a6\",
          \"path\": \"README.rdoc\",
          \"specificity\": \"default\"
        },
        {
          \"name\": \"metadata.rb\",
          \"checksum\": \"967087a09f48f234028d3aa27a094882\",
          \"path\": \"metadata.rb\",
          \"specificity\": \"default\"
        },
        {
          \"name\": \"metadata.json\",
          \"checksum\": \"45b27c78955f6a738d2d42d88056c57c\",
          \"path\": \"metadata.json\",
          \"specificity\": \"default\"
        }
      ],
      \"chef_type\": \"cookbook_artifact_version\"
    }">>).

canonical_example() ->
    chef_json:decode(?COOKBOOK_ARTIFACT_VERSION_CANONICAL_EXAMPLE).
canonical_example(Name, Identifier) ->
    Renamed = ej:set({<<"name">>}, canonical_example(),
                     erlang:list_to_binary(Name)),
    ej:set({<<"identifier">>}, Renamed, erlang:list_to_binary(Identifier)).

canonical_example_checksums() ->
    chef_cookbook_version:extract_checksums(canonical_example()).

missing_checksum_example(Name, Identifier) ->
    change_first_checksum(Name, Identifier,
                          <<"75b4a4edd63e6a9988d4927599010fb3">>).

change_first_checksum(Name, Identifier, Checksum) ->
    Ejson = canonical_example(Name, Identifier),
    [FirstDefinition | OtherDefinitions] = ej:get({<<"definitions">>}, Ejson),
    NewDefinition = ej:set({<<"checksum">>}, FirstDefinition, Checksum),
    NewDefinitions = [NewDefinition | OtherDefinitions],
    ej:set({<<"definitions">>}, Ejson, NewDefinitions).
