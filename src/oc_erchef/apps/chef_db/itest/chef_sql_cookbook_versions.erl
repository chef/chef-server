-module(chef_sql_cookbook_versions).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

%%%======================================================================
%%% COOKBOOKS
%%%======================================================================

insert_cookbook_data() ->
    {AuthzId, OrgId, Name} = itest_cookbook_util:make_cookbook(<<"insert_cookbook">>),
    CbStub = #chef_cookbook_version{authz_id=AuthzId,
                                    org_id=OrgId,
                                    name=Name},
    ?assertEqual(false, chef_sql:cookbook_exists(OrgId, Name)),
    Results = chef_sql:create_cookbook_if_needed(CbStub),
    ?assertEqual(true, chef_sql:cookbook_exists(OrgId, Name)),
    ?assertEqual(ok, Results).

fetch_cookbook_authz() ->
    {AuthzId, OrgId, Name} = itest_cookbook_util:make_cookbook(<<"fetch_cookbook_authz">>),
    CbStub = #chef_cookbook_version{authz_id=AuthzId,
                                    org_id=OrgId,
                                    name=Name},
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),
    Results = chef_sql:create_cookbook_if_needed(CbStub),
    ?assertEqual(ok, Results),

    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),
    ?assertEqual(AuthzId, Got).

%%%======================================================================
%%% COOKBOOK VERSIONS
%%%======================================================================

%% covers both insert new cookbook_version and insert new version of cookbook_version
insert_cookbook_version_data() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"insert">>),
    CookbookVersions = itest_cookbook_util:cookbook_version_list(Cookbook),
    Expected = lists:duplicate(length(CookbookVersions), {ok, 1}),
    Results = [chef_sql:create_cookbook_version(CookbookVersion) || CookbookVersion <- CookbookVersions ],
    ?assertEqual(Expected, Results).

insert_cbv_null_id() ->
    {AuthzId, OrgId, Name} = itest_cookbook_util:make_cookbook(<<"insert_invalid">>),
    %% interesting line is first one - 'id = null'
    CookbookVersion = #chef_cookbook_version{id = null,
                                             authz_id=AuthzId,
                                             org_id=OrgId,
                                             name=Name,
                                             major=0, minor=0, patch=0, frozen=false,
                                             meta_attributes= <<"">>,
                                             meta_deps= <<"">>,
                                             meta_long_desc= <<"">>,
                                             metadata= <<"">>,
                                             last_updated_by= chef_test_suite_helper:actor_id(),
                                             created_at= {datetime, {{2011,10,1},{16,47,46}}},
                                             updated_at= {datetime, {{2011,10,1},{16,47,46}}},
                                             serialized_object= <<"">>,
                                             checksums = [] },

    {error, Reason} = chef_sql:create_cookbook_version(CookbookVersion),
    Reason.

insert_cbv_with_unknown_checksums() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"insert_unknown">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001insert_unknown">>, 0, Cookbook),
    CookbookVersion1 = CookbookVersion#chef_cookbook_version{checksums= [ <<"cksum1">> ]},
    ?assertEqual({error, invalid_checksum}, chef_sql:create_cookbook_version(CookbookVersion1)).

insert_cbv_with_frozen() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"insert_frozen">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001insert_frozen">>, 0, Cookbook),
    CookbookVersion1 = CookbookVersion#chef_cookbook_version{frozen = true },
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion1)).

fetch_cookbook_version_not_exist() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"not_exist">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001not_exist">>, 0, Cookbook),

    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           itest_cookbook_util:version_tuple(CookbookVersion)}),
    ?assertEqual(not_found, Got).

fetch_cookbook_version_no_checksums() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"fetch">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001fetch">>, 0, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           itest_cookbook_util:version_tuple(CookbookVersion)}),
    ?assertEqual(CookbookVersion, Got).

fetch_cookbook_version_checksums() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"fetch_checksums">>),
    CookbookVersion0 = itest_cookbook_util:make_cookbook_version(<<"001fetch_checksums">>, 0, Cookbook),
    Checksums = [ chef_test_suite_helper:make_id(<<"checksum1">>),
                  chef_test_suite_helper:make_id(<<"checksum2">>)],
    CookbookVersion = CookbookVersion0#chef_cookbook_version{checksums=Checksums},
    ok = chef_sql:mark_checksums_as_uploaded(CookbookVersion#chef_cookbook_version.org_id,
                                             Checksums),

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           itest_cookbook_util:version_tuple(CookbookVersion)}),
    % We don't know the order the checksums will come back as
    NormalizedGotChecksums = lists:sort(Got#chef_cookbook_version.checksums),
    NormalizedGot = Got#chef_cookbook_version{checksums = NormalizedGotChecksums},
    ?assertEqual(CookbookVersion, NormalizedGot).

fetch_cookbook_version_different_version() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"fetch_different">>),
    CookbookVersion0 = itest_cookbook_util:make_cookbook_version(<<"001fetch_different">>, 0, Cookbook),

    CookbookVersion = CookbookVersion0#chef_cookbook_version{patch=1},

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion0)),
    Expected = {cookbook_exists, CookbookVersion0#chef_cookbook_version.authz_id},
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           itest_cookbook_util:version_tuple(CookbookVersion)}),
    ?assertEqual(Expected, Got).

fetch_cookbook_versions() ->
    OrgId = chef_test_suite_helper:the_org_id(),
    {ok, Versions} = chef_sql:fetch_cookbook_versions(OrgId),
    %% FIXME Hard to test the exact return value given we don't have
    %% a clean state.  For now check the shape of the returned
    %% structure
    [ begin
          [Name, {Major, Minor, Patch}] = Row,
          ?assertMatch( X when is_binary(X), Name),
          ?assertMatch( X when is_integer(X) andalso X >= 0 , Major),
          ?assertMatch( X when is_integer(X) andalso X >= 0, Minor),
          ?assertMatch( X when is_integer(X) andalso X >= 0, Patch)
      end || Row <- Versions].

fetch_cookbook_versions_single_cookbook_no_versions() ->
    OrgId = chef_test_suite_helper:the_org_id(),
    {ok, Versions} = chef_sql:fetch_cookbook_versions(OrgId, <<"does_not_exist">>),
    ?assertEqual([], Versions).

fetch_cookbook_versions_single_cookbook_with_versions() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"fetch_versions_of_me">>),
    CookbookVersion0 = itest_cookbook_util:make_cookbook_version(<<"000fetch_versions_of_me">>, 0, Cookbook),
    CookbookVersion1 = itest_cookbook_util:make_cookbook_version(<<"001fetch_versions_of_me">>, 1, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion0)),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion1)),
    {ok, Got} = chef_sql:fetch_cookbook_versions(CookbookVersion0#chef_cookbook_version.org_id,
                                                 CookbookVersion0#chef_cookbook_version.name),
    Expected = [[CookbookVersion1#chef_cookbook_version.name, itest_cookbook_util:version_tuple(1)],
                [CookbookVersion0#chef_cookbook_version.name, itest_cookbook_util:version_tuple(0)]],
    ?assertEqual(Expected, Got).


%% @doc This tests pulling the latest version of a cookbook using the _latest endpoint
fetch_latest_cookbook_version() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"the_latest">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001the_latest">>, 0, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    Expected = CookbookVersion,
    Got = chef_sql:fetch_latest_cookbook_version(CookbookVersion#chef_cookbook_version.org_id, CookbookVersion#chef_cookbook_version.name),
    ?assertEqual(Expected, Got).

%% @doc This tests pulling the latest version of a cookbook using the _latest endpoint with two of the same cookbook
%% with different versions in the system
fetch_latest_cookbook_version_different_versions() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"the_old">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001the_old">>, 0, Cookbook),
                                                % First param is the primary id of the cookbook - different for every cookbook and cookbook version
    CookbookVersionToo = itest_cookbook_util:make_cookbook_version(<<"002the_old">>, 1, Cookbook),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion)),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersionToo)),
    Expected = CookbookVersionToo,
    Got = chef_sql:fetch_latest_cookbook_version(CookbookVersion#chef_cookbook_version.org_id, CookbookVersion#chef_cookbook_version.name),
    ?assertEqual(Expected, Got).

%% @doc This tests we can update with checksums for a cookbook version.
% Checksums must be unique when we mark it as uploaded
update_cookbook_version_checksums() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_01">>, 1, Cookbook),
    Checksums = [ chef_test_suite_helper:make_id(<<"1checksum2">>),
                  chef_test_suite_helper:make_id(<<"1checksum1">>) ],
    update_cookbook_version_checksums(ExistingVersion, Checksums, Checksums).

update_cookbook_version_checksums_with_deletions() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_02">>, 2, Cookbook),
    ExistingChecksums = [ chef_test_suite_helper:make_id(<<"2checksum2_2">>), chef_test_suite_helper:make_id(<<"2checksum1">>), chef_test_suite_helper:make_id(<<"2checksum3">>) ],
    UpdatedChecksums = [ chef_test_suite_helper:make_id(<<"2checksum1">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).

update_cookbook_version_checksums_with_additions() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_03">>, 3, Cookbook),
    ExistingChecksums = [ chef_test_suite_helper:make_id(<<"3checksum2">>) ],
    UpdatedChecksums = [ chef_test_suite_helper:make_id(<<"3checksum1">>), chef_test_suite_helper:make_id(<<"3checksum2">>), chef_test_suite_helper:make_id(<<"3checksum0">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).

update_cookbook_version_checksums_with_deletions_and_additions() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_04">>, 4, Cookbook),
    ExistingChecksums = [ chef_test_suite_helper:make_id(<<"4checksum2">>), chef_test_suite_helper:make_id(<<"4checksum1">>), chef_test_suite_helper:make_id(<<"4checksums6">>) ],
    UpdatedChecksums = [ chef_test_suite_helper:make_id(<<"4checksums6">>), chef_test_suite_helper:make_id(<<"4checksum5">>), chef_test_suite_helper:make_id(<<"4checksum0">>) ],
    update_cookbook_version_checksums(ExistingVersion, ExistingChecksums, UpdatedChecksums).


update_cookbook_version_checksums(#chef_cookbook_version{} = ExistingVersion, ExistingChecksums, UpdatedChecksums)
    when is_list(ExistingChecksums) andalso is_list(UpdatedChecksums) ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    NormalizedExistingChecksums = lists:sort(ExistingChecksums),
    NormalizedUpdatedChecksums  = lists:sort(UpdatedChecksums),
    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = NormalizedUpdatedChecksums },

    % Assume new checksums have been marked as uploaded
    UploadedChecksums = lists:usort(lists:append(ExistingChecksums, UpdatedChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, UploadedChecksums)),

    % Create a new cookbook version
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(ExistingVersion)}),
    ?assertEqual(ExistingVersion#chef_cookbook_version{checksums = []}, Existing#chef_cookbook_version{checksums = []}),
    ?assertEqual(NormalizedExistingChecksums, lists:sort(Existing#chef_cookbook_version.checksums)),

    %% compute some checksums addition/deletion sets for comparison
    ExistingChecksumsSet = sets:from_list(ExistingChecksums),
    UpdatedChecksumsSet  = sets:from_list(UpdatedChecksums),
    Deletions = sets:to_list(sets:subtract(ExistingChecksumsSet, UpdatedChecksumsSet)),
    Additions = sets:to_list(sets:subtract(UpdatedChecksumsSet, ExistingChecksumsSet)),

    #chef_db_cb_version_update{
        added_checksums=AddedChecksums,
        deleted_checksums=DeletedChecksums} = chef_sql:update_cookbook_version(UpdatedVersion),

    ?assertEqual(lists:sort(Additions), lists:sort(AddedChecksums)),
    ?assertEqual(lists:sort(Deletions), lists:sort(DeletedChecksums)),

    Updated = chef_sql:fetch_cookbook_version(UpdatedVersion#chef_cookbook_version.org_id,
                                               {UpdatedVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(UpdatedVersion)}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version{checksums = []}, Updated#chef_cookbook_version{checksums = []}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version.meta_long_desc, Updated#chef_cookbook_version.meta_long_desc),

    ?assertEqual(NormalizedUpdatedChecksums, lists:sort(Updated#chef_cookbook_version.checksums)).

update_cookbook_version_checksums_with_missing_checksums() ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_05">>, 5, Cookbook),
    MissingChecksum = chef_test_suite_helper:make_id(<<"5missing1">>),
    ExistingChecksums = [ chef_test_suite_helper:make_id(<<"5checksum2">>), chef_test_suite_helper:make_id(<<"5checksum1">>), chef_test_suite_helper:make_id(<<"5checksums6">>) ],
    UpdatedChecksums = [ chef_test_suite_helper:make_id(<<"5checksums2">>), MissingChecksum ],

    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = UpdatedChecksums },

    % Assume existing checksum have been uploaded
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, ExistingChecksums)),

    % Create a new cookbook version
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(ExistingVersion)}),
    ?assertEqual(ExistingVersion#chef_cookbook_version{checksums = []}, Existing#chef_cookbook_version{checksums = []}),
    ?assertEqual(lists:sort(ExistingChecksums), lists:sort(Existing#chef_cookbook_version.checksums)),

    ?assertEqual({error, {checksum_missing, MissingChecksum}}, chef_sql:update_cookbook_version(UpdatedVersion)).

update_cookbook_version_checksums_with_shared_checksums() ->
    Cookbook = itest_cookbook_util:make_cookbook(<<"update_checksums">>),
    OlderVersion    = itest_cookbook_util:make_cookbook_version(<<"update_version_06">>, 6, Cookbook),
    ExistingVersion = itest_cookbook_util:make_cookbook_version(<<"update_version_07">>, 7, Cookbook),

    SharedChecksum = chef_test_suite_helper:make_id(<<"shared-1">>),
    OlderChecksums = [ chef_test_suite_helper:make_id(<<"v6-1">>), chef_test_suite_helper:make_id(<<"v6-2">>), SharedChecksum ],
    ExistingChecksums = [ chef_test_suite_helper:make_id(<<"v7-1">>), chef_test_suite_helper:make_id(<<"v7-2">>), SharedChecksum ],
    UpdatedChecksums = [ chef_test_suite_helper:make_id(<<"v7-3">>), chef_test_suite_helper:make_id(<<"v7-2">>) ],

    UpdatedVersion = ExistingVersion#chef_cookbook_version{ meta_long_desc = <<"Updated Description">>, checksums = UpdatedChecksums },

    % Assume existing checksum have been uploaded
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, OlderChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, ExistingChecksums)),
    ?assertEqual(ok, chef_sql:mark_checksums_as_uploaded(UpdatedVersion#chef_cookbook_version.org_id, UpdatedChecksums)),

    % Create two cookbook version with one shared checksum
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(OlderVersion#chef_cookbook_version{ checksums = OlderChecksums })),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(ExistingVersion#chef_cookbook_version{ checksums = ExistingChecksums })),

    % A cookbook version should already exist in the datastore
    Existing = chef_sql:fetch_cookbook_version(ExistingVersion#chef_cookbook_version.org_id,
                                               {ExistingVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(ExistingVersion)}),
    ?assertEqual(lists:sort(ExistingChecksums), lists:sort(Existing#chef_cookbook_version.checksums)),

    % Update a version which causes the shared checksum to be deleted
    #chef_db_cb_version_update{
        added_checksums=AddedChecksums,
        deleted_checksums=DeletedChecksums} = chef_sql:update_cookbook_version(UpdatedVersion),
    ?assertEqual([chef_test_suite_helper:make_id(<<"v7-3">>)] , AddedChecksums),
    ?assertEqual([chef_test_suite_helper:make_id(<<"v7-1">>)], DeletedChecksums),

    Updated = chef_sql:fetch_cookbook_version(UpdatedVersion#chef_cookbook_version.org_id,
                                               {UpdatedVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(UpdatedVersion)}),
    % Prove that checksums have been updated
    ?assertEqual(UpdatedVersion#chef_cookbook_version{checksums = []}, Updated#chef_cookbook_version{checksums = []}),
    ?assertEqual(UpdatedVersion#chef_cookbook_version.meta_long_desc, Updated#chef_cookbook_version.meta_long_desc),
    ?assertEqual(lists:sort(UpdatedChecksums), lists:sort(Updated#chef_cookbook_version.checksums)),

    % Prove that the shared checksum have not been overwritten
    Older = chef_sql:fetch_cookbook_version(OlderVersion#chef_cookbook_version.org_id,
                                               {OlderVersion#chef_cookbook_version.name,
                                                itest_cookbook_util:version_tuple(OlderVersion)}),
    ?assertEqual(lists:sort(OlderChecksums), lists:sort(Older#chef_cookbook_version.checksums)).


%% @doc This tests we can delete with checksums for a cookbook version
delete_cookbook_version_checksums() ->
    %% This CB exists since we created it in an earlier test - we retrieve it to
    %% get the full CB including checksums
    Cookbook = itest_cookbook_util:make_cookbook(<<"fetch_checksums">>),
    CookbookVersion = itest_cookbook_util:make_cookbook_version(<<"001fetch_checksums">>, 0, Cookbook),
    Got = chef_sql:fetch_cookbook_version(CookbookVersion#chef_cookbook_version.org_id,
                                          {CookbookVersion#chef_cookbook_version.name,
                                           itest_cookbook_util:version_tuple(CookbookVersion)}),

    %% Verify all checksums exist
    [Checksum1, Checksum2] = Got#chef_cookbook_version.checksums,
    ?assertEqual(true, itest_cookbook_util:checksum_exists(Got#chef_cookbook_version.org_id, Checksum1)),
    ?assertEqual(true, itest_cookbook_util:checksum_exists(Got#chef_cookbook_version.org_id, Checksum2)),

    %% We should have gotten back a list of deleted checksums
    #chef_db_cb_version_delete{
        cookbook_delete=CookbookDeleted,
        deleted_checksums=DeletedChecksums} = chef_sql:delete_cookbook_version(Got),

    ?assertEqual(true, CookbookDeleted), %% Last version of this cookbook, so deleting the cookbook as well
    ?assertEqual(lists:sort(Got#chef_cookbook_version.checksums),
                 lists:sort(DeletedChecksums)),

    ?assertEqual(not_found, chef_sql:fetch_cookbook_version(Got#chef_cookbook_version.org_id,
                                                            {Got#chef_cookbook_version.name,
                                                             itest_cookbook_util:version_tuple(Got)})),

    %% Ensure the checksums don't exist in the checksum table
    ?assertEqual(false, itest_cookbook_util:checksum_exists(Got#chef_cookbook_version.org_id, Checksum1)),
    ?assertEqual(false, itest_cookbook_util:checksum_exists(Got#chef_cookbook_version.org_id, Checksum2)).

%% @doc check that the cookbook row is still in the database until all
%% versions of the cookbook have been deleted
delete_cookbook_multiple_versions() ->
    {AuthzId, OrgId, Name} = itest_cookbook_util:make_cookbook(<<"delete_multiple">>),
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),

    CookbookVersion0 = itest_cookbook_util:make_cookbook_version(<<"000delete_multiple">>, 0, {AuthzId, OrgId, Name}),
    CookbookVersion1 = itest_cookbook_util:make_cookbook_version(<<"001delete_multiple">>, 1, {AuthzId, OrgId, Name}),
    Checksums = [ chef_test_suite_helper:make_id(<<"checksum1">>),
                  chef_test_suite_helper:make_id(<<"checksum2">>)],
    CookbookVersion20 = CookbookVersion0#chef_cookbook_version{checksums=Checksums},
    CookbookVersion21 = CookbookVersion1#chef_cookbook_version{checksums=Checksums},
    ok = chef_sql:mark_checksums_as_uploaded(CookbookVersion20#chef_cookbook_version.org_id,
                                             Checksums),

    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion20)),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CookbookVersion21)),
    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% No checksums should be deleted from the checksum table so we should get
    %% back an empty list.
    ?assertEqual(#chef_db_cb_version_delete{
                    cookbook_delete=false,
                    deleted_checksums=[]}, chef_sql:delete_cookbook_version(CookbookVersion20)),
    Got = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% All checksums should be deleted when the second (and final) cookbook
    %% version is deleted.
    #chef_db_cb_version_delete{
        cookbook_delete=CookbookDeleted,
        deleted_checksums=DeletedChecksums} = chef_sql:delete_cookbook_version(CookbookVersion21),

    ?assertEqual(true, CookbookDeleted), %% Last version of this cookbook, so deleting the cookbook as well
    ?assertEqual(lists:sort(Checksums),
                 lists:sort(DeletedChecksums)),

    %not_found = chef_sql:fetch_cookbook_authz(OrgId, Name).
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)).

%% Combined integration test for entire cookbook create workflow.
%% Starts with (org_id, cookbook_name, cookbook_version)
%%
%% 1. Get #chef_cookbook{} record in Org with given cookbook_name
%% 2. CHECK AUTHZ
%%  a. if {ok, Cookbook}  check for READ on #chef_cookbook.authz_id
%%  b. if {ok, not_found}, check for CREATE on
%%     cookbook_container_authz_id, create new AuthzId

%% 3. Create #chef_cookbook_version{}
%%  a. If {ok, not_found) then create_cookbook_version/1
%%  b. If exists update_cookbook_version/?
%%
%% TODO - integrate cookbook_version_checksums
%% TODO - 3b. write cookbook update code


%% Cover the case where nothing exists in the DB
cookbook_create_from_scratch() ->
    OrgId = chef_test_suite_helper:the_org_id(),
    Name = <<"cookbook_itest_create">>,

    %% Step 1.
    %% TODO: fetch_cookbook_version with helpful return values
    ?assertEqual(not_found, chef_sql:fetch_cookbook_authz(OrgId, Name)),

    %% Step 2b. - AUTHZ
    AzId = chef_test_suite_helper:make_az_id(<<"itest_create">>),
    Cookbook = {AzId, OrgId, Name},

    %% Step 3.
    %% TODO: add chef_object:new_record for this
    CbVersion = itest_cookbook_util:make_cookbook_version(<<"001itest_create">>, 1, Cookbook),

    %% Step 3a.
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion)).

%% Cover the case where the cookbook exists, but not the actual cookbook version
cookbook_create_new_version() ->
    OrgId = chef_test_suite_helper:the_org_id(),
    Name = <<"cookbook_itest_create_new">>,

    %% Setup - upload a different version of the Cookbook
    Cookbook0 = {chef_test_suite_helper:make_az_id(<<"itest_create_new">>), OrgId, Name},
    CbVersion0 = itest_cookbook_util:make_cookbook_version(<<"000itest_create_new">>, 0, Cookbook0),
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion0)),

    %% Step 1.
    AuthzId = chef_sql:fetch_cookbook_authz(OrgId, Name),

    %% Step 2a. - AUTHZ
    %%   We check Cookbook.authz_id here

    %% Step 3.
    CbVersion = itest_cookbook_util:make_cookbook_version(<<"001itest_create_new">>, 1, {OrgId, Name, AuthzId}),

    %% Step 3a.
    ?assertEqual({ok, 1}, chef_sql:create_cookbook_version(CbVersion)).
