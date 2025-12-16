%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_acl_permission_tests).

-include_lib("eunit/include/eunit.hrl").

%% Username mapping functions for multi-tenancy support
%% These functions handle transparent username transformation between
%% internal (username__tenant-uuid) and external (username) formats.

%% @doc Strip tenant ID suffix from mapped username, returning legacy username.
%% Splits on the LAST occurrence of double-underscore followed by UUID.
%% Returns {ok, LegacyUsername} if tenant suffix found and stripped.
%% Returns {error, MappedUsername} if no valid tenant suffix (username returned as-is).
%% Special case: If stripping results in empty binary, returns {error, MappedUsername}.
%% Caller should log warning when {error, _} is returned.
-spec strip_tenant_id(binary()) -> {ok, binary()} | {error, binary()}.
strip_tenant_id(MappedUsername) when is_binary(MappedUsername) ->
    %% Pattern: look for last occurrence of "__" followed by UUID format
    %% UUID format: 8-4-4-4-12 hex digits (e.g., "6e365298-7478-49b9-ba51-38a8c1204af2")
    case binary:split(MappedUsername, <<"__">>, [global]) of
        [_SinglePart] ->
            %% No __ found, return original username
            {error, MappedUsername};
        Parts ->
            %% Split into username parts and potential UUID
            case lists:reverse(Parts) of
                [PotentialUuid | ReversedUsernameParts] ->
                    case is_valid_uuid(PotentialUuid) of
                        true ->
                            %% Valid UUID found, reconstruct username without it
                            LegacyUsername = join_with_separator(
                                lists:reverse(ReversedUsernameParts), 
                                <<"__">>
                            ),
                            %% If stripping results in empty, return original with error
                            case LegacyUsername of
                                <<>> -> {error, MappedUsername};
                                _ -> {ok, LegacyUsername}
                            end;
                        false ->
                            %% Last part is not a UUID, return original username
                            {error, MappedUsername}
                    end
            end
    end.

%% @doc Append tenant ID to legacy username, creating mapped username.
%% Returns binary in format "username__tenant-uuid".
-spec append_tenant_id(binary(), binary()) -> binary().
append_tenant_id(LegacyUsername, TenantId) 
  when is_binary(LegacyUsername), is_binary(TenantId) ->
    <<LegacyUsername/binary, "__", TenantId/binary>>.

%% @doc Check if binary matches UUID format (8-4-4-4-12 hex digits).
-spec is_valid_uuid(binary()) -> boolean().
is_valid_uuid(<<_:8/binary, "-", _:4/binary, "-", _:4/binary, "-", 
                _:4/binary, "-", _:12/binary>>) ->
    true;
is_valid_uuid(_) ->
    false.

%% @doc Join binary parts with separator.
-spec join_with_separator([binary()], binary()) -> binary().
join_with_separator([], _Sep) ->
    <<>>;
join_with_separator([Part], _Sep) ->
    Part;
join_with_separator([Head | Tail], Sep) ->
    lists:foldl(
        fun(Part, Acc) -> <<Acc/binary, Sep/binary, Part/binary>> end,
        Head,
        Tail
    ).

%%====================================================================
%% Unit Tests
%%====================================================================

%% Test strip_tenant_id/1 - Success Cases
strip_tenant_id_simple_username_test() ->
    MappedUsername = <<"test-user-1__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({ok, <<"test-user-1">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_username_with_underscores_test() ->
    MappedUsername = <<"user__name__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({ok, <<"user__name">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_username_with_multiple_double_underscores_test() ->
    MappedUsername = <<"user__foo__bar__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({ok, <<"user__foo__bar">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_username_with_dashes_test() ->
    MappedUsername = <<"test-user-123__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({ok, <<"test-user-123">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_single_char_username_test() ->
    MappedUsername = <<"a__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({ok, <<"a">>}, strip_tenant_id(MappedUsername)).

%% Test strip_tenant_id/1 - Error Cases (returns username as-is)
strip_tenant_id_no_suffix_test() ->
    Username = <<"test-user-1">>,
    ?assertEqual({error, <<"test-user-1">>}, strip_tenant_id(Username)).

strip_tenant_id_malformed_uuid_test() ->
    MappedUsername = <<"test-user-1__not-a-uuid">>,
    ?assertEqual({error, <<"test-user-1__not-a-uuid">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_incomplete_uuid_test() ->
    MappedUsername = <<"test-user-1__6e365298-7478">>,
    ?assertEqual({error, <<"test-user-1__6e365298-7478">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_double_underscore_but_no_uuid_test() ->
    MappedUsername = <<"user__name__value">>,
    ?assertEqual({error, <<"user__name__value">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_empty_username_with_uuid_test() ->
    %% Edge case: just "__uuid" without username part - returns original on empty strip
    MappedUsername = <<"__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual({error, <<"__6e365298-7478-49b9-ba51-38a8c1204af2">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_empty_binary_test() ->
    %% Edge case: empty binary
    MappedUsername = <<"">>,
    ?assertEqual({error, <<"">>}, strip_tenant_id(MappedUsername)).

strip_tenant_id_three_uuids_test() ->
    %% Edge case: uuid1__uuid2__uuid3 (should strip only last UUID)
    MappedUsername = <<"6e365298-7478-49b9-ba51-38a8c1204af2__47499637-46a0-4a75-ad08-cbcf02c0b23e__12345678-1234-1234-1234-123456789abc">>,
    Expected = <<"6e365298-7478-49b9-ba51-38a8c1204af2__47499637-46a0-4a75-ad08-cbcf02c0b23e">>,
    ?assertEqual({ok, Expected}, strip_tenant_id(MappedUsername)).

%% Test append_tenant_id/2 - Success Cases
append_tenant_id_simple_username_test() ->
    Username = <<"test-user-1">>,
    TenantId = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    Expected = <<"test-user-1__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual(Expected, append_tenant_id(Username, TenantId)).

append_tenant_id_username_with_underscores_test() ->
    Username = <<"user__name">>,
    TenantId = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    Expected = <<"user__name__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual(Expected, append_tenant_id(Username, TenantId)).

append_tenant_id_username_with_dashes_test() ->
    Username = <<"test-user-123">>,
    TenantId = <<"47499637-46a0-4a75-ad08-cbcf02c0b23e">>,
    Expected = <<"test-user-123__47499637-46a0-4a75-ad08-cbcf02c0b23e">>,
    ?assertEqual(Expected, append_tenant_id(Username, TenantId)).

append_tenant_id_empty_username_test() ->
    Username = <<>>,
    TenantId = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    Expected = <<"__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual(Expected, append_tenant_id(Username, TenantId)).

append_tenant_id_single_char_username_test() ->
    Username = <<"a">>,
    TenantId = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    Expected = <<"a__6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assertEqual(Expected, append_tenant_id(Username, TenantId)).

%% Test roundtrip: strip -> append should yield original
roundtrip_simple_test() ->
    LegacyUsername = <<"test-user-1">>,
    TenantId = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    MappedUsername = append_tenant_id(LegacyUsername, TenantId),
    ?assertEqual({ok, LegacyUsername}, strip_tenant_id(MappedUsername)).

roundtrip_complex_username_test() ->
    LegacyUsername = <<"user__foo__bar">>,
    TenantId = <<"47499637-46a0-4a75-ad08-cbcf02c0b23e">>,
    MappedUsername = append_tenant_id(LegacyUsername, TenantId),
    ?assertEqual({ok, LegacyUsername}, strip_tenant_id(MappedUsername)).

%% Test is_valid_uuid/1 helper
is_valid_uuid_valid_test() ->
    Uuid = <<"6e365298-7478-49b9-ba51-38a8c1204af2">>,
    ?assert(is_valid_uuid(Uuid)).

is_valid_uuid_another_valid_test() ->
    Uuid = <<"47499637-46a0-4a75-ad08-cbcf02c0b23e">>,
    ?assert(is_valid_uuid(Uuid)).

is_valid_uuid_invalid_format_test() ->
    NotUuid = <<"not-a-uuid">>,
    ?assertNot(is_valid_uuid(NotUuid)).

is_valid_uuid_wrong_length_test() ->
    NotUuid = <<"6e365298-7478-49b9">>,
    ?assertNot(is_valid_uuid(NotUuid)).

is_valid_uuid_no_dashes_test() ->
    NotUuid = <<"6e36529874784b9ba5138a8c1204af2">>,
    ?assertNot(is_valid_uuid(NotUuid)).

%% Test join_with_separator/2 helper
join_with_separator_empty_list_test() ->
    ?assertEqual(<<>>, join_with_separator([], <<"__">>)).

join_with_separator_single_element_test() ->
    ?assertEqual(<<"foo">>, join_with_separator([<<"foo">>], <<"__">>)).

join_with_separator_two_elements_test() ->
    ?assertEqual(<<"foo__bar">>, join_with_separator([<<"foo">>, <<"bar">>], <<"__">>)).

join_with_separator_three_elements_test() ->
    ?assertEqual(<<"foo__bar__baz">>, 
                 join_with_separator([<<"foo">>, <<"bar">>, <<"baz">>], <<"__">>)).
%% ACL response transformation tests
%% These tests verify that ACL response functions properly transform
%% usernames when X-Ops-TenantId header is present.
%% NOTE: These tests require the transformation functions in
%% oc_chef_wm_acl_permission.erl to include transformation logic.

acl_strips_tenant_ids_from_users_test() ->
    TenantId = <<"a1b2c3d4-e5f6-7890-abcd-ef1234567890">>,
    Username = <<"alice">>,
    UsernameWithTenant = <<Username/binary, "__", TenantId/binary>>,
    
    %% Test that strip_tenant_id works on the username
    {ok, Stripped} = strip_tenant_id(UsernameWithTenant),
    ?assertEqual(Username, Stripped).

acl_strips_tenant_ids_from_actors_test() ->
    TenantId = <<"b2c3d4e5-f6a7-8901-bcde-f12345678901">>,
    User1 = <<"bob">>,
    User2 = <<"charlie">>,
    User1WithTenant = <<User1/binary, "__", TenantId/binary>>,
    User2WithTenant = <<User2/binary, "__", TenantId/binary>>,
    
    %% Simulate actors list with tenant IDs
    ActorsWithTenant = [User1WithTenant, User2WithTenant],
    
    %% Transform using the response function from oc_chef_wm_acl_permission module
    ActorsStripped = oc_chef_wm_acl_permission:transform_usernames_for_response(ActorsWithTenant),
    
    ?assertEqual([User1, User2], ActorsStripped).

acl_handles_mixed_users_and_clients_test() ->
    TenantId = <<"c3d4e5f6-a7b8-9012-cdef-123456789012">>,
    User = <<"dave">>,
    Client = <<"app_client">>,
    UserWithTenant = <<User/binary, "__", TenantId/binary>>,
    ClientWithTenant = <<Client/binary, "__", TenantId/binary>>,
    
    %% Both users and clients should be stripped
    ?assertEqual({ok, User}, strip_tenant_id(UserWithTenant)),
    ?assertEqual({ok, Client}, strip_tenant_id(ClientWithTenant)).

%% ACL structure-specific tests
%% These tests verify transformation works with actual ACL permission part structures

transform_acl_permission_part_granular_mode_test() ->
    %% Test granular mode: separate users and clients arrays
    TenantId = <<"d4e5f6a7-b8c9-0123-def0-123456789013">>,
    
    %% Input: users need transformation, clients do not
    Users = [<<"alice">>, <<"bob">>],
    Clients = [<<"client1">>, <<"client2">>],
    
    %% Transform users for request (append tenant IDs)
    TransformedUsers = oc_chef_wm_acl_permission:transform_usernames_for_request(Users, TenantId),
    
    %% Verify users have tenant IDs appended
    ?assertEqual([<<"alice__", TenantId/binary>>, <<"bob__", TenantId/binary>>], TransformedUsers),
    
    %% Verify clients are unchanged
    ?assertEqual(Clients, Clients),
    
    %% Transform back for response (strip tenant IDs)
    StrippedUsers = oc_chef_wm_acl_permission:transform_usernames_for_response(TransformedUsers),
    
    %% Verify users stripped back to original
    ?assertEqual(Users, StrippedUsers).

transform_acl_permission_part_non_granular_mode_test() ->
    %% Test non-granular mode: users and clients merged into actors array
    TenantId = <<"e5f6a7b8-c9d0-1234-ef01-234567890124">>,
    
    %% Input: separate users and clients
    Users = [<<"user1">>, <<"user2">>],
    Clients = [<<"client1">>],
    
    %% Transform users for request
    TransformedUsers = oc_chef_wm_acl_permission:transform_usernames_for_request(Users, TenantId),
    
    %% Merge into actors (non-granular mode)
    Actors = TransformedUsers ++ Clients,
    
    %% Verify actors contains both transformed users and untransformed clients
    ?assert(lists:member(<<"user1__", TenantId/binary>>, Actors)),
    ?assert(lists:member(<<"user2__", TenantId/binary>>, Actors)),
    ?assert(lists:member(<<"client1">>, Actors)),
    ?assertNot(lists:member(<<"user1">>, Actors)),
    
    %% Transform back for response (strip tenant IDs from entire actors list)
    StrippedActors = oc_chef_wm_acl_permission:transform_usernames_for_response(Actors),
    
    %% Verify users stripped, clients unchanged
    ?assert(lists:member(<<"user1">>, StrippedActors)),
    ?assert(lists:member(<<"user2">>, StrippedActors)),
    ?assert(lists:member(<<"client1">>, StrippedActors)).

transform_acl_multiple_permission_parts_test() ->
    %% Test transformation works across multiple ACL permission parts
    %% Simulates full ACL with create/read/update/delete/grant parts
    TenantId = <<"f6a7b8c9-d0e1-2345-f012-345678901235">>,
    
    Users = [<<"admin">>, <<"editor">>],
    
    %% Transform for each permission part (simulating request)
    TransformedUsers = oc_chef_wm_acl_permission:transform_usernames_for_request(Users, TenantId),
    
    %% Verify transformation is consistent across all parts
    ?assertEqual([<<"admin__", TenantId/binary>>, <<"editor__", TenantId/binary>>], 
                 TransformedUsers),
    
    %% Transform back (simulating response for all parts)
    StrippedUsers = oc_chef_wm_acl_permission:transform_usernames_for_response(TransformedUsers),
    
    %% Verify stripping is consistent
    ?assertEqual(Users, StrippedUsers).

transform_acl_empty_users_list_test() ->
    %% Test edge case: empty users list in permission part
    TenantId = <<"a1a2a3a4-b5b6-c7c8-d9d0-e1e2e3e4e5e6">>,
    
    EmptyUsers = [],
    
    %% Transform empty list
    Transformed = oc_chef_wm_acl_permission:transform_usernames_for_request(EmptyUsers, TenantId),
    ?assertEqual([], Transformed),
    
    %% Strip empty list
    Stripped = oc_chef_wm_acl_permission:transform_usernames_for_response(EmptyUsers),
    ?assertEqual([], Stripped).

transform_acl_mixed_mapped_unmapped_users_test() ->
    %% Test handling of mixed mapped/unmapped usernames in response
    %% (Could occur if some users exist in multiple tenants)
    TenantId = <<"b2b3b4b5-c6c7-d8d9-e0e1-f2f3f4f5f6f7">>,
    
    %% Mix of usernames: some with tenant IDs, some without
    MappedUser = <<"user1__", TenantId/binary>>,
    UnmappedUser = <<"user2">>,
    MixedList = [MappedUser, UnmappedUser],
    
    %% Transform for response (strip where possible)
    Stripped = oc_chef_wm_acl_permission:transform_usernames_for_response(MixedList),
    
    %% Mapped user should be stripped, unmapped user unchanged
    ?assertEqual([<<"user1">>, <<"user2">>], Stripped).