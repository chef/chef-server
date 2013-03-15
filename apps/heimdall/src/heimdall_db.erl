%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(heimdall_db).

-include("heimdall.hrl").

-export([acl_membership/4,
         add_to_group/3,
         create/3,
         create_ace/5,
         delete/2,
         delete_acl/4,
         exists/2,
         group_membership/2,
         has_any_permission/3,
         has_permission/4,
         remove_from_group/3,
         statements/0]).

-spec create(auth_type(), auth_id(), auth_id()) ->
                    ok | {conflict, term()} | {error, term()}.
create(Type, AuthzId, RequestorId) when RequestorId =:= superuser orelse
                                        RequestorId =:= undefined ->
    CreateStatement = create_entity_with_no_requestor,
    case sqerl:select(CreateStatement, [Type, AuthzId], first_as_scalar, [success]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
create(Type, AuthzId, RequestorId) ->
    CreateStatement = create_entity,
    case sqerl:select(CreateStatement, [Type, AuthzId, RequestorId], first_as_scalar,
                      [success]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

delete_stmt(actor)     -> delete_actor_by_authz_id;
delete_stmt(container) -> delete_container_by_authz_id;
delete_stmt(group)     -> delete_group_by_authz_id;
delete_stmt(object)    -> delete_object_by_authz_id.

-spec delete(auth_type(), auth_id()) -> ok | {error, term()}.
delete(Type, AuthzId) ->
    DeleteStatement = delete_stmt(Type),
    case sqerl:statement(DeleteStatement, [AuthzId], count) of
        {ok, 1} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

exists_query(actor)     -> actor_exists;
exists_query(container) -> container_exists;
exists_query(group)     -> group_exists;
exists_query(object)    -> object_exists.

-spec exists(auth_type(), auth_id()) -> boolean().
exists(Type, AuthId) ->
    StatementName = exists_query(Type),
    {ok, Answer} = sqerl:select(StatementName, [AuthId], first_as_scalar, [exists]),
    Answer.

create_ace_stmt(actor, actor) -> insert_actor_acl_actor;
create_ace_stmt(actor, group) -> insert_actor_acl_group;
create_ace_stmt(group, actor) -> insert_group_acl_actor;
create_ace_stmt(group, group) -> insert_group_acl_group;
create_ace_stmt(object, actor) -> insert_object_acl_actor;
create_ace_stmt(object, group) -> insert_object_acl_group;
create_ace_stmt(container, actor) -> insert_container_acl_actor;
create_ace_stmt(container, group) -> insert_container_acl_group.

-spec create_ace(auth_type(), auth_id(), auth_type(), auth_id(),
                 permission()) -> ok | {error, term()}.
create_ace(TargetType, TargetId, AuthorizeeType, AuthorizeeId, Permission) ->
    CreateStatement = create_ace_stmt(TargetType, AuthorizeeType),
    case sqerl:statement(CreateStatement, [TargetId, AuthorizeeId, Permission],
                         count) of
        {ok, 1} ->
            ok;
        {conflict, _Reason} ->
            % Conflicts are actually just fine here; if permission already exists,
            % we simply don't add it again.
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

acl_member_query(actor, actor) -> actors_in_actor_acl;
acl_member_query(group, actor) -> groups_in_actor_acl;
acl_member_query(actor, group) -> actors_in_group_acl;
acl_member_query(group, group) -> groups_in_group_acl;
acl_member_query(actor, object) -> actors_in_object_acl;
acl_member_query(group, object) -> groups_in_object_acl;
acl_member_query(actor, container) -> actors_in_container_acl;
acl_member_query(group, container) -> groups_in_container_acl.

-spec acl_membership(auth_type(), auth_type(), auth_id(), permission()) ->
                              list() | {error, term()}.
acl_membership(TargetType, AuthorizeeType, AuthzId, Permission) ->
    MembershipStatement = acl_member_query(AuthorizeeType, TargetType),
    case sqerl:select(MembershipStatement, [AuthzId, Permission], rows_as_scalars,
                      [authz_id]) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, Error} ->
            {error, Error}
    end.

delete_acl_stmt(actor, actor)    -> delete_actors_from_actor_acl;
delete_acl_stmt(group, actor)    -> delete_groups_from_actor_acl;
delete_acl_stmt(actor, group)    -> delete_actors_from_group_acl;
delete_acl_stmt(group, group)    -> delete_groups_from_group_acl;
delete_acl_stmt(actor, object)    -> delete_actors_from_object_acl;
delete_acl_stmt(group, object)    -> delete_groups_from_object_acl;
delete_acl_stmt(actor, container)    -> delete_actors_from_container_acl;
delete_acl_stmt(group, container)    -> delete_groups_from_container_acl.

-spec delete_acl(auth_type(), auth_type(), auth_id(), permission()) ->
                        ok | {error, term()}.
delete_acl(AuthorizeeType, TargetType, TargetId, Permission) ->
    DeleteStatement = delete_acl_stmt(AuthorizeeType, TargetType),
    case sqerl:statement(DeleteStatement, [TargetId, Permission], count) of
        {ok, _Count} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec has_permission(auth_type(), auth_id(), auth_id(), permission()) -> boolean().
has_permission(TargetType, TargetId, RequestorId, Permission) ->
    PermissionStatement = actor_has_permission_on,
    case sqerl:select(PermissionStatement, [RequestorId, TargetId, TargetType, Permission],
                      first_as_scalar, [permission]) of
        {ok, Answer} ->
            Answer;
        {error, <<"null value cannot be assigned to variable \"actor_id\" declared NOT NULL">>} ->
            %% If we get a request for a bogus member_id, just return false
            false
    end.

-spec has_any_permission(auth_type(), auth_id(), auth_id()) -> boolean().
has_any_permission(TargetType, TargetId, RequestorId) ->
    PermissionStatement = actor_has_any_permission_on,
    case sqerl:select(PermissionStatement, [RequestorId, TargetId, TargetType],
                      first_as_scalar, [permission]) of
        {ok, Answer} ->
            Answer;
        {error, <<"null value cannot be assigned to variable \"actor_id\" declared NOT NULL">>} ->
            %% If we get a request for a bogus member_id, just return false
            false
    end.

membership_query(actor) -> group_actor_members;
membership_query(group) -> group_group_members.

-spec group_membership(auth_type(), auth_id()) -> list() | {error, term()}.
group_membership(TargetType, GroupId) ->
    MembershipStatement = membership_query(TargetType),
    case sqerl:select(MembershipStatement, [GroupId], rows_as_scalars,
                      [authz_id]) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, Error} ->
            {error, Error}
    end.

group_insert_stmt(actor)     -> insert_actor_into_group;
group_insert_stmt(group)     -> insert_group_into_group.

-spec add_to_group(auth_type(), auth_id(), auth_id()) -> ok | {error, term()}.
add_to_group(Type, MemberId, GroupId) ->
    InsertStatement = group_insert_stmt(Type),
    case sqerl:statement(InsertStatement, [MemberId, GroupId], count) of
        {ok, 1} ->
            ok;
        {conflict, Reason} ->
            % Already in group, nothing to do here
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

group_remove_stmt(actor)     -> delete_actor_from_group;
group_remove_stmt(group)     -> delete_group_from_group.

-spec remove_from_group(auth_type(), auth_id(), auth_id()) -> ok | {error, term()}.
remove_from_group(Type, MemberId, GroupId) ->
    DeleteStatement = group_remove_stmt(Type),
    case sqerl:statement(DeleteStatement, [MemberId, GroupId], count) of
        {ok, 1} ->
            ok;
        {ok, none} ->
            {error, not_found_in_group};
        {error, Reason} ->
            {error, Reason}
    end.

statements() ->
    Path = filename:join([code:priv_dir(heimdall), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.
