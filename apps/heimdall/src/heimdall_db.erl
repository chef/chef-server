%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et

-module(heimdall_db).

-include("heimdall.hrl").

-export([
         create/2,
         create_acl/5,
         delete/2,
         exists/2,
         has_permission/4,
         statements/0
        ]).

create_stmt(actor)     -> insert_actor;
create_stmt(container) -> insert_container;
create_stmt(group)     -> insert_group;
create_stmt(object)    -> insert_object.

-spec create(auth_type(), auth_id()) -> ok | {conflict, term()} | {error, term()}.
create(Type, AuthzId) ->
    CreateStatement = create_stmt(Type),
    case sqerl:statement(CreateStatement, [AuthzId], count) of
        {ok, 1} ->
            ok;
        {conflict, Reason} ->
            {conflict, Reason};
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

create_acl_stmt(actor, actor) -> insert_actor_acl_actor;
create_acl_stmt(actor, group) -> insert_actor_acl_group;
create_acl_stmt(group, actor) -> insert_group_acl_actor;
create_acl_stmt(group, group) -> insert_group_acl_group;
create_acl_stmt(object, actor) -> insert_object_acl_actor;
create_acl_stmt(object, group) -> insert_object_acl_group;
create_acl_stmt(container, actor) -> insert_container_acl_actor;
create_acl_stmt(container, group) -> insert_container_acl_group.

-spec create_acl(auth_type(), auth_id(), auth_type(), auth_id(),
                 binary()) -> ok | {error, term()}.
create_acl(TargetType, TargetId, AuthorizeeType, AuthorizeeId, Permission) ->
    CreateStatement = create_acl_stmt(TargetType, AuthorizeeType),
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

permission_query(actor) -> actor_has_permission_on_actor;
permission_query(group) -> actor_has_permission_on_group;
permission_query(object) -> actor_has_permission_on_object;
permission_query(container) -> actor_has_permission_on_container.

-spec has_permission(auth_type(), auth_id(), auth_id(), binary()) -> boolean().
has_permission(TargetType, TargetId, RequestorId, Permission) ->
    PermissionStatement = permission_query(TargetType),
    {ok, Answer} = sqerl:select(PermissionStatement,
                                [RequestorId, TargetId, Permission],
                                first_as_scalar, [permission]),
    Answer.

statements() ->
    Path = filename:join([code:priv_dir(heimdall), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.
