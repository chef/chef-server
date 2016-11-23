%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 80-*-
%% ex: ts=4 sw=4 et

-module(bifrost_db).

-include("bifrost.hrl").

-export([ping/0,
         acl_membership/4,
         add_to_group/3,
         bulk_permission/4,
         create/3,
         delete/2,
         delete_acl/3,
         exists/2,
         group_membership/2,
         has_permission/4,
         remove_from_group/3,
         statements/0,
         update_acl/5,
         is_recursive_member_of_group/3]).

ping() ->
    case select(ping, [], first_as_scalar, [ping]) of
        {ok, <<"pong">>} -> ok;
        {error, Reason} -> {error, Reason}
    end.

translate(<<"OC001">>) -> group_cycle;
translate(Code) -> sqerl_pgsql_errors:translate_code(Code).

select(Statement, Params, Transform, TransformParams) ->
    case sqerl:select_with(sqerl:make_context(bifrost), Statement, Params, Transform, TransformParams) of
        {error, {Code, Message}} ->
            {error, {translate(Code), Message}};
        Other ->
            Other
    end.

statement(Statement, Params, Transform) ->
    case sqerl:statement_with(sqerl:make_context(bifrost), Statement, Params, Transform) of
        {error, {Code, Message}} ->
            {error, {translate(Code), Message}};
        Other ->
            Other
    end.

%% If this looks a bit odd, it's because we use the same PG function for authz
%% types created with or without requestors (to avoid repeating logic in the DB
%% function).  In the cases where no requestor is supplied (either because it's
%% a superuser request, or for an actor -- the V1 API doesn't require that any
%% requesting actor be supplied), it simply skips inserting the requestor into
%% the new entity's ACL.
-spec create(auth_type(), auth_id(), requestor_id()) -> ok | {error, term()}.
create(Type, AuthzId, RequestorId) when RequestorId =:= superuser ->
    create(Type, AuthzId, undefined);
create(Type, AuthzId, RequestorId) ->
    case select(create_entity, [Type, AuthzId, RequestorId],
                first_as_scalar, [success]) of
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
    case statement(DeleteStatement, [AuthzId], count) of
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
    {ok, Answer} = select(StatementName, [AuthId], first_as_scalar, [exists]),
    Answer.

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
    case select(MembershipStatement, [AuthzId, Permission], rows_as_scalars,
                [authz_id]) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, Error} ->
            {error, Error}
    end.

-spec update_acl(auth_type(), auth_id(), permission(), list(), list()) ->
                        ok | {error, term()}.
update_acl(TargetType, TargetId, Permission, Actors, Groups) ->
    case select(update_acl, [TargetType, TargetId, Permission, Actors, Groups],
                first_as_scalar, [success]) of
        {ok, true} ->
            ok;
        {error, {not_null_violation, _Reason}} ->
            % We'll get this whenever there is an attempt to add a non-existent member
            {error, not_null_violation};
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_acl(auth_type(), auth_id(), permission()) -> ok | {error, term()}.
delete_acl(TargetType, TargetId, Permission) ->
    case select(clear_acl, [TargetType, TargetId, Permission],
                first_as_scalar, [success]) of
        {ok, true} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec bulk_permission(auth_id(), list(), permission(), auth_type()) ->
                             list() | {error, term()}.
bulk_permission(ActorId, Targets, Perm, TargetType) ->
    % Note: this returns which of the supplied Targets actually have permission;
    % the bulk endpoint takes them and returns the opposite list, i.e., the
    % targets that the supplied actor does NOT have permission on
    case select(actor_has_bulk_permission, [ActorId, Targets, TargetType, Perm],
                rows_as_scalars, [authz_id]) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, {null_value_not_allowed, _Error}} ->
            % This happends when AuthId doesn't exist
            {error, {invalid_actor, ActorId}};
        {error, Error} ->
            {error, Error}
    end.

-spec has_permission(auth_type(), auth_id(), auth_id(), permission() | any) ->
                            boolean() | {error, _}.
has_permission(TargetType, TargetId, RequestorId, Permission) ->
    case select(actor_has_permission_on, [RequestorId, TargetId, TargetType,
                                          Permission],
                first_as_scalar, [permission]) of
        {ok, Answer} ->
            Answer;
        {error, {not_null_violation, _Error}} ->
            % This happens when the target doesn't exist
            {error, {invalid_target, {TargetType, TargetId}}};
        {error, {null_value_not_allowed, _Error}} ->
            % This happens when the actor doesn't exist
            {error, {invalid_actor, RequestorId}};
        {error, Error} ->
            {error, Error}
    end.

membership_query(actor) -> group_actor_members;
membership_query(group) -> group_group_members.

-spec group_membership(auth_type(), auth_id()) -> list() | {error, term()}.
group_membership(TargetType, GroupId) ->
    MembershipStatement = membership_query(TargetType),
    case select(MembershipStatement, [GroupId], rows_as_scalars,
                [authz_id]) of
        {ok, L} when is_list(L) ->
            L;
        {ok, none} ->
            [];
        {error, Error} ->
            {error, Error}
    end.

%% TODO currently only supports actor since db query depends on db function
%% groups_for_actor and a similar db function for groups does not exist.
is_recursive_member_of_group_query(actor) -> is_actor_recursive_member_of_group.

%% Returns all groups this actor is a member of,
%% including parents of those groups, recursively.
-spec is_recursive_member_of_group(auth_type(), auth_id(), auth_id()) -> boolean() | {error, term()}.
is_recursive_member_of_group(Type, MemberAuthzId, ParentGroupAuthzId) ->
    case select(is_recursive_member_of_group_query(Type), [MemberAuthzId, ParentGroupAuthzId], rows_as_scalars, [exists]) of
        {ok, [Answer]} ->
            Answer;
        {error, Error} ->
            {error, Error}
    end.

group_insert_stmt(actor)     -> insert_actor_into_group;
group_insert_stmt(group)     -> insert_group_into_group.

-spec add_to_group(auth_type(), auth_id(), auth_id()) -> ok | {error, term()}.
add_to_group(Type, MemberId, GroupId) ->
    InsertStatement = group_insert_stmt(Type),
    case statement(InsertStatement, [MemberId, GroupId], count) of
        {ok, 1} ->
            ok;
        {conflict, _Reason} ->
            % Already in group, nothing to do here
            ok;
        % Both of the next two errors are returned for cycles; the first is a custom
        % error from our cycle-detection function, the second is due to the simpler
        % validation (check) that groups aren't members of themselves
        {error, {group_cycle, _Reason}} ->
            {error, group_cycle};
        {error, {check_violation, _Reason}} ->
            {error, group_cycle};
        {error, {not_null_violation, _Reason}} ->
            % We'll get this whenever there is an attempt to add a non-existent member
            {error, not_null_violation};
        {error, Reason} ->
            {error, Reason}
    end.

group_remove_stmt(actor)     -> delete_actor_from_group;
group_remove_stmt(group)     -> delete_group_from_group.

-spec remove_from_group(auth_type(), auth_id(), auth_id()) -> ok | {error, term()}.
remove_from_group(Type, MemberId, GroupId) ->
    DeleteStatement = group_remove_stmt(Type),
    case statement(DeleteStatement, [MemberId, GroupId], count) of
        {ok, 1} ->
            ok;
        {ok, none} ->
            {error, not_found_in_group};
        {error, Reason} ->
            {error, Reason}
    end.

statements() ->
    Path = filename:join([code:priv_dir(bifrost), "pgsql_statements.config"]),
    {ok, Statements} = file:consult(Path),
    Statements.
