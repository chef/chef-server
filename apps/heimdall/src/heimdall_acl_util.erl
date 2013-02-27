-module(heimdall_acl_util).

-include("heimdall.hrl").

-export([add_access/5,
         add_full_access/4,
         check_access/4,
         check_any_access/3]).

add_access(Permission, TargetType, TargetId, AuthorizeeType, AuthorizeeId) ->
    case heimdall_db:create_acl(TargetType, list_to_binary(TargetId),
                                AuthorizeeType, list_to_binary(AuthorizeeId),
                                atom_to_binary(Permission, latin1)) of
        ok ->
            ok;
        Error ->
            throw(Error)
    end.

add_full_access(TargetType, TargetId, AuthorizeeType, AuthorizeeId) ->
    case {AuthorizeeType, AuthorizeeId} of
        {actor, undefined} ->
            % The user we're giving access to doesn't exist (i.e., this is
            % an actor creation with no requestor) so don't add any access
            ok;
        {actor, superuser} ->
            % The user we're giving access to doesn't exist (i.e., this is
            % a superuser request) so don't add any access
            ok;
        {Type, Id} ->
            add_access(create, TargetType, TargetId, Type, Id),
            add_access(read, TargetType, TargetId, Type, Id),
            add_access(update, TargetType, TargetId, Type, Id),
            add_access(delete, TargetType, TargetId, Type, Id),
            add_access(grant, TargetType, TargetId, Type, Id)
    end.

check_access(TargetType, TargetId, RequestorId, Permission) ->
    case RequestorId of
        superuser ->
            true;
        Id ->
            heimdall_db:has_permission(TargetType, list_to_binary(TargetId),
                                       list_to_binary(Id),
                                       atom_to_binary(Permission, latin1))
    end.

check_any_access(TargetType, TargetId, RequestorId) ->
    check_access(TargetType, TargetId, RequestorId, create) or
        check_access(TargetType, TargetId, RequestorId, read) or
        check_access(TargetType, TargetId, RequestorId, update) or
        check_access(TargetType, TargetId, RequestorId, delete) or
        check_access(TargetType, TargetId, RequestorId, grant).
