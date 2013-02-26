-module(heimdall_acl_util).

-include("heimdall.hrl").

-export([add_access/5,
         add_full_access/4]).

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
    add_access(create, TargetType, TargetId, AuthorizeeType, AuthorizeeId),
    add_access(read, TargetType, TargetId, AuthorizeeType, AuthorizeeId),
    add_access(update, TargetType, TargetId, AuthorizeeType, AuthorizeeId),
    add_access(delete, TargetType, TargetId, AuthorizeeType, AuthorizeeId),
    add_access(grant, TargetType, TargetId, AuthorizeeType, AuthorizeeId).
