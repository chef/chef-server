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
    case {AuthorizeeType, AuthorizeeId} of
        {actor, undefined} ->
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
