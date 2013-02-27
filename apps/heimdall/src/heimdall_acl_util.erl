-module(heimdall_acl_util).

-include("heimdall.hrl").

-export([add_access/5,
         add_full_access/4,
         check_access/4,
         check_any_access/3,
         make_ejson_acl/4,
         make_ejson_action/5]).

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

acl_members(TargetType, AuthorizeeType, AuthzId, Permission, Req, State) ->
    case heimdall_db:acl_membership(TargetType, AuthorizeeType, AuthzId,
                                    Permission) of
        {error, Error} ->
            throw({wm_db_error_tuple,
                   heimdall_wm_error:set_db_exception(Req, State, {error, Error})});
        List ->
            List
    end.

make_ejson_action(Permission, RequestType, AuthzId, Req, State) ->
    {Permission,
     {[{<<"actors">>,
       acl_members(RequestType, actor, AuthzId, Permission, Req, State)},
      {<<"groups">>,
       acl_members(RequestType, group, AuthzId, Permission, Req, State)}]}}.

make_ejson_acl(RequestType, AuthzId, Req, State) ->
    {[make_ejson_action(<<"create">>, RequestType, AuthzId, Req, State),
      make_ejson_action(<<"read">>, RequestType, AuthzId, Req, State),
      make_ejson_action(<<"update">>, RequestType, AuthzId, Req, State),
      make_ejson_action(<<"delete">>, RequestType, AuthzId, Req, State),
      make_ejson_action(<<"grant">>, RequestType, AuthzId, Req, State)]}.
