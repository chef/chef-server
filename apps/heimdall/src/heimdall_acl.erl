-module(heimdall_acl).

-include("heimdall.hrl").

-export([add_access/5,
         add_access_set/5,
         add_full_access/4,
         check_access/4,
         check_any_access/3,
         clear_access/3,
         make_ejson_acl/2,
         make_ejson_action/3,
         parse_acl_json/2]).

add_access(Permission, TargetType, TargetId, AuthorizeeType,
           AuthorizeeId) when is_list(AuthorizeeId) ->
    case heimdall_db:create_ace(TargetType, list_to_binary(TargetId),
                                AuthorizeeType, list_to_binary(AuthorizeeId),
                                atom_to_binary(Permission, latin1)) of
        ok ->
            ok;
        {error, <<"null value in column \"authorizee\" violates not-null constraint">>} ->
            throw({db_error, {non_existent_authorizee_for_acl,
                              AuthorizeeType, AuthorizeeId}});
        {error, Error} ->
            throw({db_error, Error})
    end.

add_access_set(_Perm, _Type, _Id, _OtherType, []) ->
    ok;
add_access_set(Permission, TargetType, TargetId, AuthorizeeType,
               [AuthorizeeId | AuthorizeeList]) ->
    add_access(Permission, TargetType, TargetId, AuthorizeeType,
               binary_to_list(AuthorizeeId)),
    add_access_set(Permission, TargetType, TargetId, AuthorizeeType,
                   AuthorizeeList).

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

clear_access(TargetType, TargetId, Permission) ->
    % TODO: this needs to be a postgres function
    case heimdall_db:delete_acl(actor, TargetType, TargetId,
                                atom_to_binary(Permission, latin1)) of
        {error, Error} ->
            throw({db_error, Error});
        ok ->
            case heimdall_db:delete_acl(group, TargetType, TargetId,
                                        atom_to_binary(Permission, latin1)) of
                {error, Error} ->
                    throw({db_error, Error});
                ok ->
                    ok
            end
    end.

acl_members(TargetType, AuthorizeeType, AuthzId, Permission) ->
    case heimdall_db:acl_membership(TargetType, AuthorizeeType, AuthzId,
                                    Permission) of
        {error, Error} ->
            throw({db_error, Error});
        List ->
            List
    end.

make_ejson_part(Permission, RequestType, AuthzId) ->
    {Permission,
     {[{<<"actors">>,
       acl_members(RequestType, actor, AuthzId, Permission)},
      {<<"groups">>,
       acl_members(RequestType, group, AuthzId, Permission)}]}}.

make_ejson_action(Permission, RequestType, AuthzId) ->
    {[{<<"actors">>,
       acl_members(RequestType, actor, AuthzId, Permission)},
      {<<"groups">>,
       acl_members(RequestType, group, AuthzId, Permission)}]}.

make_ejson_acl(RequestType, AuthzId) ->
    {[make_ejson_part(<<"create">>, RequestType, AuthzId),
      make_ejson_part(<<"read">>, RequestType, AuthzId),
      make_ejson_part(<<"update">>, RequestType, AuthzId),
      make_ejson_part(<<"delete">>, RequestType, AuthzId),
      make_ejson_part(<<"grant">>, RequestType, AuthzId)]}.

parse_acl_json(Json, Action) ->
    try
        Ejson = heimdall_wm_util:decode(Json),
        Actors = ej:get({<<"actors">>}, Ejson),
        Groups = ej:get({<<"groups">>}, Ejson),
        case {Actors, Groups} of
            {ActorList, GroupList} when is_list(ActorList) andalso is_list(GroupList) ->
                {ActorList, GroupList};
            {_, _} ->
                throw({error, invalid_json})
        end
    catch
        throw:{error, {_, invalid_json}} ->
            throw({error, invalid_json});
        throw:{error, {_, truncated_json}} ->
            throw({error, invalid_json})
    end.
