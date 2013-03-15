-module(heimdall_acl).

-include("heimdall.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-export([check_access/5,
         check_any_access/4,
         clear_access/4,
         make_ejson_acl/3,
         make_ejson_action/4,
         parse_acl_json/1,
         update_acl/6]).

%% @doc Check to see if requestor has permission on a particular target
check_access(ReqId, TargetType, TargetId, RequestorId, Permission) ->
    case RequestorId of
        superuser ->
            true;
        Id ->
            ?SH_TIME(ReqId, heimdall_db, has_permission, (TargetType, TargetId, Id,
                                                          Permission))
    end.

%% @doc Check to see if requestor has any permission on target
check_any_access(ReqId, TargetType, TargetId, RequestorId) ->
    case RequestorId of
        superuser ->
            true;
        Id ->
            ?SH_TIME(ReqId, heimdall_db, has_any_permission, (TargetType, TargetId, Id))
    end.

%% @doc Update ACL (for given permission type) on target for all actors and groups
update_acl(ReqId, TargetType, TargetId, Permission, Actors, Groups) ->
    case ?SH_TIME(ReqId, heimdall_db, update_acl, (TargetType, TargetId, Permission,
                                                   Actors, Groups)) of
        {error, Error} ->
            throw({db_error, Error}); 
        ok ->
            ok
    end.

%% @doc Return all ACL members for given member type on an ID
%%
%% I.e., return all actors or groups with read permission for a supplied AuthzID
%% of a given type (we'd be more generic about it, but we need the type to find
%% the correct tables in the DB to return the answer).
acl_members(ReqId, ForType, MemberType, ForId, Permission) ->
    case ?SH_TIME(ReqId, heimdall_db, acl_membership, (ForType, MemberType, ForId,
                                                       Permission)) of
        {error, Error} ->
            throw({db_error, Error});
        {error, <<"null value in column \"authorizee\" violates not-null constraint">>} ->
            throw({db_error, {non_existent_authorizee_for_acl,
                              MemberType, ForId}});
        List ->
            List
    end.

% @doc Clear permission (for given permission type) on target for all actors and groups
clear_access(ReqId, TargetType, TargetId, Permission) ->
    % TODO: Should this be in a postgres function?
    case ?SH_TIME(ReqId, heimdall_db, delete_acl, (actor, TargetType, TargetId,
                                                   Permission)) of
        {error, Error} ->
            throw({db_error, Error});
        ok ->
            case ?SH_TIME(ReqId, heimdall_db, delete_acl, (group, TargetType, TargetId,
                                                           Permission)) of
                {error, Error} ->
                    throw({db_error, Error});
                ok ->
                    ok
            end
    end.

%% @doc Create full EJSON object for permission type on given ID
%%
%% This is returned by the GET /<type>/<id>/acl/<action> endpoint
make_ejson_action(ReqId, Permission, ForType, ForId) ->
    {[{<<"actors">>,
       acl_members(ReqId, ForType, actor, ForId, Permission)},
      {<<"groups">>,
       acl_members(ReqId, ForType, group, ForId, Permission)}]}.

%% @doc Create EJSON object fragment for permission type on given ID
%%
%% This is a fragment for a specific permission, part of what make_ejson_acl
%% returns
make_ejson_part(ReqId, Permission, ForType, ForId) ->
    {Permission, make_ejson_action(ReqId, Permission, ForType, ForId)}.

%% @doc Create full EJSON object for given ID
%%
%% This is returned by the GET /<type>/<id>/acl endpoint
make_ejson_acl(ReqId, ForType, ForId) ->
    {[make_ejson_part(ReqId, <<"create">>, ForType, ForId),
      make_ejson_part(ReqId, <<"read">>, ForType, ForId),
      make_ejson_part(ReqId, <<"update">>, ForType, ForId),
      make_ejson_part(ReqId, <<"delete">>, ForType, ForId),
      make_ejson_part(ReqId, <<"grant">>, ForType, ForId)]}.

%% @doc Parse supplied JSON ACL object, return members it contains
%%
%% This is used by the PUT /<type>/<id>/acl/<action> endpoint
parse_acl_json(Json) ->
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
