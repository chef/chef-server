-module(bifrost_acl).

-include("bifrost.hrl").
-include_lib("ej/include/ej.hrl").
-include_lib("stats_hero/include/stats_hero.hrl").

-export([check_access/5,
         clear_access/4,
         make_ejson_acl/3,
         make_ejson_action/4,
         parse_acl_json/1,
         update_acl/6]).

%% @doc Check to see if actor has permission on a particular target
-spec check_access(request_id(), auth_type(), auth_id(), requestor_id(), permission()) ->
                          boolean() | {error, _}.
check_access(ReqId, TargetType, TargetId, ActorId, Permission) ->
    case ActorId of
        superuser ->
            %% Super user has all access but
            %% we should still validate the target.
            case ?SH_TIME(ReqId, bifrost_db, exists, (TargetType, TargetId)) of
              false ->
                {error, {invalid_target, {TargetType, TargetId}}};
              true ->
                true
            end;
        Id ->
            ?SH_TIME(ReqId, bifrost_db, has_permission, (TargetType, TargetId, Id,
                                                          Permission))
    end.

%% @doc Update ACL (for given permission type) on target for all actors and groups
-spec update_acl(request_id(), auth_type(), auth_id(), permission(), list(), list()) ->
                        ok.
update_acl(ReqId, TargetType, TargetId, Permission, Actors, Groups) ->
    case ?SH_TIME(ReqId, bifrost_db, update_acl, (TargetType, TargetId, Permission,
                                                   Actors, Groups)) of
        {error, not_null_violation} ->
            throw({db_error, {non_existent_member_for_acl, Actors, Groups}});
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
-spec acl_members(request_id(), auth_type(), auth_type(), auth_id(), permission()) ->
                         list().
acl_members(ReqId, ForType, MemberType, ForId, Permission) ->
    case ?SH_TIME(ReqId, bifrost_db, acl_membership, (ForType, MemberType, ForId,
                                                       Permission)) of
        {error, Error} ->
            throw({db_error, Error});
        List ->
            List
    end.

% @doc Clear permission (for given permission type) on target for all actors and groups
-spec clear_access(request_id(), auth_type(), auth_id(), permission()) -> ok.
clear_access(ReqId, TargetType, TargetId, Permission) ->
    case ?SH_TIME(ReqId, bifrost_db, delete_acl, (TargetType, TargetId, Permission)) of
        {error, Error} ->
            throw({db_error, Error});
        ok ->
            ok
    end.

%% @doc Create full EJSON object for permission type on given ID
%%
%% This is returned by the GET /<type>/<id>/acl/<action> endpoint
-spec make_ejson_action(request_id(), permission(), auth_type(), auth_id()) ->
                               ej:json_object().
make_ejson_action(ReqId, Permission, ForType, ForId) ->
    {[{<<"actors">>,
       acl_members(ReqId, ForType, actor, ForId, Permission)},
      {<<"groups">>,
       acl_members(ReqId, ForType, group, ForId, Permission)}]}.

%% @doc Create EJSON object fragment for permission type on given ID
%%
%% This is a fragment for a specific permission, part of what make_ejson_acl
%% returns
-spec make_ejson_part(request_id(), permission(), auth_type(), auth_id()) ->
                             {binary(), ej:json_object()}.
make_ejson_part(ReqId, Permission, ForType, ForId) ->
    {atom_to_binary(Permission, utf8),
     make_ejson_action(ReqId, Permission, ForType, ForId)}.

%% @doc Create full EJSON object for given ID
%%
%% This is returned by the GET /<type>/<id>/acl endpoint
-spec make_ejson_acl(request_id(), auth_type(), auth_id()) ->
                            ej:json_object().
make_ejson_acl(ReqId, ForType, ForId) ->
    {[make_ejson_part(ReqId, create, ForType, ForId),
      make_ejson_part(ReqId, read, ForType, ForId),
      make_ejson_part(ReqId, update, ForType, ForId),
      make_ejson_part(ReqId, delete, ForType, ForId),
      make_ejson_part(ReqId, grant, ForType, ForId)]}.

%% @doc Parse supplied JSON ACL object, return members it contains
%%
%% This is used by the PUT /<type>/<id>/acl/<action> endpoint
-spec parse_acl_json(binary()) -> {list(), list()}.
parse_acl_json(Json) ->
    try
        Ejson = bifrost_wm_util:decode(Json),
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
