%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-module(oc_chef_authz).

-include("oc_chef_authz.hrl").
-include_lib("oc_chef_authz/src/oc_chef_authz_db.hrl").

-export([
         create_entity_if_authorized/4,
         delete_resource/3,
         get_container_aid_for_object/3,
         is_authorized_on_resource/6,
         make_context/3
        ]).

create_entity_if_authorized(_Context, _OrgId, _Creator, _ObjectType) ->
    <<RandomInt:128>> = crypto:rand_bytes(16),
    RandomId = iolist_to_binary(io_lib:format("~32.16.0b", [RandomInt])),
    {ok, RandomId}.

delete_resource(_RequestorId, _ResourceType, _Id) ->
    ok.

get_container_aid_for_object(_Context, _OrgId, _ObjectType) ->
    <<"00000000000000000000000000000000">>.

is_authorized_on_resource(_ReqestorId, _ResourceType, _ResourceId, _ActorType, _ActorId, _AccessMethod) ->
    true.

make_context(ApiVersion, ReqId, Darklaunch) ->
    #oc_chef_authz_context{server_api_version = ApiVersion,
                           reqid = ReqId,
                           darklaunch = Darklaunch }.

is_actor_transitive_member_of_group(_SuperuserId, _ActorId, _GroupId) ->
    true.

superuser_id() ->
    <<"00000000000000000000000000000000">>.
