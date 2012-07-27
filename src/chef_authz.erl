%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @doc authorization - Interface to the opscode authorization servize
%%
%% This module is an Erlang port of the mixlib-authorization Ruby gem.
%%
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(chef_authz).

-include("chef_authz.hrl").

-define(x_ops_requester_id, "X-Ops-Requesting-Actor-Id").
-define(x_ops_user_id, "X-Ops-User-Id").
-define(x_ops_user_id_value, "front-end-service").
-define(atom_bin_perms, [{create, <<"create">>},
                         {read, <<"read">>},
                         {update, <<"update">>},
                         {delete, <<"delete">>},
                         {grant, <<"grant">>}]).

%
% TODO:
%
% Helper function to ADD/DELETE actors and groups from record
% (expecting we won't want to make everyone do this common task

-export([create_object_if_authorized/4,
         create_object_with_container_acl/2,
         get_container_aid_for_object/3,
         is_authorized_on_resource/6,
         create_resource/2,
         delete_resource/3,
         add_to_group/4,
         delete_from_group/4,
         get_group/2,
         get_acl_for_resource/3,
         get_ace_for_resource/4,
         set_ace_for_resource/5,
         ping/0]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

-type requestor_id() :: binary().
-type actor_id() :: binary().
-type object_id() :: <<_:256>>.

-spec ping() -> pong | pang.
ping() ->
    chef_authz_http:ping().

%%%
%%% Takes a Creator authz id,  an object type atom and an OrgId and
%%% creates the authz permissions for the object if the creator is permitted
%%% otherwise returns {error, forbidden}
%%%
create_object_if_authorized(Context, OrgId, CreatorAId, ObjectType) ->
    ContainerAId = get_container_aid_for_object(Context, OrgId, ObjectType),
    case is_authorized_on_resource(CreatorAId, container, ContainerAId, actor, CreatorAId, create) of
        false -> {error, forbidden};
        true ->
            create_object_with_container_acl(CreatorAId, ContainerAId)
    end.

%%%
%%% get_container_aid_for_object
%%% TODO: consider error cases in more detail
get_container_aid_for_object(Context, OrgId, ObjectType) ->
    ContainerName = object_type_to_container_name(ObjectType),
    Container = chef_authz_db:fetch_container(Context, OrgId, ContainerName),
    Container#chef_container.authz_id.

-spec create_object_with_container_acl(requestor_id(), object_id()) -> {ok, object_id()}.
%% @doc Create a new authz object using the ACL of a container as a template for the new
%% object's ACL.
%%
%% `RequestorId' will have all permissions on the new object.  The permissions associated
%% with the ACL for `ContainerAId' will be merged into the ACL for the new object.
%%
%% TODO: consider error cases in more detail
create_object_with_container_acl(RequestorId, ContainerAId) ->
    {ok, ObjectId} = create_resource(RequestorId, object),
    ok = merge_acl_from_container(RequestorId, ContainerAId, ObjectId),
    {ok, ObjectId}.

%%%
%%% merge_acl_from_container
%%% TODO: consider error cases in more detail
-spec merge_acl_from_container(requestor_id(), binary(), object_id()) -> ok.
merge_acl_from_container(RequestorId, ContainerId, ObjectId) ->
    {ok, CAcl} = get_acl_for_resource(RequestorId, container, ContainerId),
    {ok, OAcl} = get_acl_for_resource(RequestorId, object, ObjectId),
    NAcl = merge_acl(CAcl, OAcl),
    [ ok = set_ace_for_resource(RequestorId, object, ObjectId,  Method, Ace) ||
        {Method, Ace} <- NAcl],
    ok.

%
% Test if an actor is authorized
% Corresponds to GET /{actors|containers|groups|objects}/:id/acl/{actors|groups}/:member_id
%
-spec is_authorized_on_resource(requestor_id(), resource_type(), object_id(),
                              'actor'|'group', actor_id(), access_method())
                             -> true|false|{error,server_error}.
is_authorized_on_resource(RequestorId, ResourceType, ResourceId, ActorType, ActorId, AccessMethod)
  when is_atom(ResourceType) and is_atom(ActorType) and is_atom(AccessMethod) ->
    Url = make_url([pluralize_resource(ResourceType), ResourceId, <<"acl">>,
                    AccessMethod, pluralize_resource(ActorType), ActorId ] ),
    case chef_authz_http:request(Url, get, [], [], RequestorId) of
        ok -> true;
        %% This api returns not found for any missing permission
        {error, not_found} -> false;
        %% Otherwise, we expect server_error; not forbidden
        {error, server_error} -> {error, server_error}
    end.


%
% Create entity in authz

%
% This succeeds unless authz is truly foobared. It doesn't seem to care what the requestor-id is.
%
-spec create_resource(requestor_id(), actor|container|group|object) -> {ok, object_id()} |
                                                                       {error, server_error}.
create_resource(RequestorId, ResourceType) ->
    %% authz can return 500, and we'll throw. I think that is correct
    %% What are the failure modes? Can we succeed and not get a doc?
    case chef_authz_http:request(pluralize_resource(ResourceType), post, [], [], RequestorId) of
        %% What are the failure modes? Can we succeed and not get a doc?
        {ok, IdDoc} -> {ok, ej:get({<<"id">>}, IdDoc)};
        {error, server_error} -> {error, server_error}
    end.

%
% Delete entity in authz
% Corresponds to DELETE /{actors|groups|objects}/:id
% No delete for containers...
-spec delete_resource(requestor_id(), 'actor'|'group'|'object', object_id())
                     ->  ok | {error, forbidden|not_found|server_error}.
delete_resource(RequestorId, ResourceType, Id) ->
    Url = make_url([pluralize_resource(ResourceType), Id]),
    case chef_authz_http:request(Url, delete, [], [], RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Get contents of a group
% Corresponds to GET groups/:group_id
%
-spec get_group(requestor_id(), binary())
               -> {ok, #authz_group{}} | {error, forbidden|not_found|server_error}.
get_group(RequestorId, GroupId) ->
    Url = make_url([groups, GroupId]),
    case chef_authz_http:request(Url, get, [], [], RequestorId) of
        {ok, Data} ->
            {Actors, Groups} = extract_actors_and_groups(Data),
            Group = #authz_group{actors=Actors, groups=Groups},
            {ok, Group};
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Add a member to a group
% Corresponds to PUT /groups/:group_id/{actor|group}/:actor_or_group_id
%
-spec add_to_group(requestor_id(), binary(), 'actor'|'group', binary()) -> ok|{error, any()}.
add_to_group(RequestorId, GroupId, ResourceType, ActorOrGroupId) ->
    Url = make_url([groups, GroupId, pluralize_resource(ResourceType), ActorOrGroupId]),
    %% Expected errors are forbidden, not_found, server_error
    case chef_authz_http:request(Url, put, [], [], RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Remove a member from a group
% Corresponds to PUT /groups/:group_id/{actor|group}/:actor_or_group_id
%
-spec delete_from_group(requestor_id(), binary(), 'actor'|'group', binary()) -> ok|{error, any()}.
delete_from_group(RequestorId, GroupId, Type, ActorOrGroupId) ->
    Url = make_url([groups, GroupId, pluralize_resource(Type), ActorOrGroupId]),
    case chef_authz_http:request(Url, delete, [], [], RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.


%
% Get acl from an entity
% GET {objects|groups|actors|containers}/:id/acl
%
-spec get_acl_for_resource(requestor_id(), resource_type(), binary()) -> {ok, authz_acl()}|{error, any()}.
get_acl_for_resource(RequestorId, ResourceType, Id) ->
    Url = make_url([pluralize_resource(ResourceType), Id, acl]),
    case chef_authz_http:request(Url, get, [], [], RequestorId) of
        {ok, Data} ->
            Acl=extract_acl(Data),
            {ok, Acl};
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Get the ace for a resource
%
% GET {objects|groups|actors|containers}/:id/acl/:action

-spec get_ace_for_resource(requestor_id(), resource_type(), binary(), access_method()) -> {ok, authz_ace()}|{error, any()}.
get_ace_for_resource(RequestorId, ResourceType, Id, AccessMethod) ->
    Url = make_url([pluralize_resource(ResourceType), Id, acl, AccessMethod]),
    case chef_authz_http:request(Url, get, [], [], RequestorId) of
        {ok, Data} ->
            {Actors, Groups} = extract_actors_and_groups(Data),
            Ace = #authz_ace{actors=Actors, groups=Groups},
            {ok, Ace};
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Remove all actors and groups from an ace
% DELETE {objects|groups|actors|containers}/:id/acl/:action
%
% THIS ISN'T ACTUALLY IMPLEMENTED IN AUTHZ!!!
%
-ifdef(HASDELETE).
-spec delete_ace_for_resource(requestor_id(), resource_type(), binary(), access_method()) -> ok|{error, any()}.
delete_ace_for_resource(RequestorId, ResourceType, Id, AccessMethod) ->
    Url = make_url([pluralize_resource(ResourceType), Id, acl, AccessMethod]),
    case chef_authz_http:request(Url, delete, [], [], RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.
-endif.

%
% Replace the actors and groups of an ace
% PUT {objects|groups|actors|containers}/:id/acl/:action
%
-spec set_ace_for_resource(requestor_id(), resource_type(), binary(), access_method(), authz_ace()) -> ok|{error, any()}.
set_ace_for_resource(RequestorId, ResourceType, Id, AccessMethod, #authz_ace{actors=Actors, groups=Groups}) ->
    Url = make_url([pluralize_resource(ResourceType), Id, acl, AccessMethod]),
    Body = ejson:encode({[{<<"actors">>, Actors}, {<<"groups">>, Groups}]}),
    case chef_authz_http:request(Url, put, [], Body, RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

-spec pluralize_resource(resource_type()) -> <<_:48,_:_*8>>. % <<"actors">> | <<"containers">> | <<"groups">> | <<"objects">>.
pluralize_resource(actor) -> <<"actors">>;
pluralize_resource(container) -> <<"containers">>;
pluralize_resource(group) -> <<"groups">>;
pluralize_resource(object) -> <<"objects">>.

-spec object_type_to_container_name('client' |
                                    'container' |
                                    'cookbook' |
                                    'data' |
                                    'environment'|
                                    'group' |
                                    'node' |
                                    'role' |
                                    'sandboxes' |
                                    'search') -> <<_:32,_:_*8>>.
object_type_to_container_name(client) -> <<"clients">>;
object_type_to_container_name(container) -> <<"containers">>;
object_type_to_container_name(cookbook) -> <<"cookbooks">>;
object_type_to_container_name(data) -> <<"data">>; % breaks the simple atom() + s strategy
object_type_to_container_name(environment) -> <<"environments">>;
object_type_to_container_name(group) -> <<"groups">>;
object_type_to_container_name(node) -> <<"nodes">>;
object_type_to_container_name(role) -> <<"roles">>;
object_type_to_container_name(sandbox) -> <<"sandboxes">>;
object_type_to_container_name(search) -> <<"search">>.

%
% This exists for testing and debugging; it's too expensive for day to day use.
% TODO:
% write: auth_id_to_object(Server, OrgId, AuthId) ->
%

to_text(E) when is_binary(E) ->
    binary_to_list(E);
to_text(E) when is_atom(E) ->
    atom_to_list(E);
to_text(E) when is_list(E) ->
    E.

-spec make_url([string()|binary()|atom()]) -> string().
make_url(Components) ->
    string:join([to_text(E) || E <- Components],"/").

% Extract actors and groups from the
% TODO refine spec
% todo add error handling
-spec extract_actors_and_groups({}) -> {[binary()], [binary()]} | error.
extract_actors_and_groups(JsonBlob) ->
    Actors = ej:get({<<"actors">>}, JsonBlob),
    Groups = ej:get({<<"groups">>}, JsonBlob),
    case Actors =:= undefined orelse Groups =:= undefined of
        true -> error;
        false -> {Actors, Groups}
    end.

-spec extract_ace({}) -> authz_ace().
extract_ace(JsonBlob) ->
    {Actors, Groups} = extract_actors_and_groups(JsonBlob),
    #authz_ace{actors=Actors, groups=Groups}.

-spec extract_acl({}) -> authz_acl().
extract_acl(JsonBlob) ->
    [ {PAtom, extract_ace(ej:get({PBin},JsonBlob))} || {PAtom, PBin} <- ?atom_bin_perms ].

%
% This is needed by the container permission inheritance
%
-spec merge_ace(authz_ace(), authz_ace()) -> authz_ace().
merge_ace(#authz_ace{actors=Ace1Actors, groups=Ace1Groups},
          #authz_ace{actors=Ace2Actors, groups=Ace2Groups}) ->
    #authz_ace{
           actors=lists:usort(Ace1Actors ++ Ace2Actors),
           groups=lists:usort(Ace1Groups ++ Ace2Groups)
    }.

-spec merge_acl(authz_acl(), authz_acl()) -> authz_acl().
merge_acl(Acl1, Acl2) ->
    [{K, merge_ace(A,B)} || {{K, A}, {K, B}} <- lists:zip(Acl1, Acl2)].


%% unit tests for internal functions
-ifdef(TEST).
extract_acl_test() ->
    {ok, AclJson} = file:read_file("../test/example_container_acl.json"),
    RawAcl = ejson:decode(AclJson),
    Acl = extract_acl(RawAcl),
    [ ?assertEqual(true, lists:keymember(Perm, 1, Acl)) || Perm <- ?access_methods ],
    ReadAce = proplists:get_value(read, Acl),
    ?assertEqual(ReadAce#authz_ace.actors, [<<"5360afaf2d6bace8609f0e3df100086a">>]),
    ?assertEqual(lists:sort(ReadAce#authz_ace.groups),
                 lists:sort([<<"5360afaf2d6bace8609f0e3df1c18c1c">>,
                             <<"5360afaf2d6bace8609f0e3df1c1094a">>,
                             <<"5360afaf2d6bace8609f0e3df1c1555d">>])).

merge_acl_test_() ->
    {ok, ContainerAclJson} = file:read_file("../test/example_container_acl.json"),
    RawContainerAcl = ejson:decode(ContainerAclJson),
    ContainerAcl = extract_acl(RawContainerAcl),

    {ok, NodeAclJson} = file:read_file("../test/example_node_acl.json"),
    RawNodeAcl = ejson:decode(NodeAclJson),
    NodeAcl = extract_acl(RawNodeAcl),

    MergedAcl = merge_acl(ContainerAcl, NodeAcl),

    ReadAce = proplists:get_value(read, MergedAcl),
    ExpectedActors = [<<"5360afaf2d6bace8609f0e3df100086a">>,
                      <<"5360afaf2d6bace8609f0e3df13e7e8e">>],
    ExpectedGroups = [<<"5360afaf2d6bace8609f0e3df1c18c1c">>,
                      <<"5360afaf2d6bace8609f0e3df1c1094a">>,
                      <<"5360afaf2d6bace8609f0e3df1c1555d">>],
    [ ?_assertEqual(true, lists:member(Actor, ReadAce#authz_ace.actors)) || Actor <- ExpectedActors ] ++
    [ ?_assertEqual(true, lists:member(Group, ReadAce#authz_ace.groups)) || Group <- ExpectedGroups ] ++
    [ ?_assertEqual(true, lists:keymember(Perm, 1, MergedAcl)) || Perm <- ?access_methods ].

-endif.
