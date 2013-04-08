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

-module(oc_chef_authz).

%
% TODO:
%
% Helper function to ADD/DELETE actors and groups from record
% (expecting we won't want to make everyone do this common task

-ifndef(TEST).
-export([
         add_client_to_clients_group/3,
         delete_resource/3,
         create_object_if_authorized/4,
         get_container_aid_for_object/3,
         make_context/1,
         is_authorized_on_resource/6,
         ping/0,
         remove_actor_from_actor_acl/2
        ]).
-else.
-compile([export_all]).
-endif.

-include("oc_chef_authz.hrl").
-include("oc_chef_authz_db.hrl").

-export_type([oc_chef_authz_context/0]).

-define(x_ops_requester_id, "X-Ops-Requesting-Actor-Id").
-define(x_ops_user_id, "X-Ops-User-Id").
-define(x_ops_user_id_value, "front-end-service").
-define(atom_bin_perms, [{create, <<"create">>},
                         {read, <<"read">>},
                         {update, <<"update">>},
                         {delete, <<"delete">>},
                         {grant, <<"grant">>}]).

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

-type contained_object_name() :: 'client' |
                                 'container' |
                                 'cookbook' |
                                 'data' |
                                 'environment'|
                                 'group' |
                                 'node' |
                                 'role' |
                                 'sandboxes' |
                                 'search'.

-spec ping() -> pong | pang.
ping() ->
    oc_chef_authz_http:ping().

-spec make_context(binary()) -> #oc_chef_authz_context{}.
make_context(ReqId)  ->
    oc_chef_authz_db:make_context(ReqId).

%%%
%%% Takes a Creator authz id,  an object type atom and an OrgId and
%%% creates the authz permissions for the object if the creator is permitted
%%% otherwise returns {error, forbidden}
%%%
-spec create_object_if_authorized(oc_chef_authz_context(),
                                  object_id(),
                                  object_id(),
                                  contained_object_name()) ->
                                         {ok, object_id()} |
                                         {error, forbidden}.
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
-spec get_container_aid_for_object(oc_chef_authz_context(),
                                   object_id(),
                                   contained_object_name()) ->
                                          object_id().
get_container_aid_for_object(Context, OrgId, ObjectType) ->
    ContainerName = object_type_to_container_name(ObjectType),
    Container = oc_chef_authz_db:fetch_container(Context, OrgId, ContainerName),
    Container#chef_container.authz_id.

%% @doc Create a new authz object using the ACL of a container as a template for the new
%% object's ACL.
%%
%% `RequestorId' will have all permissions on the new object.  The permissions associated
%% with the ACL for `ContainerAId' will be merged into the ACL for the new object.
%%
%% TODO: consider error cases in more detail
-spec create_object_with_container_acl(requestor_id(), object_id()) -> {ok, object_id()} |
                                                                       {error, forbidden}.
create_object_with_container_acl(RequestorId, ContainerAId) ->
    {ok, ObjectId} = create_resource(RequestorId, object),
    case merge_acl_from_container(RequestorId, ContainerAId, ObjectId) of
        ok ->
            {ok, ObjectId};
        {error, object_acl} ->
            error_logger:error_msg("Unable to read ACL for newly created resource: ~p~n", [ObjectId]),
            {error, forbidden};
        _Error ->
            {error, forbidden}
    end.

%%%
%%% merge_acl_from_container
%%% TODO: consider error cases in more detail
-spec merge_acl_from_container(requestor_id(), object_id(), object_id()) -> ok |
                                                                            {error, object_acl | container_acl}.
merge_acl_from_container(RequestorId, ContainerId, ObjectId) ->
    case get_acl_for_resource(RequestorId, container, ContainerId) of
        {ok, CAcl} ->
            case get_acl_for_resource(RequestorId, object, ObjectId) of
                {ok, OAcl} ->
                    NAcl = merge_acl(CAcl, OAcl),
                    case set_acl(RequestorId, ObjectId, NAcl) of
                        ok ->
                            ok;
                        {error, _} ->
                            %% The details of the error will have already been logged
                            {error, object_acl}
                    end;
                Error ->
                    error_logger:error_msg("Error fetching ACL on object ~p for requestor ~p: ~p~n",
                                           [ObjectId, RequestorId, Error]),
                    {error, object_acl}
            end;
        Error ->
            error_logger:error_msg("Error fetching ACL on container ~p for requestor ~p: ~p~n",
                                   [ContainerId, RequestorId, Error]),
            {error, container_acl}
    end.

%% @doc Set the ACL of the object to the given ACL, one ACE at a time.
%%
%% Returns 'ok' if all ACEs are successfully set, and {error, Reason} at the first failure.
%%
%% No, this is not transactional in any sense, but then again, neither is CouchDB.  In any
%% case, this will change dramatically once we move to SQL.
-spec set_acl(requestor_id(),
              object_id(),
              authz_acl()) -> ok | {error, any()}.
set_acl(_RequestorId, _ObjectId, []) ->
    ok;
set_acl(RequestorId, ObjectId, [{Method, ACE}|Rest]) ->
    case set_ace_for_object(RequestorId, ObjectId, Method, ACE) of
        ok ->
            set_acl(RequestorId, ObjectId, Rest);
        {error, Reason} ->
            error_logger:error_msg("Error setting ACE ~p for method ~p on object ~p for requestor ~p: ~p~n",
                                   [ACE, Method, ObjectId, RequestorId, Reason]),
            {error, Reason}
    end.

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
    case oc_chef_authz_http:request(Url, get, [], [], RequestorId) of
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
    case oc_chef_authz_http:request(pluralize_resource(ResourceType), post, [], [], RequestorId) of
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
    case oc_chef_authz_http:request(Url, delete, [], [], RequestorId) of
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
    case oc_chef_authz_http:request(Url, get, [], [], RequestorId) of
        {ok, Data} ->
            Acl=extract_acl(Data),
            {ok, Acl};
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%
% Replace the actors and groups of an ace
% PUT {objects|groups|actors|containers}/:id/acl/:action
%
-spec set_ace_for_object(requestor_id(),
                         object_id(),
                         access_method(),
                         authz_ace()) -> ok |
                                         {error, any()}.
set_ace_for_object(RequestorId, Id, AccessMethod, #authz_ace{actors=Actors, groups=Groups}) ->
    Url = make_url([pluralize_resource(object), Id, acl, AccessMethod]),
    Body = jiffy:encode({[{<<"actors">>, Actors}, {<<"groups">>, Groups}]}),
    case oc_chef_authz_http:request(Url, put, [], Body, RequestorId) of
        ok -> ok;
        %% Expected errors are forbidden, not_found, server_error
        {error, Error} -> {error, Error}
    end.

%% @doc Adds the given client authz ID to the actors list of the organization's "clients"
%% group.  Clients must be members of this group in order to behave properly as such.
%%
%% Taking this approach instead of a more general "add_to_group" approach since this client
%% operation is the only instance.
-spec add_client_to_clients_group(#oc_chef_authz_context{},
                                 OrgId :: object_id(),
                                 ClientAuthzId :: object_id()) ->
                                          ok | {error, term()}.
add_client_to_clients_group(Context, OrgId, ClientAuthzId) ->
    %% We need the superuser ID here because when a validator creates a client, the won't
    %% have the permissions required to add that new client to the clients group.
    {ok, SuperuserId} = application:get_env(oc_chef_authz, authz_superuser_id),

    ClientGroupAuthzId = oc_chef_authz_db:fetch_group_authz_id(Context, OrgId, <<"clients">>),
    Url = make_url([<<"groups">>, ClientGroupAuthzId, <<"actors">>, ClientAuthzId]),
    case oc_chef_authz_http:request(Url, put, [], [], SuperuserId) of
        ok             -> ok;
        {error, Error} -> {error, Error}
    end.

%% @doc As a given actor (`TargetActorId'), remove another actor (`ActorIdToRemove')
%% completely from its ACL.
%%
%% This is originally intended to support the case of removing a validator client from its
%% own ACL.  We only want the validator to be able to create a new non-validator client, and
%% we really mean it!
%%
%% Both `TargetActorId' and `ActorIdToRemove' are assumed here to be the AuthzId of actors,
%% not any other kind of Authz object (group, container, or object).  This should be
%% verified by callers of this function.
-spec remove_actor_from_actor_acl(ActorIdToRemove::object_id(),
                                  TargetActorId::object_id()) -> ok | {error, any()}.
remove_actor_from_actor_acl(ActorIdToRemove, TargetActorId) ->
    %% Target actor fetches its own ACL
    {ok, Acl} = get_acl_for_resource(TargetActorId, actor, TargetActorId),
    FilteredAcl = remove_actor_from_acl(ActorIdToRemove, Acl),

    %% Target actor updates its own ACL
    set_acl(TargetActorId, actor, TargetActorId, FilteredAcl).

%% @doc Front-end to recursive implementation in `remove_actor_from_acl/3`.
-spec remove_actor_from_acl(ActorId::object_id(), Acl::authz_acl()) -> authz_acl().
remove_actor_from_acl(ActorId, Acl) ->
    remove_actor_from_acl(ActorId, Acl, []).

%% @doc Removes `ActorId` from all `actors` lists in the given ACL.
-spec remove_actor_from_acl(ActorId::object_id(),
                            AclToProcess::authz_acl(),
                            FilteredAcl::authz_acl()) ->
                                   authz_acl().
remove_actor_from_acl(_ActorId, [], Acc) ->
    lists:reverse(Acc);
remove_actor_from_acl(ActorId, [{Permission, Ace}|Rest], Acc) ->
    Filtered = remove_actor_from_ace(ActorId, Ace),
    remove_actor_from_acl(ActorId, Rest, [{Permission, Filtered} | Acc]).

%% @doc Returns the given authz_ace with `ActorId` filtered out of the `actors` lists.  The
%% `groups` list is untouched.
-spec remove_actor_from_ace(ActorId::object_id(), Ace::#authz_ace{}) -> #authz_ace{}.
remove_actor_from_ace(ActorId, #authz_ace{actors=Actors, groups=Groups}) ->
    #authz_ace{actors=[A || A <- Actors, A /= ActorId],
               groups=Groups}.

-spec pluralize_resource(resource_type()) -> <<_:48,_:_*8>>.
pluralize_resource(actor) -> <<"actors">>;
pluralize_resource(container) -> <<"containers">>;
pluralize_resource(group) -> <<"groups">>;
pluralize_resource(object) -> <<"objects">>.

-spec object_type_to_container_name(contained_object_name()) -> <<_:32,_:_*8>>.
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
-spec to_text(atom() | binary() | string()) -> string().
to_text(E) when is_binary(E) ->
    binary_to_list(E);
to_text(E) when is_atom(E) ->
    atom_to_list(E);
to_text(E) when is_list(E) ->
    E.

-spec make_url([string()|binary()|atom(),...]) -> string().
make_url(Components) ->
    string:join([to_text(E) || E <- Components],"/").

% Extract actors and groups from the
% TODO refine spec
% todo add error handling
%% The return type should always be `{[binary()], [binary()]}' but since we are extracting
%% from a JSON blob using ej, dialyzer has no way to verify that so we have to use a broader
%% spec.
-spec extract_actors_and_groups(ej:json_object()) -> {ej:json_term(), ej:json_term()} | error.
extract_actors_and_groups(JsonBlob) ->
    Actors = ej:get({<<"actors">>}, JsonBlob),
    Groups = ej:get({<<"groups">>}, JsonBlob),
    case Actors =:= undefined orelse Groups =:= undefined of
        true -> error;
        false -> {Actors, Groups}
    end.

-spec extract_ace(ej:json_object()) -> authz_ace().
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
    RawAcl = jiffy:decode(AclJson),
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
    RawContainerAcl = jiffy:decode(ContainerAclJson),
    ContainerAcl = extract_acl(RawContainerAcl),

    {ok, NodeAclJson} = file:read_file("../test/example_node_acl.json"),
    RawNodeAcl = jiffy:decode(NodeAclJson),
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
