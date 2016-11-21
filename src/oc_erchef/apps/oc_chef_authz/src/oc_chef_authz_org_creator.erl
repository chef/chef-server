%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2014 Chef, Inc. All Rights Reserved.

-module(oc_chef_authz_org_creator).

-export([
         create_org/2,
         create_org/3
       ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%%
%% This is intended to be a quick reimplementation of the previous group policy
%%
%% Future directions:
%% We will probably want to make the default starting group of a user configurable

-include("oc_chef_authz.hrl").
-include("oc_chef_types.hrl").
-include("chef_types.hrl").

%% This is an ACE in human readable form, with chef objects, not
-record(hr_ace, {clients = [],
                 users = [],
                 groups = []}).


-define(CONTAINERS, [clients, containers, cookbooks, data, environments,
                     groups, nodes, roles, sandboxes, policies, policy_groups,
                     cookbook_artifacts]).

-define(GROUPS, [admins, 'billing-admins', clients, users, public_key_read_access]).

-define(ALL_PERMS, [create, read, update, delete, grant]).

%%
%% We use a simple declarative language to describe permissions. This could use some
%% cleanup, but eventually something like this could be uploaded as JSON, and users
%% could select the org policy on creation
%%
%% This probably sufficient for Chef 12, but there's some enhancements that would be nice
%% for future chef versions
%%
%% TODO (future work)
%% * Decide if we need an explicit superuser in this syntax. Right now the 'creator' user is
%%   assumed to be the superuser (pivotal). If we use the 'superuser' that is provided in the
%%   config file we need to be able to handle multiple such users. Disambiguating the creator
%%   would help us move more of the policy of org creation out of webui.
%% * It would be nice to provide an api that retroactively enforces a policy on an org; especially
%%   would be useful to fix broken orgs.
%% * The DEFAULT_EC_EXPANDED_ORG policy represents EC, we should write a policy that resembles OSC.
%% * Document this language a bit more thoroughly
%% * This might be expanded to cover other large org policy features
%%   * default starting group for users
%%   * special users added to every org (associated, and with permissions)
%%
%% Performance enhancements. The default policy takes about 2 seconds on an unloaded dev-vm.
%% * Look at bulk APIs to support this
%% * Look at parallel execution of bifrost stuff; we could divide it into three phases:
%%   * Object creation (parallel)
%%   * Group population (parallel, but for efficiency should probably not update a group simulaneously
%%   * ACL population (parallel, probably should not update ACL simulaneously)
-define(DEFAULT_EC_EXPANDED_ORG,
        [{create_containers, ?CONTAINERS},
         {create_groups, ?GROUPS},
         {create_org_read_access_group},
         {add_to_groups, user, [creator], [admins, users]},
         {add_to_groups, group, [admins, users], [read_access_group]},
         {add_to_groups, group, [users, clients], [public_key_read_access]},

         %% ACLs are expanded, then applied
         {acls,
          [
           %% Billing admins is very restrictive.
           {add_acl, [{group, 'billing-admins'}], [read,update], [{user, creator},{group, 'billing-admins'}]},

           %% Creator (superuser normally) goes everywhere
           {add_acl,
            [mk_tl(container, ?CONTAINERS), mk_tl(group, ?GROUPS), {organization}],
            ?ALL_PERMS, [{user, creator}]},

           %% Admins
           {add_acl,
            [mk_tl(container, ?CONTAINERS), mk_tl(group, [admins, clients, users]), {organization}],
            ?ALL_PERMS, [{group, admins}]},
           {add_acl, [{group, public_key_read_access}], [read, update], [{group, admins}]},

           %% users
           {add_acl,
            [mk_tl(container, [cookbooks, data, nodes, roles, environments, policies, policy_groups, cookbook_artifacts])],
            [create, read, update, delete], [{group, users}]},
           {add_acl, [{container, clients}], [read, delete], [{group, users}]},
           {add_acl, [mk_tl(container, [groups, containers]), {organization}], [read], [{group, users}]},
           {add_acl, [{container, sandboxes}], [create], [{group, users}]},

           %% clients
           {add_acl, [{container, nodes}], [read, create], [{group, clients}]},
           {add_acl, [{container, policies}], [read], [{group, clients}]},
           {add_acl, [{container, policy_groups}], [read], [{group, clients}]},
           {add_acl, [{container, cookbook_artifacts}], [read], [{group, clients}]},
           {add_acl, [{container, data}], [read], [{group, clients}]},
           {add_acl, mk_tl(container, [cookbooks, environments, roles]), [read] , [{group, clients}]}
          ]
         }
        ]).

%% A little bit of sugar to make a list of Type, Item pairs
mk_tl(Type, List) ->
    [{Type, Item} || Item <- List].

%%
%%

-spec create_org(#oc_chef_organization{}, #chef_requestor{}) -> ok | {error, any()}.
create_org(Org, CreatingUser) ->
    create_org(Org, CreatingUser, ?DEFAULT_EC_EXPANDED_ORG).

create_org(Org, CreatingUser, Policy) ->
    process_policy(Org, CreatingUser, Policy).

%%
%% Execute a policy to create an org
%%

process_policy(#oc_chef_organization{} = Org,
               #chef_requestor{} = User,
               Policy) ->
    process_policy(Policy, Org, User, init_cache(Org, User)).

process_policy([], _, _, _Cache) ->
    %% This is where we might extract some stuff from cache to descibe the created org
    ok;
process_policy([PolicyEntry|Policy], Org, User, Cache) ->
    case process_policy_step(PolicyEntry, Org, User, Cache) of
        {error, _} = Error -> Error;
        {Cache1, Steps} ->
            process_policy(Steps++Policy, Org, User, Cache1)
    end.

%% Returns a tuple of updated cache, and expanded steps to process
%%
process_policy_step({create_containers, List},
                    #oc_chef_organization{server_api_version = ApiVersion, id=OrgId}, #chef_requestor{authz_id=RequestorId}, Cache) ->
    {create_object(ApiVersion, OrgId, RequestorId, container, List, Cache), []};
process_policy_step({create_groups, List},
                    #oc_chef_organization{server_api_version = ApiVersion, id=OrgId}, #chef_requestor{authz_id=RequestorId}, Cache) ->
    {create_object(ApiVersion, OrgId, RequestorId, group, List, Cache), []};
process_policy_step({set_acl_expanded, Object, Acl},
                    #oc_chef_organization{}, #chef_requestor{authz_id=_RequestorId}, Cache) ->
    {ResourceType, AuthzId} = find(Object, Cache),
    Acl1 = [{Action, ace_to_authz(Cache, ACE)} || {Action, ACE} <- Acl],
    %% TODO: Error check authz results
    [ oc_chef_authz:set_ace_for_entity(superuser, ResourceType, AuthzId, Method, ACE) ||
        {Method, ACE} <- Acl1],
    {Cache, []};
process_policy_step({add_to_groups, ActorType, Members, Groups},
                    #oc_chef_organization{}, #chef_requestor{}, Cache) ->
    %% Note that server_api_version is available to us here, if we should
    %% need this behavior versioned in the future.
    MemberIds = objectlist_to_authz(Cache, ActorType, Members),
    GroupIds = objectlist_to_authz(Cache, group, Groups),
    %% TODO capture error return
    [oc_chef_authz:add_to_group(GroupId, Type, MemberId, superuser) ||
        {_, GroupId} <- GroupIds,
        {Type,MemberId} <- MemberIds],
    {Cache, []};
process_policy_step({create_org_read_access_group},
                    #oc_chef_organization{name=OrgName, server_api_version=ApiVersion},
                    #chef_requestor{authz_id=RequestorId}, Cache) ->
    ReadAccessGroupName = oc_chef_authz_db:make_read_access_group_name(OrgName),
    %% TODO: Fix this to be the global groups org id.
    GlobalOrgId = ?GLOBAL_PLACEHOLDER_ORG_ID,
    case create_helper(ApiVersion, GlobalOrgId, RequestorId, group, ReadAccessGroupName) of
        AuthzId when is_binary(AuthzId) ->
            {add_cache(Cache, {group, read_access_group}, group, AuthzId), []};
        Error ->
            {error, Error}
    end;
process_policy_step({acls, Steps}, _Org, _User, Cache) ->
    {Cache, process_acls(Steps)}.

%%
%% Sequence of operations to create an object in authz and in chef sql.
%%
create_object(_, _, _, _, [], Cache) ->
    Cache;
create_object(ApiVersion, OrgId, RequestorId, Type, [Name|Remaining], Cache) ->
    case create_helper(ApiVersion, OrgId, RequestorId, Type, Name) of
        AuthzId when is_binary(AuthzId) ->
            NewCache = add_cache(Cache,{Type, Name}, AuthzId),
            create_object(ApiVersion, OrgId, RequestorId, Type, Remaining, NewCache);
        Error ->
            %% Do we clean up created authz stuff here, or save it for
            %% general org deletion routine later?
            lager:error("Could not create object ~p during creation of org ~s",
                        [{Type, Name}, OrgId]),
            throw(Error)
    end.

create_helper(ApiVersion, OrgId, RequestorId, Type, Name) when is_atom(Name) ->
    BinaryName = atom_to_binary(Name, utf8),
    create_helper(ApiVersion, OrgId, RequestorId, Type, BinaryName);
create_helper(ApiVersion, OrgId, RequestorId, Type, Name) ->
    case oc_chef_authz:create_resource(RequestorId, Type) of
        {ok, AuthzId} ->
            create_chef_side(ApiVersion, OrgId, RequestorId, Type, Name, AuthzId);
        {error, _} = Error ->
            Error
    end.

create_chef_side(ApiVersion, OrgId, RequestorId,  container, Name, AuthzId) ->
    Data = ej:set({<<"containername">>}, {[]}, Name),
    Object =chef_object:new_record(oc_chef_container, ApiVersion, OrgId, AuthzId, Data),
    create_insert(Object, AuthzId, RequestorId);
create_chef_side(ApiVersion, OrgId, RequestorId, group, Name, AuthzId) ->
    Data = ej:set({<<"groupname">>}, {[]}, Name),
    Object = chef_object:new_record(oc_chef_group, ApiVersion, OrgId, AuthzId, Data),
    create_insert(Object, AuthzId, RequestorId).

create_insert(Object, AuthzId, RequestorId) ->
    ObjectRec = chef_object:set_created(Object, RequestorId),
    case chef_sql:create_object(chef_object:create_query(ObjectRec), chef_object:fields_for_insert(ObjectRec)) of
        {ok, 1} ->
            AuthzId;
        Error ->
            {chef_sql, {Error, ObjectRec}}
    end.

%%
%% Helper for creating acls. It's very tedious to write them all out longhand.
%%
%% We keep the human readable names throughout this expansion.  We could probably be more
%% efficient to translate to authz ids earlier, at the cost of a lot of readability
process_acls(AclDesc) ->
    AclMap = lists:foldl(fun update_acl_step/2, dict:new(), lists:flatten(AclDesc)),
    [{set_acl_expanded, Object, Acl} || {Object, Acl} <- dict:to_list(AclMap)].

%% Syntax:
%% {add_acl, Objects, Actions, Members}
%%
%% Objects: [{type, Name} ...]
%% Actions: List of actions to permit (create, read, update, delete, grant)
%% Members: {user|client|group, name}
%% Adds cross product to existing world
%%
%% TODO: There's a lot of common code for ACLs between the ACL endpoint and here.
%% We should extract that code, and expose it as programmatic API for ACL manipulation for
%% use in the future orgmapper replacement.
update_acl_step({add_acl, Objects, Actions, Members}, Acls) ->
    %% split out actors and groups separately
    {Clients, Users, Groups} = lists:foldl(
                                 fun(M, {C, U, G}) ->
                                         case M of
                                             {user, N} ->
                                                 {C, [N|U],G};
                                             {client, N} ->
                                                 {[N|C], U, G};
                                             {group, N} ->
                                                 {C, U, [N|G]}
                                         end
                                 end, {[], [], []}, lists:flatten(Members)),
    AceToAdd = #hr_ace{clients=Clients, users=Users, groups=Groups},
    ObjUpdate = fun(Acl) ->
                           update_acl(Acl, Actions, AceToAdd)
                   end,
    lists:foldl(fun(Object, Acc) ->
                        update_acl_for_object(ObjUpdate, Acc, Object)
                end,
                Acls, lists:flatten(Objects)).

update_acl(Acl, Actions, AceToAdd) ->
    UpdateFun = fun(Ace) ->
                        add_to_ace(Ace, AceToAdd)
                end,
    lists:foldl(fun(Action, AAcl) ->
                        update_ace_by_action(UpdateFun, AAcl, Action)
                end,
                Acl, lists:flatten(Actions)).

% lookup acl for object, create new if missing, apply function, and set it
update_acl_for_object(UpdateFun, Acls, Object) ->
    Acl0 = case dict:find(Object, Acls) of
               {ok, V} -> V;
               error -> []
           end,
    Acl1 = UpdateFun(Acl0),
    dict:store(Object, Acl1, Acls).

% lookup ace for action, create new if missing, apply function, and set it
update_ace_by_action(UpdateFun, Acl, Action) ->
    Ace0 = case lists:keyfind(Action, 1, Acl) of
                         false -> #hr_ace{};
                         {_, A} -> A
                     end,
    Ace1 = UpdateFun(Ace0),
    lists:keystore(Action, 1, Acl, {Action, Ace1}).

merge(Old, New) ->
    lists:umerge(Old, lists:sort(New)).

add_to_ace(#hr_ace{clients=OClients, users=OUsers, groups=OGroups},
           #hr_ace{clients=Clients, users=Users, groups=Groups}) ->
    NClients = merge(OClients, Clients),
    NUsers = merge(OUsers, Users),
    NGroups = merge(OGroups, Groups),
    #hr_ace{clients=NClients, users=NUsers, groups=NGroups}.

%%
%% Simple cache for managing object-> authzid mapping.
%%
%% This is 'scoped' in terms of the org being created, and the  entries
%% here implicitly contain the org id.
%% Most objects are {Type, Name} pairs, such as {user, creator} or {container, nodes}
%% and are mapped to {ResourceType, AuthzId} pairs (e.g. {object, a452dfadsfa} )

init_cache(#oc_chef_organization{authz_id=OrgAuthzId},
           #chef_requestor{authz_id=CreatorAuthzId}) ->
    %% Notes: we assume the creator is a superuser;
    Elements = [ { {user, creator}, CreatorAuthzId },
                 { {organization}, OrgAuthzId } ],
    InsertFun = fun({Item,AuthzId}, Acc) ->
                        add_cache(Acc, Item, AuthzId)
                end,
    lists:foldl(InsertFun, dict:new(), Elements).

add_cache(C, Object, Type, AuthzId) ->
    set(Object, {Type, AuthzId}, C).

add_cache(C, {Type, Object}, AuthzId) ->
    Resource = oc_chef_authz:object_type_to_resource(Type),
    set({Type, Object}, {Resource, AuthzId}, C);
add_cache(C, {Type}, AuthzId) ->
    Resource = oc_chef_authz:object_type_to_resource(Type),
    set({Type}, {Resource, AuthzId}, C).

objectlist_to_authz(C, Type, BareObjectList) ->
    [find({Type,O},C) || O <- lists:flatten(BareObjectList)].

ace_to_authz(C, #hr_ace{clients=Clients, users=Users, groups=Groups}) ->
    {_, ClientIds} = lists:unzip(objectlist_to_authz(C, client, Clients)),
    {_, UserIds} = lists:unzip(objectlist_to_authz(C, user, Users)),
    {_, GroupIds} = lists:unzip(objectlist_to_authz(C, group, Groups)),
    ActorIds = lists:flatten([ClientIds, UserIds]),
    #authz_ace{actors=ActorIds,groups=GroupIds}.

set(Key, Value, C) ->
    dict:store(Key,Value, C).

find(Key, C) ->
    case dict:find(Key,C) of
        {ok, Value} -> Value;
        error ->
            lager:error("Error processing org creation policy, no definition found for ~p", [Key]),
            throw( {error, bad_org_creation_policy})
    end.
