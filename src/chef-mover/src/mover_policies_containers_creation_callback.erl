-module(mover_policies_containers_creation_callback).

-export([
     migration_init/0,
     migration_complete/0,
     migration_type/0,
     supervisor/0,
     migration_start_worker_args/2,
     error_halts_migration/0,
     reconfigure_object/2,
     migration_action/2,
     next_object/0,
     needs_account_dets/0
    ]).

-include("mover.hrl").
-include("mv_oc_chef_authz.hrl").

-record(mover_org, {id, authz_id, name, superuser}).
-record(mover_requestor, {authz_id}).

%% This is an ACE in human readable form
-record(mover_hr_ace, {clients = [],
                 users = [],
                 groups = []}).

-record(mover_chef_container, {
          id,
          authz_id,
          org_id,
          name,
          last_updated_by,
          created_at,
          updated_at
         }).

-define(ORG_POLICY_FOR_POLICIES_CONTAINERS,
        [{create_containers, [policies, policy_groups, cookbook_artifacts]},

         {acls,
          [
           %% Creator (superuser normally) goes everywhere
           {add_acl, [{container, policies}],           [create, read, update, delete, grant], [{user, creator}]},
           {add_acl, [{container, policy_groups}],      [create, read, update, delete, grant], [{user, creator}]},
           {add_acl, [{container, cookbook_artifacts}], [create, read, update, delete, grant], [{user, creator}]},

           %% Admins
           {add_acl, [{container, policies}],           [create, read, update, delete, grant], [{group, admins}]},
           {add_acl, [{container, policy_groups}],      [create, read, update, delete, grant], [{group, admins}]},
           {add_acl, [{container, cookbook_artifacts}], [create, read, update, delete, grant], [{group, admins}]},


           %% users
           {add_acl, [{container, policies}],           [create, read, update, delete], [{group, users}]},
           {add_acl, [{container, policy_groups}],      [create, read, update, delete], [{group, users}]},
           {add_acl, [{container, cookbook_artifacts}], [create, read, update, delete], [{group, users}]},

           %% clients

           {add_acl, [{container, policies}],           [read], [{group, clients}]},
           {add_acl, [{container, policy_groups}],      [read], [{group, clients}]},
           {add_acl, [{container, cookbook_artifacts}], [read], [{group, clients}]}
          ]
         }
        ]).

needs_account_dets() ->
    false.

migration_init() ->
    %% TODO: this fails if the pool is already created. Should be tolerant of
    %% errors and also should tear this down after the migration
    mv_oc_chef_authz_http:create_pool(),
    {ok, SuperUserAuthzId} = sqerl:select(<<"select authz_id from users where username = 'pivotal'">>, [], first_as_scalar, [authz_id]),
    Superuser = #mover_requestor{authz_id = SuperUserAuthzId},
    FindOrgsWithNoPoliciesContainer = <<"SELECT orgs.name, orgs.id, orgs.authz_id FROM orgs "
                                        "LEFT JOIN containers ON orgs.id = org_id AND containers.name='policies' "
                                        "WHERE containers.name IS NULL;">>,
    {ok, OrgsToMigrateResults} = sqerl:execute(FindOrgsWithNoPoliciesContainer),
    OrgsToMigrate = [ db_results_to_org(OrgDbRow, Superuser) || OrgDbRow <- OrgsToMigrateResults],
    mover_transient_migration_queue:initialize_queue(?MODULE, OrgsToMigrate).

migration_complete() ->
    mv_oc_chef_authz_http:delete_pool().


db_results_to_org(FieldsValues, Superuser) ->
    %% Each object will be a list like this:
    %% [{<<"name">>,<<"ponyville">>},
    %%  {<<"id">>,<<"1e9ed26e6cbcdeedb128e896aa0f34c2">>},
    %%  {<<"authz_id">>,<<"8184b014cba9484eabafb8568d299e67">>}
    {<<"name">>, Name}     = proplists:lookup(<<"name">>, FieldsValues),
    {<<"id">>,  Id}        = proplists:lookup(<<"id">>, FieldsValues),
    {<<"authz_id">>, AzId} = proplists:lookup(<<"authz_id">>, FieldsValues),
    #mover_org{name = Name, id = Id, authz_id = AzId, superuser = Superuser}.

migration_start_worker_args(Object, AcctInfo) ->
    [Object, AcctInfo].

next_object() ->
    mover_transient_migration_queue:next(?MODULE).

migration_action(Org, _AcctInfo) ->
    upgrade_org(Org).

migration_type() ->
    <<"policies_containers_creation">>.

supervisor() ->
    mover_transient_worker_sup.

error_halts_migration() ->
    true.

reconfigure_object(_ObjectId, _AcctInfo) ->
    no_op.

insert_container_sql() ->
      <<"INSERT INTO containers (id, authz_id, org_id, name,"
        " last_updated_by, created_at, updated_at) VALUES"
        " ($1, $2, $3, $4, $5, $6, $7)">>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VENDORED oc_chef_auth_org_creator CODE
%%
%% Code in here is vendored and then modified to remove external dependencies.
%% This makes the data migration resilent to future code changes that might
%%
%% break the migration.
%%
%% TODO: remove use of these:
%% * oc_chef_authz (maybe?)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

upgrade_org(#mover_org{superuser = CreatingUser} = Org) ->
    Cache = init_cache(Org, CreatingUser),
    CacheWithGroups = prepopulate_groups(Cache, Org),
    process_policy(?ORG_POLICY_FOR_POLICIES_CONTAINERS, Org, CreatingUser, CacheWithGroups).

prepopulate_groups(Cache, Org) ->
    % Failure is arguably okay if we can't set up admins acls
    % but we can't continue if we can't set it up for clients and users.
    Cache1 = prepopulate_group(Cache, Org, admins),
    Cache2 = prepopulate_group(Cache1, Org, users),
    prepopulate_group(Cache2, Org, clients).

prepopulate_group(Cache, #mover_org{id = OrgId, name = OrgName}, Group) ->
    {ok, Results} = sqerl:adhoc_select([name, authz_id, org_id],
                                       groups,
                                       {'and', [{name, equals, Group}, {org_id, equals, OrgId}]}),
    case Results of
        [] ->
            lager:warning("Could not find group '~p' for org '~s'", [Group, OrgName]),
            add_cache(Cache,{group, Group}, not_found);
        _ ->
            GroupFieldsList = hd(Results),
            {<<"authz_id">>, AuthzId} = proplists:lookup(<<"authz_id">>, GroupFieldsList),
            add_cache(Cache,{group, Group}, AuthzId)
    end.

%%
%% Execute a policy to create an org
%%


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
                    #mover_org{id=OrgId}, #mover_requestor{authz_id=RequestorId}, Cache) ->
    {create_object(OrgId, RequestorId, container, List, Cache), []};
process_policy_step({set_acl_expanded, Object, Acl},
                    #mover_org{}, #mover_requestor{authz_id=_RequestorId}, Cache) ->
    case  find(Object, Cache) of
        {_, not_found} ->
            {Cache, []};
        {ResourceType, AuthzId} ->
            Acl1 = [{Action, ace_to_authz(Cache, ACE)} || {Action, ACE} <- Acl],
            %% TODO: Error check authz results
            %% FURTHER TODO: this probably won't work with our hacked ace records :'(
            [ mv_oc_chef_authz:set_ace_for_entity(superuser, ResourceType, AuthzId, Method, ACE) ||
                {Method, ACE} <- Acl1 ],
            {Cache, []}
    end;
process_policy_step({acls, Steps}, _Org, _User, Cache) ->
    {Cache, process_acls(Steps)}.

objectlist_to_authz(C, Type, BareObjectList) ->
    [find({Type,O},C) || O <- lists:flatten(BareObjectList)].

ace_to_authz(C, #mover_hr_ace{clients=Clients, users=Users, groups=Groups}) ->
    {_, ClientIds} = lists:unzip(objectlist_to_authz(C, client, Clients)),
    {_, UserIds} = lists:unzip(objectlist_to_authz(C, user, Users)),
    {_, GroupIds} = lists:unzip(objectlist_to_authz(C, group, Groups)),
    ActorIds = lists:flatten([ClientIds, UserIds]),
    #authz_ace{actors=ActorIds,groups=GroupIds}.


%%
%% Sequence of operations to create an object in authz and in chef sql.
%%
create_object(_, _, _, [], Cache) ->
    Cache;
create_object(OrgId, RequestorId, Type, [Name|Remaining], Cache) ->
    case create_helper(OrgId, RequestorId, Type, Name) of
        AuthzId when is_binary(AuthzId) ->
            NewCache = add_cache(Cache,{Type, Name}, AuthzId),
            create_object(OrgId, RequestorId, Type, Remaining, NewCache);
        Error ->
            %% Do we clean up created authz stuff here, or save it for
            %% general org deletion routine later?
            lager:error("Could not create object ~p during policies containers migration of org ~s",
                        [{Type, Name}, OrgId]),
            throw(Error)
    end.

create_helper(OrgId, RequestorId, Type, Name) when is_atom(Name) ->
    BinaryName = atom_to_binary(Name, utf8),
    create_helper(OrgId, RequestorId, Type, BinaryName);
create_helper(OrgId, RequestorId, Type, Name) ->
    case mv_oc_chef_authz:create_resource(RequestorId, Type) of
        {ok, AuthzId} ->
            create_chef_side(OrgId, RequestorId, Type, Name, AuthzId);
        {error, _} = Error ->
            Error
    end.

create_chef_side(OrgId, RequestorId,  container, Name, AuthzId) ->
    Object = new_container_record(OrgId, AuthzId, Name, RequestorId),
    create_insert(Object, AuthzId, RequestorId).

create_insert(#mover_chef_container{} = Object, AuthzId, _RequestorId) ->
    case chef_sql_create_container(chef_object_flatten_container(Object)) of
        {ok, 1} ->
            AuthzId;
        Error ->
            {chef_sql, {Error, Object}}
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
    AceToAdd = #mover_hr_ace{clients=Clients, users=Users, groups=Groups},
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
                         false -> #mover_hr_ace{};
                         {_, A} -> A
                     end,
    Ace1 = UpdateFun(Ace0),
    lists:keystore(Action, 1, Acl, {Action, Ace1}).

merge(Old, New) ->
    lists:umerge(Old, lists:sort(New)).

add_to_ace(#mover_hr_ace{clients=OClients, users=OUsers, groups=OGroups},
           #mover_hr_ace{clients=Clients, users=Users, groups=Groups}) ->
    NClients = merge(OClients, Clients),
    NUsers = merge(OUsers, Users),
    NGroups = merge(OGroups, Groups),
    #mover_hr_ace{clients=NClients, users=NUsers, groups=NGroups}.

%%
%% Simple cache for managing object-> authzid mapping.
%%
%% This is 'scoped' in terms of the org being created, and the  entries
%% here implicitly contain the org id.
%% Most objects are {Type, Name} pairs, such as {user, creator} or {container, nodes}
%% and are mapped to {ResourceType, AuthzId} pairs (e.g. {object, a452dfadsfa} )

init_cache(#mover_org{authz_id=OrgAuthzId},
           #mover_requestor{authz_id=CreatorAuthzId}) ->
    %% Notes: we assume the creator is a superuser;
    Elements = [ { {user, creator}, CreatorAuthzId },
                 { {organization}, OrgAuthzId } ],
    InsertFun = fun({Item,AuthzId}, Acc) ->
                        add_cache(Acc, Item, AuthzId)
                end,
    lists:foldl(InsertFun, dict:new(), Elements).

add_cache(C, {Type, Object}, AuthzId) ->
    Resource = mv_oc_chef_authz:object_type_to_resource(Type),
    set({Type, Object}, {Resource, AuthzId}, C);
add_cache(C, {Type}, AuthzId) ->
    Resource = mv_oc_chef_authz:object_type_to_resource(Type),
    set({Type}, {Resource, AuthzId}, C).



set(Key, Value, C) ->
    dict:store(Key,Value, C).
find(Key, C) ->
    case dict:find(Key,C) of
        {ok, Value} -> Value;
        error ->
            lager:error("Error processing org creation policy, no definition found for ~p", [Key]),
            throw( {error, bad_org_creation_policy})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Vendored from oc_chef_container, chef_object_base, chef_sql
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_container_record(OrgId, AuthzId, Name, RequestorId) ->
    %% Now = chef_object_base_sql_date(now),
    Now = os:timestamp(),
    Id = chef_object_base_make_org_prefix_id(OrgId, Name),
    #mover_chef_container{id = Id,
                          authz_id = AuthzId,
                          org_id = OrgId,
                          name = Name,
                          last_updated_by = RequestorId,
                          created_at = Now,
                          updated_at = Now}.

%% vendored from chef_object_base
chef_object_base_make_org_prefix_id(OrgId, Name) ->
    %% assume couchdb guid where trailing part has uniqueness
    <<_:20/binary, OrgSuffix:12/binary>> = OrgId,
    Bin = iolist_to_binary([OrgId, Name, crypto:strong_rand_bytes(6)]),
    <<ObjectPart:80, _/binary>> = erlang:md5(Bin),
    iolist_to_binary(io_lib:format("~s~20.16.0b", [OrgSuffix, ObjectPart])).

%% TODO: epgsql was blowing up trying to convert the output of this to a
%% DATETIME, but it happily accepts an os:timestamp(). Yet this works just fine
%% in oc_erchef. Should probably figure out what's going on.
%%
%%  chef_object_base_sql_date(now) ->
%%      chef_object_base_sql_date(os:timestamp());
%%  chef_object_base_sql_date(DateString) when is_binary(DateString) ->
%%      DateString;
%%  chef_object_base_sql_date({_,_,_} = TS) ->
%%      {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_universal_time(TS),
%%      iolist_to_binary(io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
%%                    [Year, Month, Day, Hour, Minute, Second])).

chef_object_flatten_container(ObjectRec) ->
    [_RecName|Tail] = tuple_to_list(ObjectRec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, ObjectRec});
        false -> ok
    end,
    Tail.


is_undefined(undefined) ->
    true;
is_undefined(_) ->
    false.

chef_sql_create_container(Args) ->
    sqerl:execute(insert_container_sql(), Args).

