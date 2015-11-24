-module(oc_erchef_bootstrap).

-export([bootstrap/0]).

-include("chef_types.hrl").
-include("oc_chef_types.hrl").

bootstrap() ->
    Ctx = chef_db:make_context(?API_MAX_VER, <<"0000">>),
    verify_not_bootstrapped(Ctx),
    {SuperuserAuthzId, SuperuserId} = make_superuser_ids(),
    ok = make_superuser_keypair(Ctx, SuperuserAuthzId, SuperuserId),
    ok = make_superuser(Ctx, SuperuserAuthzId, SuperuserId),
    ok = make_global_container(Ctx, SuperuserAuthzId, <<"organizations">>),
    ok = make_global_container(Ctx, SuperuserAuthzId, <<"users">>),
    ok.

verify_not_bootstrapped(Ctx) ->
    % We'll want to expand this...
    not_found = chef_db:fetch(Ctx, #chef_user{username = <<"pivotal">>}).

make_superuser_ids() ->
    {ok, SuperuserAuthzId} = oc_chef_authz:create_resource(oc_chef_authz:superuser_id(), actor),
    SuperuserId = chef_object_base:make_org_prefix_id(?GLOBAL_PLACEHOLDER_ORG_ID),
    {SuperuserAuthzId, SuperuserId}.

make_superuser(Ctx, SuperuserAuthzId, SuperuserId) ->
    Data = {[{<<"id">>, SuperuserId},
             {<<"username">>, <<"pivotal">>},
             {<<"email">>, <<"support@chef.io">>},
             % Password is unusable for auth, but required:
             {<<"password">>, chef_object_base:make_guid()},
             {<<"display_name">>, <<"Default Superuser Account">>}]},
    Superuser = chef_user:new_record(?API_MAX_VER, ?GLOBAL_PLACEHOLDER_ORG_ID, SuperuserAuthzId, Data),
    chef_db:create(Superuser, Ctx, SuperuserAuthzId).

make_superuser_keypair(Ctx, SuperuserAuthzId, SuperuserId) ->
    {Public, Private} = chef_keygen_cache:get_key_pair(),
    PivotalKey = chef_key:new_record(?API_MAX_VER, ?GLOBAL_PLACEHOLDER_ORG_ID,
                                     SuperuserAuthzId, {SuperuserId, {[{<<"public_key">>, Public}]}}),
    ok = chef_db:create(PivotalKey, Ctx, SuperuserAuthzId),
    ok = file:write_file("/etc/opscode/pivotal.pem", Private, [sync]),
    ok.

make_global_container(Ctx, SuperuserAuthzId, Type) ->
    {ok, ContainerAuthzId} = oc_chef_authz:create_resource(SuperuserAuthzId, container),
    Container = oc_chef_container:new_record(?API_MAX_VER, ?GLOBAL_PLACEHOLDER_ORG_ID,
                                             ContainerAuthzId, {[{<<"containername">>, Type}]}),
    chef_db:create(Container, Ctx, SuperuserAuthzId).

