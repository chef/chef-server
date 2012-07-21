% @author Mark Anderson <mark@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @end

% @doc authorization - Interface to the opscode authorization servize
%
% This module is an Erlang port of the mixlib-authorization Ruby gem.

-module(chef_authz_tests).

-compile([export_all]).

%% -export([raw_request_test_helper/1,
%%          test_setup/0,
%%          test_cleanup/1]).

-include("chef_authz.hrl").

%-define(setup, chef_authz_tests).
-define(setup, test_utils).

-define(SUPERUSER,  <<"platform-superuser">>).
-define(test_org_name, <<"clownco">>).
-define(test_org_id,  <<"0aa7c5c35fbb4f1890c0a673511137af">>).
-define(test_org_admin, <<"clownco-org-admin">>).
-define(test_org_user1, <<"cooky">>).
-define(no_such_id, <<"deadbeefdeadbeefdeadbeefdeadbeef">>).
-define(authz_host, "http://localhost:5959").
-define(chef_host_name, "localhost").
-define(chef_host_port, 5984).

-define(AUTOMECK_FILE(TestName), filename:join(["..", "test", "automeck_config",
                                                atom_to_list(?MODULE) ++ "_" ++ atom_to_list(TestName) ++
                                                    ".config"])).

-include_lib("eunit/include/eunit.hrl").

%% user_lookup_test() ->
%%     test_utils:test_setup(),                    % starts stats_hero
%%     automeck:mocks(?AUTOMECK_FILE(user_lookup)),
%%     Context = chef_db:make_context(<<"testing">>), % req_id must be a binary
%%     ?assert(is_authz_id(chef_authz:username_to_auth_id(Context, ?SUPERUSER))),
%%     meck:unload().

resource_test_() ->
    {foreach,
    fun() ->
            automeck:mocks(?AUTOMECK_FILE(resource)),
            test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
          %% Resource creation
          {"Simple group create",
           fun() ->
               {ok, NewId} = chef_authz:create_resource(Superuser, group),
               true = is_authz_id(NewId)
           end}
      end,
      fun({_Server, Superuser}) ->
              {"Simple group create by non superuser",
               fun() ->
                       {ok, Actor} = chef_authz:create_resource(Superuser, actor),
                       {ok, Group} = chef_authz:create_resource(Actor, group),
                       true = is_authz_id(Group)
               end}
      end,
      fun({_Server, _Superuser}) ->
              {"Simple group create by non-existient user",
               fun() ->
                       {ok, Group} = chef_authz:create_resource(?no_such_id, group),
                       true = is_authz_id(Group)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete",
               fun() ->
                       {ok, NewId} = chef_authz:create_resource(Superuser, group),
                       true = is_authz_id(NewId),
                       ok = chef_authz:delete_resource(Superuser, group, NewId)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete without permission",
               fun() ->
                       {ok, NewId} = chef_authz:create_resource(Superuser, group),
                       true = is_authz_id(NewId),
                       {error, forbidden} = chef_authz:delete_resource(?no_such_id, group, NewId)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group delete for non-group",
               fun() ->
                       {ok, NewId} = chef_authz:create_resource(Superuser, actor),
                       true = is_authz_id(NewId),
                       {error, server_error} = chef_authz:delete_resource(?no_such_id, group, NewId)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group create, then get empty group",
               fun() ->
                       {ok, NewGroup} = chef_authz:create_resource(Superuser, group),
                       true = is_authz_id(NewGroup),
                       {ok, Group} = chef_authz:get_group(Superuser, NewGroup),
                       #authz_group{actors=[], groups=[]} = Group
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get nonexistient group",
               fun() ->
                       {error, not_found} = chef_authz:get_group(Superuser, ?no_such_id)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"group create, then get by non-auth-d actor",
               fun() ->
                       {ok, Group} = chef_authz:create_resource(Superuser, group),
                       {ok, Actor} = chef_authz:create_resource(Superuser, actor),
                       true = is_authz_id(Group),
                       {error, forbidden} = chef_authz:get_group(Actor, Group)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get as group something that isn't a group (actor)",
               fun() ->
                       {ok, Actor} = chef_authz:create_resource(Superuser, actor),
                       true = is_authz_id(Actor),
                       {error, server_error} = chef_authz:get_group(Superuser, Actor)
               end}
      end]}.

add_to_group_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(add_to_group1)),
              {"add actor to group should succeed as superuser",
               fun() ->
                       {ok, GroupId1} = chef_authz:create_resource(Superuser, group),
                       {ok, ActorId1} = chef_authz:create_resource(Superuser, actor),
                       ok = chef_authz:add_to_group(Superuser, GroupId1, actor, ActorId1),
                       {ok, Group} = chef_authz:get_group(Superuser, GroupId1),
                       #authz_group{actors=[ActorId1], groups=[]} = Group
               end}
      end,
      fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(add_to_group2)),
              {"add actor to group when the actor doesn't exist will fail",
               fun() ->
                       {ok, GroupId} = chef_authz:create_resource(Superuser, group),
                       {error, forbidden} = chef_authz:add_to_group(Superuser, GroupId, actor, ?no_such_id),
                       {ok, Group} = chef_authz:get_group(Superuser, GroupId),
                       #authz_group{actors=[], groups=[]} = Group
               end}
      end,
      fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(add_to_group3)),
              {"add actor to group when the group doesn't exist will fail",
               fun() ->
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       {error, not_found} = chef_authz:add_to_group(Superuser, ?no_such_id, actor, ActorId)
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(add_to_group4)),
              {"add actor to group when you are not the owner or in its ACLs should be forbidden",
               fun() ->
                       {ok, GroupId1} = chef_authz:create_resource(Superuser, group),
                       {ok, ActorId1} = chef_authz:create_resource(Superuser, actor),
                       {error, forbidden} = chef_authz:add_to_group(ActorId1, GroupId1, actor, ActorId1),
                       {ok, Group} = chef_authz:get_group(Superuser, GroupId1),
                       #authz_group{actors=[], groups=[]} = Group
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(add_to_group5)),
              {"add group to group",
               fun() ->
                       {ok, GroupId1} = chef_authz:create_resource(Superuser, group),
                       {ok, ActorId1} = chef_authz:create_resource(Superuser, actor),
                       {ok, GroupId2} = chef_authz:create_resource(ActorId1, group),
                       ok = chef_authz:add_to_group(Superuser, GroupId1, group, GroupId2),
                       {ok, Group} = chef_authz:get_group(Superuser, GroupId1),
                       #authz_group{actors=[], groups=[GroupId2]} = Group
               end}
      end]}.

delete_from_group_test_() ->
    {foreach,
     fun() ->
             automeck:mocks(?AUTOMECK_FILE(delete_from_group)),
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
	  {"delete actor from group should succeed as superuser",
           fun() ->
               {ok, GroupId1} = chef_authz:create_resource(Superuser, group),
               {ok, ActorId1} = chef_authz:create_resource(Superuser, actor),
               ok = chef_authz:add_to_group(Superuser, GroupId1, actor, ActorId1),
               ok = chef_authz:delete_from_group(Superuser, GroupId1, actor, ActorId1),
               {ok, Group} = chef_authz:get_group(Superuser, GroupId1),
               #authz_group{actors=[], groups=[]} = Group
           end}
      end,
      fun({_Server, Superuser}) ->
	   {"delete actor from a group that doesn't exist should fail",
	    fun() ->
		{ok, ActorId1} = chef_authz:create_resource(Superuser, actor),
		{error, not_found} = chef_authz:delete_from_group(Superuser, ?no_such_id, actor, ActorId1)
	    end}
      end,
      fun({_Server, Superuser}) ->
	   {"delete actor from something that isn't a group that doesn't exist should fail",
	    fun() ->
		{ok, ActorId} = chef_authz:create_resource(Superuser, actor),
		{ok, ObjectId} = chef_authz:create_resource(Superuser, object),
		{error, server_error} = chef_authz:delete_from_group(Superuser, ObjectId, actor, ActorId)
	    end}
      end]}.

get_acl_from_resource_test_() ->
    {foreach,
     fun() ->
             automeck:mocks(?AUTOMECK_FILE(get_acl)),
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
              {"get the acl for a newly created resource",
               fun() ->
                       {ok, GroupId} = chef_authz:create_resource(Superuser, group),
                       {ok, Acl} = chef_authz:get_acl_for_resource(Superuser, group, GroupId),
                       %%ACE should only contain the superuser
                       lists:foreach(fun(Ace) -> {_Method, #authz_ace{actors=[Superuser],groups=[]}} = Ace end, Acl)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get the acl for a non existient resource",
               fun() ->
                       %% Should this be remapped?
                       {error, server_error} = chef_authz:get_acl_for_resource(Superuser, group, ?no_such_id)
               end}
      end,
      fun({_Server, Superuser}) ->
              {"get the acl for a resource you don't have rights to",
               fun() ->
                       {ok, GroupId} = chef_authz:create_resource(Superuser, group),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       ?assertEqual({error, forbidden}, chef_authz:get_acl_for_resource(ActorId, group, GroupId))
               end}
      end]}.

get_ace_for_resource_test_() ->
    {foreach,
     fun() ->
             automeck:mocks(?AUTOMECK_FILE(get_ace)),
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
	  {"get the grant ace for a newly created resource",
           fun() ->
               {ok, GroupId} = chef_authz:create_resource(Superuser, group),
               {ok, Ace} = chef_authz:get_ace_for_resource(Superuser, group, GroupId, grant),
               #authz_ace{actors=[Superuser], groups=[]} = Ace
           end}
      end,
      fun({_Server, Superuser}) ->
          {"get the grant ace for a non existient resource",
           fun() ->
               ?assertEqual({error, not_found},
                            chef_authz:get_ace_for_resource(Superuser, group, ?no_such_id, grant))
           end}
      end,
      fun({_Server, Superuser}) ->
          {"get the grant ace for a resource you don't have rights to",
	    fun() ->
		{ok, GroupId} = chef_authz:create_resource(Superuser, group),
		{ok, ActorId} = chef_authz:create_resource(Superuser, actor),
		?assertEqual({error, forbidden}, chef_authz:get_ace_for_resource(ActorId, group, GroupId, grant))
	    end}
      end]}.

set_ace_of_resource_test_() ->
    {foreach,
     fun() ->
             test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(set_ace1)),
              {"set and get the grant ace for a newly created resource",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       Ace = #authz_ace{actors=lists:sort([ActorId,Superuser]), groups=[]},
                       ok = chef_authz:set_ace_for_resource(Superuser, object, ObjectId, grant, Ace),
                       {ok, Ace2} = chef_authz:get_ace_for_resource(Superuser, object, ObjectId, grant),
                       [] = Ace2#authz_ace.groups,
                       ?assert(Ace#authz_ace.actors == lists:sort(Ace2#authz_ace.actors))
               end}
      end,
      fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(set_ace2)),
              {"set the grant ace for a non-existient resource",
               fun() ->
                       Ace = #authz_ace{actors=[?no_such_id,Superuser], groups=[]},
                       ?assertEqual({error, not_found},
                                    chef_authz:set_ace_for_resource(Superuser, object, ?no_such_id, grant, Ace))
               end}
      end,
      fun({_Server, Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(set_ace3)),
              {"set the grant ace for a resource you don't have rights to",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       Ace1 = #authz_ace{actors=[ActorId,Superuser], groups=[]},
                       {error, forbidden} = chef_authz:set_ace_for_resource(ActorId, object, ObjectId, grant, Ace1),
                       {ok, Ace2} = chef_authz:get_ace_for_resource(Superuser, object, ObjectId, grant),
                       ?assertEqual(#authz_ace{actors=[Superuser], groups=[]}, Ace2)
               end}
      end]}.


is_authorized_on_resource_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized1)),
              {"check if the owner is authorized for grant on a newly created resource",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       ?assert(chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized1)),
              {"check that someone else is not authorized for grant on an newly created resource",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       ?assertNot(chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, actor, ActorId, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized2)),
              {"check that someone else can query permissions on an newly created resource",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       {ok, GroupId} = chef_authz:create_resource(Superuser, group),
                       ?assert(chef_authz:is_authorized_on_resource(ActorId, object, ObjectId, actor, Superuser, grant)),
                       ?assert(chef_authz:is_authorized_on_resource(ActorId, group, GroupId, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized2)),
              {"queries on a nonexistient object fail",
               fun() ->
                       %% would expect not_found
                       ?assertNot(chef_authz:is_authorized_on_resource(Superuser, object, ?no_such_id, actor, Superuser, grant))
               end}
      end,
      fun({_Server, Superuser}) ->
              automeck:mocks(?AUTOMECK_FILE(is_authorized3)),
              {"queries on a object of the wrong type fail",
               fun() ->
                       {ok, ObjectId} = chef_authz:create_resource(Superuser, object),
                       {ok, ActorId} = chef_authz:create_resource(Superuser, actor),
                       %% would expect server_error
                       ?assertNot(chef_authz:is_authorized_on_resource(Superuser, group, ObjectId, actor, ActorId, grant)),
                       ?assertNot(chef_authz:is_authorized_on_resource(Superuser, object, ObjectId, group, ActorId, grant))
               end}
      end]}.

get_container_aid_for_object_test_() ->
    {foreach,
     fun() -> automeck:mocks(?AUTOMECK_FILE(container_aid)),
              test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Context, _Superuser}) ->
              {"Can we get a real container",
               fun() ->
                       ObjectId = chef_authz:get_container_aid_for_object(Context, ?test_org_id, node),
                       ?assert(is_authz_id(ObjectId))
               end}
      end]}.

create_object_if_authorized_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
    [fun({Server, _Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(create_if_authorized1)),
             {"check if the admin user can create a new object (node)",
              fun() ->
                      AdminAID = <<"cf5d90545fbbac541225fbd9e73e94d9">>,
                      UserAID = <<"cf5d90545fbbac541225fbd9e73e4e42">>,
                      %% so bobo doesn't get created by setup test, probably only by running
                      %% features.
                      %% TODO: also need to decide on behavor for user_record_to_authz_id(not_found)
                      {ok, ObjectId} = chef_authz:create_object_if_authorized(Server, ?test_org_id, AdminAID, node),
                      ?assert(is_authz_id(ObjectId)),
                      %% the creator should have access
                      [ ?assert(chef_authz:is_authorized_on_resource(AdminAID, object, ObjectId, actor,
                                                                     AdminAID, Method)) || Method <- ?access_methods],
                      %% a regular user should not
                      [ ?assertNot(chef_authz:is_authorized_on_resource(UserAID, object, ObjectId, actor,
                                                                         UserAID, Method)) || Method <- ?access_methods]
              end}
     end,
     fun({Server, _Superuser}) ->
             automeck:mocks(?AUTOMECK_FILE(create_if_authorized2)),
             {"check that someone else is not authorized to create a new object",
              fun() ->
                      UserAID = <<"cf5d90545fbbac541225fbd9e73e4e42">>,
                      ?assertEqual({error, forbidden},
                                   chef_authz:create_object_if_authorized(Server, ?test_org_id, UserAID, node))
              end}
     end]}.

%% helper for tests
is_authz_id(Id) when is_binary(Id) ->
    case re:run(Id, "[0-9a-f]*", []) of
        {match, _} -> true;
        match -> true;
        nomatch -> false
    end;
is_authz_id(_Id) -> false.
