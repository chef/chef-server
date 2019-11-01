-module(oc_chef_authz_cleanup_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(SUPER_USER_AUTHZ_ID, <<"clarkkent">>).
-define(BATCH_SIZE, 10).
-define(INTERVAL, 10).

oc_chef_authz_cleanup_test_() ->
    Mods = [ oc_chef_authz_http ],
    {foreach,
     fun() ->
             oc_chef_authz_tests:start_apps(),             
             application:set_env(oc_chef_authz, cleanup_interval, ?INTERVAL),
             application:set_env(oc_chef_authz, cleanup_batch_size, ?BATCH_SIZE),
             application:set_env(oc_chef_authz, authz_superuser_id, ?SUPER_USER_AUTHZ_ID),
             %% Cancel the current timer so we can test state transitions individually
             oc_chef_authz_cleanup:stop(),
             [ meck:new(Mod) || Mod <- Mods]                
     end,
     fun(_) ->
             oc_chef_authz_tests:stop_apps(),
             [ meck:unload(Mod) || Mod <- Mods ]
     end,
     [
      {"initial state defaults to empty sets",
       fun() ->
               ?assertEqual({sets:new(), sets:new()}, oc_chef_authz_cleanup:get_authz_ids())
       end},
      {"add_authz_ids should union sets",
       fun() ->
               oc_chef_authz_cleanup:add_authz_ids(to_binary([1,2]), to_binary([3,4])),
               ?assertEqual({sets:from_list(to_binary([1,2])), sets:from_list(to_binary([3,4]))}, oc_chef_authz_cleanup:get_authz_ids()),
               oc_chef_authz_cleanup:add_authz_ids(to_binary([1,2,3,4]), to_binary([1,2,3,4])),
               ?assertEqual({sets:from_list(to_binary([1,2,3,4])), sets:from_list(to_binary([1,2,3,4]))}, oc_chef_authz_cleanup:get_authz_ids()),
               oc_chef_authz_cleanup:add_authz_ids(to_binary([5]), to_binary([5])),
               ?assertEqual({sets:from_list(to_binary([1,2,3,4,5])), sets:from_list(to_binary([1,2,3,4,5]))}, oc_chef_authz_cleanup:get_authz_ids())
       end},
      {"prune should remove " ++ integer_to_list(?BATCH_SIZE) ++ " items from actors and groups",
       fun() ->
               oc_chef_authz_cleanup:stop(),
               Actors = to_binary(lists:seq(0,?BATCH_SIZE)),
               Groups = to_binary(lists:seq(0,?BATCH_SIZE)),
               oc_chef_authz_cleanup:add_authz_ids(Actors, Groups),
               {StoredActors, _StoredGroups} = oc_chef_authz_cleanup:get_authz_ids(),
               {ActorsToBeDeleted, ActorsToBeRemaining} = oc_chef_authz_cleanup:prune(?BATCH_SIZE, sets:to_list(StoredActors)),
               expect_delete(ActorsToBeDeleted, ActorsToBeDeleted),
               oc_chef_authz_cleanup:prune(),
               {ResultingActorSet, ResultingGroupSet} = oc_chef_authz_cleanup:get_authz_ids(),
               ?assertEqual(ActorsToBeRemaining, sets:to_list(ResultingActorSet)),
               ?assertEqual(ActorsToBeRemaining, sets:to_list(ResultingGroupSet))
       end},
      {"prune should remove 1 item from actors and groups",
       fun() ->
               oc_chef_authz_cleanup:stop(),
               oc_chef_authz_cleanup:add_authz_ids(to_binary([1]),to_binary([1])),
               expect_delete(to_binary(lists:seq(0, ?BATCH_SIZE-1)), to_binary(lists:seq(0, ?BATCH_SIZE-1))),
               oc_chef_authz_cleanup:prune(),
               {ResultingActorSet, ResultingGroupSet} = oc_chef_authz_cleanup:get_authz_ids(),
               ?assertEqual(sets:from_list([]), ResultingActorSet),
               ?assertEqual(sets:from_list([]), ResultingGroupSet)
       end},
      {"add_authz_ids should delete from bifrost after interval when smaller than batch size",
      fun() ->
              Actors = [ test_utils:make_az_id(integer_to_list(In)) || In <- lists:seq(0,3) ],
              Groups = [ test_utils:make_az_id(integer_to_list(In)) || In <- lists:seq(0,3) ],
              expect_delete(Actors, Groups),
              
              oc_chef_authz_cleanup:add_authz_ids(Actors, Groups),
              oc_chef_authz_cleanup:start(),
              timer:sleep(30)
      end

      }
     ]}.

convert_to_path(Actors, Groups) ->
   ActorBasePath = "actors/",
   GroupBasePath = "groups/",

   [ActorBasePath ++ binary_to_list(Actor) || Actor <- Actors] ++
        [GroupBasePath ++ binary_to_list(Group) || Group <- Groups].

to_binary(List) ->
    [list_to_binary(integer_to_list(Int)) || Int <- List].

expect_delete(Actors, Groups) ->
    meck:delete(oc_chef_authz_http, request, 5),
    meck:expect(oc_chef_authz_http, request,
                fun(InputPath, delete, [], [], AzId) ->
                        PossiblePaths = convert_to_path(Actors, Groups),
                        FilteredPaths = lists:filter(fun(Elem) ->
                                                             Elem =:= InputPath
                                                     end, PossiblePaths),
                        ?assertEqual([InputPath], FilteredPaths),
                        ?assertEqual(AzId, ?SUPER_USER_AUTHZ_ID)
                end).
