%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et


-module(chef_wm_search_tests).

-include_lib("eunit/include/eunit.hrl").

search_test_() ->
    {foreach,
     fun() ->
             meck:new([chef_index, stats_hero])
     end,
     fun(_) ->
             meck:unload([chef_index, stats_hero])
     end,
     [
      {"Catch exception and return",
       fun() ->
               meck:expect(chef_index, search,
                           fun(solr_query) ->
                                   erlang:error(any_error)
                           end
                          ),

               ?assertMatch({error, any_error},exec_chef_solr())
       end},
       {"Catch throw and return",
       fun() ->
               meck:expect(chef_index, search,
                           fun(solr_query) ->
                                   erlang:throw({other, any_error})
                           end
                          ),

               ?assertMatch({throw, {other, any_error}},exec_chef_solr())
       end},
       {"Catch exit and return",
       fun() ->
               meck:expect(chef_index, search,
                           fun(solr_query) ->
                                   erlang:exit(any_error)
                           end
                          ),

               ?assertMatch({exit, any_error},exec_chef_solr())
       end},

      {"Return result when no error",
       fun() ->
               meck:expect(chef_index, search,
                           fun(solr_query) ->
                                   result
                           end
                          ),
               meck:expect(stats_hero, ctime, fun(_, _, Fun) ->
                                                      Fun()
                                              end),
               ?assertMatch(result,exec_chef_solr())
       end}
     ]}.

exec_chef_solr() ->
    chef_wm_search:solr_search(solr_query).
