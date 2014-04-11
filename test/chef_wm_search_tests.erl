%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et


-module(chef_wm_search_tests).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

search_test_() ->
    {foreach,
     fun() ->
             meck:new(chef_solr)
     end,
     fun(_) ->
             meck:unload([chef_solr])
     end,
     [
      {"Catch exception and return",
       fun() ->
               meck:expect(chef_solr, search,
                           fun(solr_query, url) ->
                                   erlang:error(any_error)
                           end
                          ),

               ?assertMatch({_, {error, any_error}},exec_chef_solr())
       end},
       {"Catch throw and return",
       fun() ->
               meck:expect(chef_solr, search,
                           fun(solr_query, url) ->
                                   erlang:throw({other, any_error})
                           end
                          ),

               ?assertMatch({_, {throw, {other, any_error}}},exec_chef_solr())
       end},
       {"Catch exit and return",
       fun() ->
               meck:expect(chef_solr, search,
                           fun(solr_query, url) ->
                                   erlang:exit(any_error)
                           end
                          ),

               ?assertMatch({_, {exit, any_error}},exec_chef_solr())
       end},

      {"Return result when no error",
       fun() ->
               meck:expect(chef_solr, search,
                           fun(solr_query, url) ->
                                   result
                           end
                          ),
               meck:expect(stats_hero, ctime, fun(_, _, Fun) ->
                                                      Fun()
                                              end),
               ?assertMatch({_, result},exec_chef_solr())

       end}
     ]}.

exec_chef_solr() ->
    begin
        chef_wm_search:spawn_solr_query(label, url, solr_query, req_id),
        receive
            Val ->
                Val
        end
    end.
    
