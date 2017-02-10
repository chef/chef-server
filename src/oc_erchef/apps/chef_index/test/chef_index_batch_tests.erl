%% Copyright 2015 Chef Server, Inc. All Rights Reserved.
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

-module(chef_index_batch_tests).
-include_lib("eunit/include/eunit.hrl").

chef_index_batch_test_() ->
    {foreach,
     fun() ->
             application:set_env(chef_index, search_batch_max_wait, 10000000),
             meck:new(chef_index, [passthrough]),
             {ok, Pid} = chef_index_batch:start_link(),
             Pid
     end,
     fun(_Pid) ->
             meck:unload(chef_index),
             stop_server()
     end,
     [{"current_size+wrapper_size is the actual size posted to solr",
       fun() ->
               chef_index_test_utils:set_provider(solr),
               add_item(<<"abcdefg">>),
               State = chef_index_batch:status(),
               CurrentSize = maps:get(current_size, State),
               WrapperSize = maps:get(wrapper_size, State),
               meck:expect(chef_index, update,
                           fun(solr, Payload) ->
                                   ActualSize = byte_size(iolist_to_binary(Payload)),
                                   ?assertEqual(CurrentSize+WrapperSize, ActualSize),
                                   ok
                           end),
               chef_index_batch:flush(),
               wait_for_res()
       end},
      {"chef_index_batch:stats returns a proplist of statistics",
       fun() ->
               chef_index_test_utils:set_provider(solr),
               meck:expect(chef_index, update, fun(solr, _Payload) -> ok end),
               add_item(<<"abcdefg">>),
               chef_index_batch:flush(),
               wait_for_res(),
               Stats = chef_index_batch:stats(),
               ?assertEqual(1, proplists:get_value(total_docs_queued, Stats)),
               ?assert(is_float(proplists:get_value(avg_queue_latency, Stats))),
               ?assert(is_float(proplists:get_value(avg_success_latency, Stats))),
               ?assertEqual(1, proplists:get_value(total_docs_success, Stats))
       end},
      {"chef_index_batch flushes an added item automatically",
       fun() ->
               chef_index_test_utils:set_provider(solr),
               meck:expect(chef_index, update, fun(solr, _Payload) -> ok end),
               application:set_env(chef_index, search_batch_max_wait, 10),
               restart_server(),
               add_item_no_wait(<<"abcdefg">>),
               ?assertEqual(ok, wait_for_res())
       end
      },
      {"chef_index_batch flushes when current_size+wrapper_size >= max_size",
       fun() ->
               chef_index_test_utils:set_provider(solr),
               meck:expect(chef_index, update, fun(solr, _Payload) -> ok end),
               application:set_env(chef_index, search_batch_max_size, 70),
               restart_server(),
               add_item_no_wait(<<"abcd">>), % The wrapper size is 66
               ?assertEqual(ok, wait_for_res())
       end
      }
     ]
    }.

stop_server() ->
    Pid = whereis(chef_index_batch),
    MRef = erlang:monitor(process, Pid),
    chef_index_batch:stop(),
    receive
        {'DOWN', MRef, _, _, _} ->
            ok
    end.

restart_server() ->
    stop_server(),
    chef_index_batch:start_link().

%% If you want to call this twice in one test, wait_for_added/0 has to be
%% adjusted: it currently only checks that the queue size gets from zero to
%% one (or rather, non-zero).
add_item(Item) ->
    add_item_no_wait(Item),
    wait_for_added().

add_item_no_wait(Item) ->
    Us = self(),
    spawn_link(
      fun() ->
              erlang:send(Us, started),
              Res = chef_index_batch:add_item(Item),
              erlang:send(Us, Res)
          end),
    wait_for_started().

%%
%% Async means never having to say you're sorry.
%%
wait_for_res() ->
    receive
        M ->
            M
    end.

wait_for_started() ->
    receive
        started ->
            ok
    end.

%% Note that this cannot work if the item added is bigger than max_size -- these
%% will be flushed immediately, and this _could_ have happened before we started
%% waiting/looping.
wait_for_added() ->
    case chef_index_batch:status() of
        #{ item_queue := [] } ->
            timer:sleep(10),
            wait_for_added();
        _ -> ok
    end.
