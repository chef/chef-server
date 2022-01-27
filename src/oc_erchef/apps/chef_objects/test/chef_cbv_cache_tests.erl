%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%%
%% Copyright Chef Software, Inc. All Rights Reserved.
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

-module(chef_cbv_cache_tests).

-include_lib("eunit/include/eunit.hrl").

start_cbv_cache(Enabled, TTL) ->
    application:set_env(chef_objects, cbv_cache_enabled, Enabled),
    application:set_env(chef_objects, cbv_cache_item_ttl, TTL),
    case Enabled of
        true ->
            {ok, _} = chef_cbv_cache:start_link();
        false ->
            % The gen_server is only started by supervisor when it is enabled.
            ok
    end.

cleanup_cache(_) ->
    case chef_cbv_cache:enabled() of
        true -> gen_server:stop(chef_cbv_cache);
        false -> ok
    end,
    application:set_env(chef_objects, cbv_cache_enabled, false),
    application:set_env(chef_objects, cbv_cache_item_ttl, 30000),
    ok.


chef_cbv_cache_disabled_responses_test_() ->
    {setup,
     fun() -> start_cbv_cache(false, 30000) end,
     fun cleanup_cache/1,
     [
      {"get returns undefined even after we put the key",
       fun() ->
               chef_cbv_cache:claim(value),
               chef_cbv_cache:put(value, "test"),
               ?assertEqual(undefined, chef_cbv_cache:get(value))
       end},
      {"put returns undefined",
       fun() -> ?assertEqual(undefined, chef_cbv_cache:put(value, "test")) end },
      {"claim returns undefined",
       fun() -> ?assertEqual(undefined, chef_cbv_cache:claim(value)) end }
     ]
    }.

claim_replies_retry_when_another_pid_has_claimed_first_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       spawn(fun() -> chef_cbv_cache:claim(other_key) end),
       timer:sleep(100), % Give the claim time to go through
       ?assertEqual({error, retry}, chef_cbv_cache:claim(other_key))
   end
  }.

put_does_not_return_busy_when_cache_queue_limit_exceeded_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       Pid = whereis(chef_cbv_cache),


       % Claim our key so we can test that 'put' works when it's busy
       % Not strictly required, but otherwise our put will fail and I'd rather test
       % the usage case here. (calling put without 'claim' is an error condition tested elsewhere).
       chef_cbv_cache:claim(my_key),

       % Prevent the cache from processing any further messages
       erlang:suspend_process(Pid, []),

       % Fill mailbox with junk messages to exceed the limit. We'll go well beyond the default
       % of 10, because want to make sure it doesn't drop below that limit after
       % we resume it
       [ Pid ! N || N <- lists:seq(21, 40) ],

       % Make sure our circuit breaker has a chance to see that the
       % queue is backed up
       timer:sleep(100), % QUEUE_LEN_REFRESH_INTERVAL

       % verify that rquests that use the breaker are failing
       ?assertEqual({error, busy}, chef_cbv_cache:get(my_key)),

       % Let it start processing the junk
       erlang:resume_process(Pid),
       % Meanwhile, we should be able to put our reserved key (it'll wait
       % for other messgaes to proces, but will allow ours through ignoring the limit)
       ?assertEqual(ok, chef_cbv_cache:put(my_key, <<"my value">>))
   end
  }.

get_and_claim_return_busy_when_cache_queue_limit_exceeded_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       Pid = whereis(chef_cbv_cache),

       % Prevent the cache from processing any messages
       erlang:suspend_process(Pid, []),

       % Fill mailbox with junk messages to exceed the limit.
       % The limit is currently defined as MAX_QUEUE_LEN (default 10) in chef_cbv_cache,
       % but we don't have access to that here.
       [ Pid ! N || N <- lists:seq(1, 15) ],

       % Make sure our circuit breaker has a chance to see that the
       % queue is backed up
       timer:sleep(100), % QUEUE_LEN_REFRESH_INTERVAL

       % Any calls should be stopped before a message is sent to chef_cbv_cache
       ?assertEqual({error, busy}, chef_cbv_cache:get(anything)),
       ?assertEqual({error, busy}, chef_cbv_cache:claim(anything)),
       erlang:resume_process(Pid)
   end
  }.

put_returns_error_when_claimed_by_another_pid_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       spawn(fun() -> chef_cbv_cache:claim(reserved_key) end),
       timer:sleep(100), % give time for that msg to process
       ?assertEqual({error, already_claimed}, chef_cbv_cache:put(reserved_key, <<"hello">>))
   end
  }.

put_returns_error_when_claim_not_called_first_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       ?assertEqual({error, no_claim}, chef_cbv_cache:put(unclaimed_key, <<"hello">>))
   end
  }.

get_returns_retry_when_another_proc_has_invoked_claim_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->

           spawn(fun() ->
                         chef_cbv_cache:claim(reserved_key),
                         % Sleep to keep this process alive for the duration -
                         % if it dies, the claim is released.
                         timer:sleep(200)
             end),
       timer:sleep(100),
       ?assertEqual({error, retry}, chef_cbv_cache:get(reserved_key))
   end
  }.
get_returns_undefined_when_another_proc_has_invoked_claim_and_died_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       spawn(fun() -> chef_cbv_cache:claim(reserved_key) end),
       timer:sleep(100),
       % This will return undefined because the process that claimed the key
       % died without putting a value before `get` was next called.
       ?assertEqual(undefined, chef_cbv_cache:get(reserved_key))
   end
  }.


get_returns_undefined_for_uncache_value_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       ?assertEqual(undefined, chef_cbv_cache:get(uncached_value))
   end
  }.

get_returns_value_for_cached_value_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       chef_cbv_cache:claim(my_value),
       chef_cbv_cache:put(my_value, <<"hello">>),
       ?assertEqual(<<"hello">>, chef_cbv_cache:get(my_value))
   end
  }.


get_returns_value_for_cached_value_from_another_pid_test_() ->
  {setup,
   fun() ->
       start_cbv_cache(true, 30000)
   end,
   fun cleanup_cache/1,
   fun() ->
       spawn(fun() ->
                 chef_cbv_cache:claim(their_value),
                 chef_cbv_cache:put(their_value, <<"hello">>)
             end),

       % Ensure there is time to process the request
       timer:sleep(100),
       ?assertEqual(<<"hello">>, chef_cbv_cache:get(their_value))
   end
  }.

