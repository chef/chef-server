%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% @doc Cache module for caching solved and formatted cookbook results.
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

-module(chef_cbv_cache).
-behavior(gen_server).

-define(SERVER, ?MODULE).

% How long does each key live for (max) in ms?
-define(DEFAULT_TTL, 30000).

% How often (ms) to poll for the number of messages in the cbv_cache process queue.
% This needs to be kept low because under high volume a lot of requests can come in over a
% short period of time.  Keeping this number low allows us to catch that before overwhelming
% the process
-define(QUEUE_LEN_REFRESH_INTERVAL, 100).

% Threshold for determining when to stop allowing requests to pass through to chef_cbv_cache
% Don't forget to change it in chef_cbv_cache_test if you change it here.
-define(MAX_QUEUE_LEN, 10).

-record(state, { tid = undefined, ttl = ?DEFAULT_TTL, claims = undefined }).
-export([
         start_link/0,
         get/1,
         claim/1,
         put/2,
         enabled/0
        ]).

%% gen_server callbacks
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc this function retrieves a stored value from the cache.
%% If the value is missing, or the cache is disabled, it will return undefined.
%% If another caller has claimed intent to populate the key, this will return
%% '{error, retry}' indicating that the caller should try again after a brief delay.
%% If it returns '{error, busy}' the process is not available to service the request, and
%% the caller should fail without retrying.
%%
%% Returns undefined if the cache is disabled.
%%
%% Future Improvements:
%%  We can eke a little more out of this by allowing clients to directly
%%  fetch from the ets table (within the get/1 API), without needing to post a message
%%  to chef_cbv_cache.
%%
%%  Currently funnelling everything through the gen_server queue ensures an order of
%%  operations, where only one caller ultimately calculates the results.
%%  Allowing reads directly from ETS would introduce a race condition, where:
%%  Caller A ->  does  get and finds no result, claims the key
%%  Caller B -> in parallel puts a claimed result into the cache
%%  Caller A -> assembles the result that B had just assembled and puts it to cache
%%  Under high load, there could be multiple 'caller a' behaviors in this scenario.
%%
%%  We can prevent this by having `claim` check for an existing entry in the cache in addition to
%%  an existing claim. If an entry is found, it will return `{error, retry}` just as if a claim were present;
%%  in the scenario above 'Caller A' would get `{error, retry}` in response to `claim` and would
%%  retry normally, getting the cached result in the next attempt.
%%
%%  Testing under heavy load has shown that the current solution performs very well as-is,
%%  with load-shedding working as intended when the queue reaches capacity.
%%  These changes only make sense if we hit new usage patterns that overwhelm the
%%  cbv_cache message queue on a regular basis.
%%
-spec get(any()) -> {error, retry|busy} | undefined | term().
get(Key) ->
    send_if_available({get, Key}).

%% @doc claim intent to populate Key into the cache.
%% When the claim is accepted, returns 'ok'.
%% When another process has already claimed intent, this returns 'retry' indicating
%% that the caller should retry the original 'get' request after a brief delay.
%% If it returns '{error, busy}' the process is not available to service the request, and
%% the caller should fail without retrying.
%% Returns undefined if the cache is disabled.
-spec claim(any()) -> ok | {error, retry|busy} | undefined.
claim(Key) ->
    send_if_available({claim, Key}).

%% @doc store a value in the cache.  Caller must have first claimed intent to populate
%% this key by invoking claim/1.
%%
%% This call is not protected with the breaker - this function is only to be called
%% after a successful 'claim', and we want to prioritize putting the claim even if
%% it puts us past the queue limit, since that is likely to free other processes from
%% polling against a '{error, retry}'.
%% If the caller has not first invoked claim/1, this will return {error, noclaim}.
%% If another process has invoked claim/1, this will return {error, already_claimed}.
%% returns 'ok' when the value is stored successfully
%% Returns undefined if the cache is disabled.
-spec put(any(), term()) -> {error, already_claimed} | {error, noclaim} | ok | undefined.
put(Key, Value) ->
    case enabled() of
        true -> gen_server:call(?SERVER, {put, Key, Value});
        false -> undefined
    end.

-spec enabled() -> true | false.
enabled() ->
    envy:get(chef_objects, cbv_cache_enabled, false, boolean).

%% Internal function that checks in with the process message queue for chef_cbv_cache intermittently
%% and tracks whether its mailbox has grown to large. If that happens, response with '{error, busy}' instead
%% of sending the request through.
send_if_available(Msg) ->
    case enabled() of
        true ->
            case breaker_tripped() of
                true -> {error, busy};
                false -> gen_server:call(?SERVER, Msg)
            end;
        false ->
            undefined
    end.


% gen_server implementation

init([]) ->
    TTL = envy:get(chef_objects, cbv_cache_item_ttl, ?DEFAULT_TTL, integer),
    process_flag(trap_exit, true),
    spawn_breaker(),
    {ok, #state{ttl = round(TTL/2), % Splay is TTL/2 + (0-50% of TTL) for max of TTL.
                claims = dict:new(),
                tid = ets:new(chef_cbv_cache, [set, private, {read_concurrency, true}])
               }}.
handle_call({get, Key}, _From, #state{tid = Tid, claims = Claims} = State) ->
    case dict:take(Key, Claims) of
        error ->
            % Nobody is currently working on it, so it's either here or not.
            case ets:lookup(Tid, Key) of
                [{_, Value}] -> {reply, Value, State};
                [] -> {reply, undefined, State}
            end;
        {Pid, Claims1} ->
            case is_process_alive(Pid) of
                true -> {reply, {error, retry}, State};
                false ->
                    % If the original claiming process has died, it can't complete its
                    % claim.
                    case ets:lookup(Tid, Key) of
                        [{_, Value}] -> {reply, Value, State};
                        [] -> {reply, undefined, State#state{claims = Claims1}}
                    end
            end
    end;
handle_call({claim, Key}, {From, _Tag}, #state{claims = Claims} = State) ->
    case dict:take(Key, Claims) of
        error -> % Nobody has claimed it yet
            {reply, ok, State#state{claims = dict:store(Key, From, Claims)}};
        {From, _Claims1} ->
            % This caller has already claimed it, that's fine..
            {reply, ok, State};
        {_OtherPid, _Claims1} -> % Someone has already claimed it, so caller should retry the GET
            {reply, {error, retry}, State}
    end;
handle_call({put, Key, Value}, { From, _Tag }, #state{tid = Tid, ttl = TTL, claims = Claims} = State) ->
    case dict:take(Key, Claims) of
        error ->
            % Mis-usage. Claim must be called before put.
            {reply, {error, no_claim}, State};
        { From, Claims1 } ->
            % It is claimed and by this caller, they can go ahead and put.
            % Update claims dictionary in state to remove the claim on this key
            insert_into_cache(Tid, Key, Value, TTL),
            {reply, ok, State#state{claims = Claims1}};
        { _OtherPid, _Claims1 } ->
            % Mis-usage: Someone else claimed it and is still active/working on it,
            % but we have been invoked anyway. `put` should only be invoked when the caller
            % has first invoked `claim`.
            {reply, {error, already_claimed}, State }
    end.

handle_info({expire, Key}, #state{tid = Tid} = State) ->
    ets:delete(Tid, Key),
    {noreply, State};
handle_info({'EXIT', _From, Reason}, State) ->
    lager:error("chef_cbv_cache: circuit breaker proc failed because ~p, restarting", [Reason]),
    spawn_breaker(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    case whereis(cbv_cache_breaker) of
        undefined  ->
            ok;
        Pid ->
            exit(Pid, kill)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal implementation
%%

insert_into_cache(Tid, Key, Value, TTL) ->
    case ets:insert_new(Tid, {Key, Value}) of
        true ->
            % - we see that often the pattern is that many requests for new keys
            % come in clusters when traffic is first directed to the server.
            % We can't avoid the initial cluster and corresponding CPU spike
            % when we have nothing cached, but staggering the expirations after that
            % will reduce CPU/VM utilization spikes that can impact the overall system.
            erlang:send_after(TTL + rand:uniform(TTL), self(), {expire, Key});
        false ->
            % Value already exists.  Given the enforced ordering to prevent more than
            % one caller from trying to put the same key, this should no longer be possible
            lager:info("chef_cbv_cache: Key ~p already present, ignoring.", [Key])
    end.

spawn_breaker() ->
    spawn_link(fun() -> breaker(0) end) ! init.

breaker_tripped() ->
  whereis(cbv_cache_breaker) ! { self(), check_limit },
  receive
    { result, limit_reached } -> true;
    { result, ok } -> false
  end.

% A circuit breaker that is spawn_linked to
% cbv_cache, and monitors the state of the cache queue.
% This prevents the overhead of checking the queue size on every request
breaker(Count) ->
  receive
    {Sender, check_limit} when Count >= ?MAX_QUEUE_LEN ->
      Sender ! {result, limit_reached},
      breaker(Count);
    {Sender, check_limit} ->
      Sender ! {result, ok},
      breaker(Count);
    init ->
      register(cbv_cache_breaker, self()),
      self() ! update,
      breaker(Count);
    update ->
      NewCount = case whereis(chef_cbv_cache) of
        undefined -> Count;
        Pid ->
          {_, Count0} = process_info(Pid, message_queue_len),
          Count0
      end,
      erlang:send_after(?QUEUE_LEN_REFRESH_INTERVAL, self(), update),
      breaker(NewCount)
  end.
