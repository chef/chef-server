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

% How long does each key live for?
-define(DEFAULT_TTL, 30000).

% How often (ms) to poll for the number of messages in the cbv_cache process queue.
% This needs to be kept low because under high volume a lot of requests can come in over a
% short period of time.  Keeping this number low allows us to catch that before overwhelming
% the process.
-define(QUEUE_LEN_REFRESH_INTERVAL, 100).
% Threshold for determining when to stop allowing requests to pass through to chef_cbv_cache
% Don't forget to change it in chef_cbv_cache_test if you change it here.
-define(MAX_QUEUE_LEN, 10).

-record(state, { tid = undefined, ttl = ?DEFAULT_TTL, enabled = false, claims = undefined }).
-export([
         start_link/0,
         get/1,
         claim/1,
         put/2,
         %% Debug and testing
         force_breaker/1,
         stop/0
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
    Enabled = envy:get(chef_objects, cbv_cache_enabled, false, boolean),
    TTL = envy:get(chef_objects, cbv_cache_item_ttl, ?DEFAULT_TTL, integer),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Enabled, TTL], []).

%% @doc this function retrieves a stored value from the cache.
%% If the value is missing, it will return undefined.
%% If another caller has claimed intent to populate the key, this will return
%% 'retry' indicating that the caller should try again after a brief delay.
%% If it returns 'busy' the process is not available to service the request, and
%% the caller should fail without retrying.
-spec get(any()) -> retry | busy | undefined | term().
get(Key) ->
    send_if_available({get, Key}).

%% @doc claim intent to populate Key into the cache.
%% When the claim is accepted, returns 'ok'.
%% When another process has already claimed intent, this returns 'retry' indicating
%% that the caller should retry the original 'get' request after a brief delay.
%% If it returns 'busy' the process is not available to service the request, and
%% the caller should fail without retrying.
-spec claim(any()) -> ok | retry | busy.
claim(Key) ->
    send_if_available({claim, Key}).

%% @doc store a value in the cache.  Caller must have first claimed intent to populate
%% this key by invoking claim/1.
%% If the caller has not first invoked claim/1, this will return {error, noclaim}.
%% If another process has invoked claim/1, this will return {error, already_claimed}.
%% returns 'ok' when the value is stored successfully
-spec put(any(), term()) -> {error, already_claimed} | {error, noclaim} | ok.
put(Key, Value) ->
    % We do not protect this with the breaker - this function is only to be called
    % after a successful 'claim', and we want to allow caller to put the claim
    % instead of `busy` response which would cause another caller to have to claim,
    % do the work, then put the result.
    gen_server:call(?SERVER, {put, Key, Value}).

%% Internal function that checks in with the process message queue for chef_cbv_cache intermittently
%% and tracks whether its mailbox has grown to large. If that happens, response with 'busy' instead
%% of sending the request through.
send_if_available(Msg) ->
    case breaker_tripped() of
        true -> busy;
        false -> gen_server:call(?SERVER, Msg)
    end.

%% @doc intended for testing.
stop() ->
    gen_server:call(?SERVER, stop).

% gen_server implementation

init([Enabled, TTL]) ->
    process_flag(trap_exit, true),
    spawn_breaker(),
    {ok, #state{enabled = Enabled,
                ttl = round(TTL/2), % Splay is TTL/2 + (0-50% of TTL) for max of TTL.
                claims = dict:new(),
                tid = ets:new(chef_cbv_cache, [set, private, {read_concurrency, true}])
               }}.
handle_call(stop, _From, State) ->
    exit(whereis(cbv_cache_breaker), kill),
    {stop, normal, ok, State};
handle_call(_Msg, _From, #state{enabled = false} = State) ->
    {reply, undefined, State};
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
                true -> {reply, retry, State};
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
            {reply, retry, State}
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
            % one caller from trying to put the same key, this should rarely occur
            % (may happen if caller who invoked claim/1 then dies before put/2)
            lager:info("chef_cbv_cache: Key ~p already present, ignoring.", [Key])
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
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal implementation

spawn_breaker() ->
    spawn_link(fun() -> breaker(0, false) end) ! init.

breaker_tripped() ->
  whereis(cbv_cache_breaker) ! { self(), check_limit },
  receive
    { result, limit_reached } ->
      true;
    { result, ok } ->
      false
  end.

force_breaker(Disable) ->
  whereis(cbv_cache_breaker) ! { force_disable, Disable }.

% A circuit breaker that is spawn_linked to
% cbv_cache, and monitors the state of the cache queue.
% This prevents the overhead of checking the queue size on every request
breaker(Count, Disabled) ->
  receive
    {Sender, check_limit} when Count >= ?MAX_QUEUE_LEN orelse Disabled =:= true ->
      Sender ! {result, limit_reached},
      breaker(Count, Disabled);
    {Sender, check_limit} ->
      Sender ! {result, ok},
      breaker(Count, Disabled);
    {force_disable, Disabled2} ->
      breaker(Count, Disabled2);
    init ->
      register(cbv_cache_breaker, self()),
      self() ! update,
      breaker(Count, Disabled);
    update ->
      case whereis(chef_cbv_cache) of
        undefined ->
          Count2 = Count;
        Pid ->
          {_, Count2} = process_info(Pid, message_queue_len)
      end,
      erlang:send_after(?QUEUE_LEN_REFRESH_INTERVAL, self(), update),
      breaker(Count2, Disabled)
  end.
