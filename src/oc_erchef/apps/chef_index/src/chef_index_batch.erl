%% Copyright 2015 Chef Software, Inc
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

-module(chef_index_batch).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
         add_item/1,
         flush/0,
         start_link/0,
         status/0,
         stats/0,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(chef_idx_batch_state, {
          wrapper_size :: non_neg_integer(),
          current_size :: non_neg_integer(),
          max_size :: non_neg_integer(),
          item_queue = [] :: [{{pid(), term()}, erlang:timestamp(), iolist()}],
          max_wait :: non_neg_integer(),
          search_provider = solr :: solr|elasticsearch,
          total_docs_queued = 0 :: integer(),
          total_docs_success = 0 :: integer(),
          avg_queue_latency = 0.0 :: float(),
          avg_success_latency = 0.0 :: float()
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_item(iolist()) -> term().
add_item(Doc) ->
    Size = byte_size(iolist_to_binary(Doc)),
    Timeout = envy:get(chef_index, add_item_timeout, 30000),
    gen_server:call(?MODULE, {add_item, Doc, Size}, Timeout).

flush() ->
    gen_server:cast(?MODULE, flush).

status() ->
    gen_server:call(?MODULE, status).

stats() ->
    gen_server:call(?MODULE, stats).

stop() ->
    gen_server:call(?MODULE, stop).

wrap_solr(Docs) ->
    [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
     <<"<update>">>,
     <<"<add>">>,
     Docs,
     <<"</add>">>,
     <<"</update>">>].

-spec wrap(iolist(), #chef_idx_batch_state{}) -> iolist().
wrap(Docs, #chef_idx_batch_state{search_provider = solr}) ->
    wrap_solr(Docs);
wrap(Docs, #chef_idx_batch_state{search_provider = elasticsearch}) ->
    Docs.

-spec wrapper_size(#chef_idx_batch_state{}) -> non_neg_integer().
wrapper_size(State) ->
    byte_size(iolist_to_binary(wrap([], State))).

-spec state_to_map(#chef_idx_batch_state{}) -> map().
state_to_map(State) ->
    #{wrapper_size => State#chef_idx_batch_state.wrapper_size,
      current_size => State#chef_idx_batch_state.current_size,
      max_size => State#chef_idx_batch_state.max_size,
      item_queue => State#chef_idx_batch_state.item_queue,
      max_wait => State#chef_idx_batch_state.max_wait,
      search_provider => State#chef_idx_batch_state.search_provider,
      total_docs_queued => State#chef_idx_batch_state.total_docs_queued,
      total_docs_success => State#chef_idx_batch_state.total_docs_success,
      avg_queue_latency => State#chef_idx_batch_state.avg_queue_latency,
      avg_success_latency => State#chef_idx_batch_state.avg_success_latency
     }.

init([]) ->
    MaxSize = envy:get(chef_index, search_batch_max_size, 5000000, integer),
    case MaxSize =< 0 of
        true ->
            {stop, list_to_binary([<<"chef_index batch_max_size is set to ">>, integer_to_binary(MaxSize),
                                   <<". Please set to non-negative value, or set search_queue_mode to something besides batch.">>])};
        false ->
            SearchProvider = envy:get(chef_index, search_provider, solr, envy:one_of([solr, elasticsearch])),
            MaxWait = envy:get(chef_index, search_batch_max_wait, 10, non_neg_integer),
            WrapperSize = wrapper_size(#chef_idx_batch_state{search_provider=SearchProvider}),
            CurrentSize = 0,
            spawn_flusher(MaxWait),
            {ok, #chef_idx_batch_state{wrapper_size = WrapperSize,
                                       current_size = CurrentSize,
                                       max_size = MaxSize,
                                       max_wait = MaxWait,
                                       search_provider = SearchProvider}}
    end.

spawn_flusher(Time) ->
    %% FIXME: this process will stay around even after someone calls
    %% chef_index_batch:stop(). It would be nice if this process also
    %% died on stop. Potentially we could send a message to it from
    %% the parent in terminate() and receive that message here
    %% or..something.
    spawn_link(
      fun Flusher() ->
              timer:sleep(Time),
              gen_server:cast(?MODULE, flush),
              Flusher()
      end
     ).

-spec flush(#chef_idx_batch_state{}) -> #chef_idx_batch_state{}.
flush(State = #chef_idx_batch_state{item_queue = []}) ->
    State;
flush(State = #chef_idx_batch_state{item_queue = Queue,
                                    current_size = CurrentSize,
                                    search_provider = Provider,
                                    wrapper_size = WrapperSize}) ->
    {PidsToReply, Timestamps, DocsToAdd} = lists:unzip3(Queue),
    Doc = wrap(DocsToAdd, State),
    Self = self(),
    spawn(
      fun() ->
              lager:debug("Batch posting to ~s ~p documents (~p bytes)", [Provider, length(DocsToAdd), CurrentSize+WrapperSize]),
              Now = os:timestamp(),
              Res = chef_index:update(Provider, Doc),
              Now1 = os:timestamp(),
              TotalDocs = length(Timestamps),
              {BeforeDiff, AfterDiff} =
                  lists:foldl(
                    fun(T, {BeforePost, AfterPost}) ->
                            {BeforePost + diff_times(Now, T),
                             AfterPost + diff_times(Now1, T)}
                    end,
                    {0, 0},
                    Timestamps),
              gen_server:cast(Self, {stats_update, TotalDocs, {BeforeDiff/TotalDocs, AfterDiff/TotalDocs}, Res}),
              [gen_server:reply(From, Res) || From <- PidsToReply]
      end),
    State#chef_idx_batch_state{item_queue = [], current_size=0}.

handle_call({add_item, Doc, Size}, From,
            State = #chef_idx_batch_state{max_size = MaxSize,
                                          current_size = CurrentSize,
                                          wrapper_size = WrapperSize})
  when (Size + CurrentSize + WrapperSize) > MaxSize  ->
    State1 = #chef_idx_batch_state{item_queue=Queue} = flush(State#chef_idx_batch_state{current_size = CurrentSize + Size}),
    {noreply, State1#chef_idx_batch_state{item_queue = [{From, os:timestamp(), Doc}| Queue]}};
handle_call({add_item, Doc, Size}, From, State = #chef_idx_batch_state{item_queue=Queue, current_size=CurrentSize}) ->
    {noreply, State#chef_idx_batch_state{item_queue = [{From, os:timestamp(), Doc}|Queue],
                                         current_size = CurrentSize + Size
                                        }};
handle_call(status, _From, State) ->
    {reply, state_to_map(State), State};
handle_call(stats, _From, State = #chef_idx_batch_state{avg_queue_latency = OQL,
                                                        avg_success_latency = OSL,
                                                        total_docs_queued = TQ,
                                                        total_docs_success = TS
                                                       }) ->
    Stats = [
             {avg_queue_latency, OQL},
             {avg_success_latency, OSL},
             {total_docs_queued, TQ},
             {total_docs_success, TS}
            ],
    {reply, Stats, State};
handle_call(stop, From, State) ->
    lager:info("Stop requested from ~p", [From]),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({stats_update, TotalDocs, {AvgQueueLatency,AvgSuccessLatency}, Resp},
            State = #chef_idx_batch_state{avg_queue_latency = OQL,
                                          avg_success_latency = OSL,
                                          total_docs_queued = TQ,
                                          total_docs_success = TS
                                         }) ->
    State1 = State#chef_idx_batch_state{total_docs_queued = TQ + TotalDocs,
                                        avg_queue_latency = ((AvgQueueLatency*TotalDocs)+(OQL*TQ))/(TQ+TotalDocs)
                                       },
    case Resp of
        ok ->
            {noreply, State1#chef_idx_batch_state{
                        total_docs_success = TS+TotalDocs,
                        avg_success_latency = ((AvgSuccessLatency*TotalDocs)+(OSL*TS))/(TS+TotalDocs)
                       }
            };
        _ ->
            {noreply, State1}
    end;
handle_cast(flush, State) ->
    State1 = flush(State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

diff_times({EM, ES, EI}, {SM, SS, SI}) ->
    ((EM - SM) * 1000000 + (ES - SS)) * 1000 + ((EI - SI) div 1000).
