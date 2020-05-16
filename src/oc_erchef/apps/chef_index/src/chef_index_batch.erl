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
          wrapper_size :: non_neg_integer() | undefined,
          current_size :: non_neg_integer() | undefined,
          max_size :: non_neg_integer() | undefined,
          item_queue = [] :: [{{pid(), term()}, erlang:timestamp(), iolist()}],
          max_wait :: non_neg_integer() | undefined,
          search_provider = solr :: solr | elasticsearch,
          total_docs_queued = 0 :: integer(),
          total_docs_success = 0 :: integer(),
          avg_queue_latency = 0.0 :: float(),
          avg_success_latency = 0.0 :: float()
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_item(iolist()) -> term().
add_item(Doc) ->
    case chef_index_limiter:take_ticket() of
        ok ->
            AddedTime = erlang:monotonic_time(),
            Size = byte_size(iolist_to_binary(Doc)),
            gen_server:call(?MODULE, {add_item, Doc, Size, AddedTime});
        E ->
            E
    end.

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

%%show the state in prometheus
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

            prometheus_gauge:declare([{name, chef_index_batch_current_batch_size_bytes},
                                      {help, "The size (in bytes) of the batch currently being constructed"}]),
            prometheus_gauge:declare([{name, chef_index_batch_current_batch_doc_count},
                                      {help, "The number of documents in the batch currently being constructed"}]),
            prometheus_gauge:declare([{name, chef_index_batch_inflight_flushes_count},
                                      {help, "The number of inflight requests to the search index"}]),
            prometheus_gauge:declare([{name, chef_index_batch_mailbox_size},
                                      {help, "The number of items in the chef_index_batch process mailbox"}]),
            prometheus_gauge:declare([{name, chef_index_batch_heap_size_bytes},
                                      {help, "The number of bytes in the chef_index_batch process heap"}]),
            prometheus_gauge:declare([{name, chef_index_batch_stack_size_bytes},
                                      {help, "The number of bytes in the chef_index_batch process stack"}]),
            prometheus_gauge:declare([{name, chef_index_batch_memory_size_bytes},
                                      {help, "The number of bytes in use by the chef_index_batch process"}]),


            prometheus_counter:declare([{name, chef_index_batch_successful_docs_total},
                                        {help, "The total number of documents that chef_index_batch has successfully processed"}]),
            prometheus_counter:declare([{name, chef_index_batch_failed_docs_total},
                                        {help, "The total number of documents that chef_index_batch failed to process"}]),

            prometheus_histogram:declare([{name, chef_index_batch_queue_latency_ms},
                                          {help, "The amount of time (in ms) that a document spent waiting in chef_index_batch (and its mailbox) before attempting to send it to the search index"},
                                          {buckets, chef_index:histogram_buckets()}]),
            prometheus_histogram:declare([{name, chef_index_batch_completed_latency_ms},
                                          {help, "The amount of time (in ms) that a document took to be processed (includes the queue time)"},
                                          {buckets, chef_index:histogram_buckets()}]),
            collect_process_info(),
            chef_index_limiter:setup(),
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
              prometheus_gauge:inc(chef_index_batch_inflight_flushes_count),
              lager:debug("Batch posting to ~s ~p documents (~p bytes)", [Provider, length(DocsToAdd), CurrentSize+WrapperSize]),
              Now = erlang:monotonic_time(),
              Res = chef_index:update(Provider, Doc),
              Now1 = erlang:monotonic_time(),
              TotalDocs = length(Timestamps),
              {BeforeDiff, AfterDiff} =
                  lists:foldl(
                    fun(T, {BeforePost, AfterPost}) ->
                            BeforePostOne = time_diff_in_ms(Now, T),
                            AfterPostOne = time_diff_in_ms(Now1, T),
                            prometheus_histogram:observe(chef_index_batch_queue_latency_ms, BeforePostOne),
                            prometheus_histogram:observe(chef_index_batch_completed_latency_ms, AfterPostOne),
                            {BeforePost + BeforePostOne,
                             AfterPost + AfterPostOne}
                    end,
                    {0, 0},
                    Timestamps),
              gen_server:cast(Self, {stats_update, TotalDocs, {BeforeDiff/TotalDocs, AfterDiff/TotalDocs}, Res}),
              [gen_server:reply(From, Res) || From <- PidsToReply],
              prometheus_gauge:dec(chef_index_batch_inflight_flushes_count)
      end),
    State#chef_idx_batch_state{item_queue = [], current_size=0}.

handle_call({add_item, Doc, Size, AddedTime}, From,
            State = #chef_idx_batch_state{max_size = MaxSize,
                                          current_size = CurrentSize,
                                          wrapper_size = WrapperSize})
  when (Size + CurrentSize + WrapperSize) > MaxSize  ->
    %% If adding this document would have overflowed the max_size,
    %% send what we have and add the document to the next batch.
    State1 = #chef_idx_batch_state{item_queue=Queue} = flush(State),
    prometheus_gauge:set(chef_index_batch_current_batch_size_bytes, Size),
    prometheus_gauge:set(chef_index_batch_current_batch_doc_count, 1),
    {noreply, State1#chef_idx_batch_state{
                item_queue = [{From, AddedTime, Doc}| Queue],
                current_size = Size
               }};
handle_call({add_item, Doc, Size, AddedTime}, From, State = #chef_idx_batch_state{item_queue=Queue, current_size=CurrentSize}) ->
    CurrentSizeUpdated = CurrentSize + Size,
    prometheus_gauge:set(chef_index_batch_current_batch_size_bytes, CurrentSizeUpdated),
    prometheus_gauge:inc(chef_index_batch_current_batch_doc_count),
    {noreply, State#chef_idx_batch_state{item_queue = [{From, AddedTime, Doc}|Queue],
                                         current_size = CurrentSizeUpdated
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

collect_process_info() ->
    ProcInfo = process_info(self(), [message_queue_len, stack_size, total_heap_size, memory]),
    case ProcInfo of
        [{message_queue_len, MailboxSize},
         {stack_size, StackSizeWords},
         {total_heap_size, HeapSizeWords},
         {memory, MemorySizeBytes}] ->
            WordSize = erlang:system_info(wordsize),
            prometheus_gauge:set(chef_index_batch_stack_size_bytes, StackSizeWords*WordSize),
            prometheus_gauge:set(chef_index_batch_heap_size_bytes, HeapSizeWords*WordSize),
            prometheus_gauge:set(chef_index_batch_memory_size_bytes, MemorySizeBytes),
            prometheus_gauge:set(chef_index_batch_mailbox_size, MailboxSize);
        Other ->
            lager:warning("unexpected process_info reponse: ~w", [Other])
    end.


handle_cast({stats_update, TotalDocs, {AvgQueueLatency,AvgSuccessLatency}, Resp},
            State = #chef_idx_batch_state{avg_queue_latency = OQL,
                                          avg_success_latency = OSL,
                                          total_docs_queued = TQ,
                                          total_docs_success = TS
                                         }) ->
    chef_index_limiter:return_tickets(TotalDocs),
    collect_process_info(),
    TotalDocsQueuedUpdated = TQ + TotalDocs,
    AvgQueueLatencyUpdated = ((AvgQueueLatency*TotalDocs)+(OQL*TQ))/(TQ+TotalDocs),
    State1 = State#chef_idx_batch_state{total_docs_queued = TotalDocsQueuedUpdated,
                                        avg_queue_latency = AvgQueueLatencyUpdated},
    case Resp of
        ok ->
            prometheus_counter:inc(chef_index_batch_successful_docs_total, TotalDocs),
            TotalDocsSuccessUpdated = TS+TotalDocs,
            AvgSuccessLatencyUpdated = ((AvgSuccessLatency*TotalDocs)+(OSL*TS))/(TS+TotalDocs),
            {noreply, State1#chef_idx_batch_state{
                        total_docs_success = TotalDocsSuccessUpdated,
                        avg_success_latency = AvgSuccessLatencyUpdated
                       }
            };
        _ ->
            prometheus_counter:inc(chef_index_batch_failed_docs_total, TotalDocs),
            {noreply, State1}
    end;
handle_cast(flush, State) ->
    State1 = flush(State),
    prometheus_gauge:set(chef_index_batch_current_batch_size_bytes, 0),
    prometheus_gauge:set(chef_index_batch_current_batch_doc_count, 0),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

time_diff_in_ms(EndTime, StartTime) ->
    TimeTaken =  EndTime - StartTime,
    TimeTakenInMicro = erlang:convert_time_unit(TimeTaken, native, microsecond),
    TimeTakenInMicro/1000.0.
