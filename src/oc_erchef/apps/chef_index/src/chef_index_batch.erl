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

-export([start_link/0,
         status/0,
         add_item/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(chef_idx_batch_state, {
          wrapper_size :: non_neg_integer(),
          current_size :: non_neg_integer(),
          max_size :: non_neg_integer(),
          item_queue = [], %% [{Pid, Timestamp, Doc}]
          max_wait :: non_neg_integer(),
          search_provider = solr :: solr|cloudsearch
         }).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_item(iolist()) -> term().
add_item(Doc) ->
    Size = byte_size(iolist_to_binary(Doc)),
    gen_server:call(?MODULE, {add_item, Doc, Size}).

status() ->
    gen_server:call(?MODULE, status).

wrap_solr(Docs) ->
    [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
     <<"<update>">>,
     <<"<add>">>,
     Docs,
     <<"</add>">>,
     <<"</update>">>].

wrap_cloudsearch(Docs) ->
    [<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
     <<"<batch>">>,
     Docs,
     <<"</batch>">>].

-spec wrap(iolist(), #chef_idx_batch_state{}) -> iolist().
wrap(Docs, #chef_idx_batch_state{search_provider = solr}) ->
    wrap_solr(Docs);
wrap(Docs, #chef_idx_batch_state{search_provider = cloudsearch}) ->
    lager:warning("Not implemented yet."),
    wrap_cloudsearch(Docs).

-spec wrapper_size(#chef_idx_batch_state{}) -> non_neg_integer().
wrapper_size(State) ->
    byte_size(iolist_to_binary(wrap([], State))).

init([]) ->
    MaxSize = envy:get(chef_index, search_batch_max_size, 5000000, integer),
    case MaxSize =< 0 of
	true ->
	    {stop, list_to_binary([<<"chef_index batch_max_size is set to ">>, integer_to_binary(MaxSize), <<". Please set to non-negative value, or set search_queue_mode to something besides batch.">>])};
	false ->
	    SearchProvider = envy:get(chef_index, search_provider, solr, chef_index_utils:one_of([solr, cloudsearch])),
	    MaxWait = envy:get(chef_index, search_batch_max_wait, 10, non_neg_integer),
	    WrapperSize = wrapper_size(#chef_idx_batch_state{search_provider=SearchProvider}),
	    CurrentSize = 0,
	    send_wakup(MaxWait),
	    {ok, #chef_idx_batch_state{wrapper_size = WrapperSize,
				       current_size = CurrentSize,
				       max_size = MaxSize,
				       max_wait = MaxWait,
				       search_provider = SearchProvider}}
    end.

send_wakup(Time) ->
    erlang:send_after(Time, self(), flush).

-spec flush(#chef_idx_batch_state{}) -> #chef_idx_batch_state{}.
flush(State = #chef_idx_batch_state{item_queue = []}) ->
    State;
flush(State = #chef_idx_batch_state{item_queue = Queue,
                                    current_size = CurrentSize,
                                    wrapper_size = WrapperSize}) ->
    {PidsToReply, _TimeStamps, DocsToAdd} = lists:unzip3(Queue),
    Doc = wrap(DocsToAdd, State),
    spawn_link(fun() ->
                       lager:info("Batch posting to solr ~p documents (~p bytes)", [length(DocsToAdd), CurrentSize+WrapperSize]),
                       Res = chef_index_expand:post_to_solr(Doc),
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
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, State = #chef_idx_batch_state{max_wait=MaxWait}) ->
    State1 = flush(State),
    send_wakup(MaxWait),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
