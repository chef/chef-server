%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2016 Chef, Inc. All Rights Reserved.
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

-module(bksw_cleanup_task).
-behaviour(gen_server).

-export([start_link/0]).
-export([force_deleted_cleanup/0,
         force_upload_cleanup/0,
         spawn_timers/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(MIN_IN_MS, 60000).

-record(state, {
          upload_cleanup_interval :: non_neg_integer(), %% delay before cleanup of abandoned uploads in ms
          deleted_cleanup_interval :: non_neg_integer() %% delay before cleanup of deleted/old files in ms
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

force_deleted_cleanup() ->
    gen_server:call(?MODULE, delete_cleanup).

force_upload_cleanup() ->
    gen_server:call(?MODULE, upload_cleanup).

init(_Args) ->
    UploadInterval = envy:get(bookshelf, abandoned_upload_cleanup_interval, 19 * ?MIN_IN_MS, positive_integer),
    CleanupInterval = envy:get(bookshelf,deleted_data_cleanup_interval, 7 * ?MIN_IN_MS, positive_integer),
    State =  #state{upload_cleanup_interval = UploadInterval, deleted_cleanup_interval = CleanupInterval} ,
    spawn_timers(State),
    {ok, State}.

handle_call(delete_cleanup, _From, State) ->
    Count = do_delete_cleanup(State),
    {reply, {ok, Count}, State};
handle_call(upload_cleanup, _From, State) ->
    Count = do_upload_cleanup(State),
    {reply, {ok, Count}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_delete_cleanup(#state{deleted_cleanup_interval = Interval}) ->
    case sqerl:select(purge_expired, [Interval], first_as_scalar, [purge_expired]) of
        {ok, Count} ->
            lager:debug("Cleanup task: cleaned up ~p expired file_data elements", [Count]),
            Count;
        _Error ->
            lager:debug("Cleanup task: error cleaning up expired file_data elements", []),
            0
    end.

do_upload_cleanup(#state{upload_cleanup_interval = Interval}) ->
    case sqerl:select(cleanup_abandoned_uploads, [Interval], first_as_scalar, [cleanup_abandoned_uploads]) of
        {ok, Count} ->
            lager:debug("Cleanup task: cleaned up ~p expired file_data elements", [Count]),
            Count;
        _Error ->
            lager:debug("Cleanup task: error cleaning up expired file_data elements", []),
            0
    end.

spawn_timers(#state{upload_cleanup_interval=UploadInterval, deleted_cleanup_interval=CleanupInterval}) ->
    spawn_link(
      fun FlushUploads() ->
              timer:sleep(UploadInterval),
              gen_server:call(?MODULE, upload_cleanup),
              FlushUploads()
      end
     ),
    spawn_link(
      fun FlushDeleted() ->
              timer:sleep(CleanupInterval),
              gen_server:call(?MODULE, delete_cleanup),
              FlushDeleted()
      end
     ).
