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
-export([force_cleanup/0, spawn_timers/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {
          upload_timeout_interval :: non_neg_integer(), %% Upload timeouts  in ms
          cleanup_interval :: non_neg_integer() %% delay before cleanup of deleted/old files in ms
         }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

force_cleanup() ->
    gen_server:call(?MODULE, cleanup).

init(_Args) ->
    UploadInterval = envy:get(bookshelf, upload_time_interval, 5 * 1000, positive_integer),
    CleanupInterval = envy:get(bookshelf, cleanup_interval, 5 * 1000, positive_integer),
    %% Setup timers
    State =  #state{upload_timeout_interval = UploadInterval, cleanup_interval = CleanupInterval} ,
    % spawn_timers(State),
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

do_delete_cleanup(_State) ->
    case sqerl:select(purge_expired, [], first_as_scalar, [purge_expired]) of
        {ok, Count} ->
            %% TODO LOG RESULT
            lager:debug("Cleanup task: cleaned up %p expired file_data elements", [Count]),
            Count;
        _Error ->
            lager:debug("Cleanup task: error cleaning up expired file_data elements", []),
            0
    end.

do_upload_cleanup(_State) ->
    case sqerl:select(cleanup_abandoned_uploads, [], first_as_scalar, [cleanup_abandoned_uploads]) of
        {ok, Count} ->
            %% TODO LOG RESULT
            lager:debug("Cleanup task: cleaned up %p expired file_data elements", [Count]),
            Count;
        _Error ->
            lager:debug("Cleanup task: error cleaning up expired file_data elements", []),
            0
    end.


spawn_timers(#state{upload_timeout_interval=UploadInterval, cleanup_interval=CleanupInterval}) ->
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

