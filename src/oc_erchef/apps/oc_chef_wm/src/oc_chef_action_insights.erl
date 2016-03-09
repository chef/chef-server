%% Copyright 2012-2016 Chef Software, Inc. All Rights Reserved.
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

%% @doc Receiver of ingest requests. Requests are queued in a file and parsed
%% asynchronously.

-module(oc_chef_action_insights).

-behaviour(gen_server).

%% API
-export([
         ingest/2,
         start_link/0
        ]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {io_device}).

-spec ingest(ShouldSend :: boolean(), Data :: binary()) -> ok.
ingest(true, Data) ->
    gen_server:cast(?MODULE, {ingest, Data});
ingest(false, _) -> ok.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, IODevice} = file:open(envy:get(oc_chef_wm, insights_log, string), [append]),
    {ok, #state{io_device = IODevice}}.

handle_call(_, _, State) ->
    {reply, ok , State}.

handle_cast({ingest, Data}, #state{io_device = IODevice} = State) ->
    case file:write(IODevice, [Data, "\n"]) of
        ok ->
            no_op;
        {error, Reason} ->
            lager:warning("Unable to write to insights file: ~p", [Reason])
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{io_device = IODevice} = _State) ->
    file:close(IODevice).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
