%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Eric Merritt <ericbmerritt@gmail.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.
-module(bksw_socket_transport).

-behaviour(bkss_transport).

-export([new/1,
         recv/2,
         send/2]).

-export_type([state/0]).

-record(state, {transport, socket, timeout}).

%%%===================================================================
%%% Types
%%%===================================================================
-opaque state() :: record(state).

%%===================================================================
%% External API
%%===================================================================
-spec new(list()) -> state().
new([Transport, Socket, Timeout]) ->
    #state{transport=Transport, socket=Socket, timeout=Timeout}.

-spec recv(state(), non_neg_integer()) -> {ok, binary()} | term().
recv(#state{transport=Transport, socket=Socket, timeout=Timeout}, Length) ->
    Transport:recv(Socket, Length, Timeout).

-spec send(state(), binary()) -> ok | term().
send(#state{transport=Transport, socket=Socket, timeout=_Timeout}, Binary) ->
    Transport:send(Socket, Binary).
