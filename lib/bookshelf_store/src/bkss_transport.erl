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
-module(bkss_transport).

-export([behaviour_info/1]).

-export([new/2,
         recv/2,
         send/2]).

-export_type([trans/0]).

-record(trans_t, {callback, data}).

%%%===================================================================
%%% Types
%%%===================================================================
-type trans() :: record(trans_t).

%%===================================================================
%% External API
%%===================================================================
-spec behaviour_info(term()) -> term().
behaviour_info(callbacks) ->
    [{new,1},
     {recv,2},
     {send,2}];
behaviour_info(_) ->
    undefined.

-spec new(atom(), term()) -> trans().
new(ModuleName, Data) when is_atom(ModuleName) ->
    #trans_t{callback=ModuleName, data=ModuleName:new(Data)}.

-spec recv(trans(), non_neg_integer()) -> {ok, binary()} | term().
recv(#trans_t{callback=Mod,data=Data}, Length) ->
    Mod:recv(Data, Length).

-spec send(trans(), binary()) -> ok | term().
send(#trans_t{callback=Mod,data=Data}, Binary) ->
    Mod:send(Data, Binary).
