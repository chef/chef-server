%% Copyright 2012-2014 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_action_queue).

-export([publish/2]).

-define(SERVER, ?MODULE).

-spec publish(RoutingKey :: binary(), Data :: binary()) -> ok.
publish(RoutingKey, Data) ->
    %% Calls stack until return values are docco'd:
    %% bunnyc:publish
    %%    gen_server:call(name, {publish ..})
    %%      bunnyc:internal_publish(fun amqp_channel:call/3, ...)
    %%        amqp_channel:call(name, {...}) -> ok|blocked|closing
    %% blocked or closing count as errors to us, and letting errors bubble up
    %% seems fine.

    AMQPMsg = bunny_util:new_message(Data),
    PersistentData = bunny_util:set_delivery_mode(AMQPMsg, 2),
    ok = bunnyc:publish(?SERVER, RoutingKey, PersistentData).
