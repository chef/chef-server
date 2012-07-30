%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Kevin Smith <kevin@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_index_sup).

-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%% @doc Start the chef_index_sup indicating the application name under which the rabbitmq
%% config is located.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("starting chef_index_sup~n", []),
    Children = amqp_child_spec(),
    {ok, {{one_for_one, 60, 10}, Children}}.

amqp_child_spec() ->
    %% Lookup AMQP connection info
    case application:get_env(chef_index, rabbitmq_host) of
        undefined ->
            error_logger:info_msg("RabbitMQ config missing. Indexing for search is disabled.~n"),
            [];
        {ok, Host} ->
            Port = get_env(rabbitmq_port),
            User = get_env(rabbitmq_user),
            Password = get_env(rabbitmq_password),
            VHost = get_env(rabbitmq_vhost),
            ExchgName = get_env(rabbitmq_exchange),
            Exchange = {#'exchange.declare'{exchange=ExchgName, durable=true}},
            Network = {network, Host, Port, {User, Password}, VHost},
            error_logger:info_msg("Connecting to Rabbit at ~s:~p~s (exchange: ~p)~n",
                                  [Host, Port, VHost, ExchgName]),
            IndexDesc = {chef_index_queue, {bunnyc, start_link, [chef_index_queue, Network, Exchange, []]},
                         permanent, 5000, worker, dynamic},
            [IndexDesc]
    end.

get_env(Key) ->
    case application:get_env(chef_index, Key) of
        undefined ->
            throw({missing_application_config, {chef_index, Key}});
        {ok, Value} ->
            Value
    end.
