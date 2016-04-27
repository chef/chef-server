%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%% @author Kevin Smith <kevin@chef.io>
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
-export([
         server_for_vhost/1,
         start_link/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-define(SERVER, ?MODULE).

%% @doc Start the chef_index_sup indicating the application name under which the rabbitmq
%% config is located.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("starting chef_index_sup~n", []),
    Children = amqp_child_spec(),
    {ok, {{one_for_one, 60, 10}, Children}}.

%% Return a spec for a bunnyc gen_server for each vhost in {chef_index, rabbitmq_vhosts}. If
%% no such list of vhosts is found, check for key rabbitmq_vhost. Each vhost induces a
%% locally registered bunnyc server with name `chef_index_queue$VHOST'.
amqp_child_spec() ->
    %% Lookup AMQP connection info
    case envy:get(chef_index, disable_rabbitmq, false, boolean) of
        true ->
            error_logger:info_msg("RabbitMQ config disabled. Indexing for search is disabled.~n"),
            [];
        false ->
            %% This uses the key 'ip_mode' in chef_index to decide how to parse the address
            Host = envy_parse:host_to_ip(chef_index, rabbitmq_host),
            Port = envy:get(chef_index,rabbitmq_port, non_neg_integer),
            User = envy:get(chef_index,rabbitmq_user, binary),
            Password = envy:get(chef_index,rabbitmq_password, binary),
            ExchangeName = envy:get(chef_index,rabbitmq_exchange, binary),
            VHost = envy:get(chef_index, rabbitmq_vhost, binary),

            error_logger:info_msg("Connecting to Rabbit at ~p:~p ~p~n",
                                  [Host, Port, {VHost, ExchangeName}]),

            [bunnyc_spec(VHost, Host, Port, User, Password, ExchangeName)]
    end.

bunnyc_spec(VHost, Host, Port, User, Password, ExchangeName) ->
    Exchange = {#'exchange.declare'{exchange=ExchangeName, durable=true}},
    Network = {network, Host, Port, {User, Password}, VHost},
    Name = server_for_vhost(VHost),
    {Name, {bunnyc, start_link, [Name, Network, Exchange, []]},
     permanent, 5000, worker, dynamic}.

%% Given a rabbitmq `VHost' binary, return the atom that identifies the registered bunnyc
%% server for that vhost.
server_for_vhost(VHost) ->
    Bin = erlang:iolist_to_binary([<<"chef_index_queue">>, VHost]),
    erlang:binary_to_atom(Bin, utf8).
    
