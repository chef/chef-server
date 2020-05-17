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
-compile([export_all, nowarn_export_all]).
-endif.

-define(SERVER, ?MODULE).

%% @doc Start the chef_index_sup indicating the application name under which the rabbitmq
%% config is located.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("Starting chef_index_sup.~n", []),
    error_logger:info_msg("Creating HTTP pool for Solr.~n"),
    Provider = envy:get(chef_index, search_provider, solr, envy:one_of([solr, elasticsearch])),
    case Provider of
        elasticsearch ->
            chef_elasticsearch:declare_metrics();
        _ -> ok
    end,
    chef_index_expand:declare_metrics(),
    chef_index_http:create_pool(),
    maybe_rabbitmq_monitoring(),
    Children = child_spec(),
    {ok, {{one_for_one, 60, 10}, Children}}.

maybe_rabbitmq_monitoring() ->
    case envy:get(chef_index, search_queue_mode, rabbitmq, envy:one_of([rabbitmq, batch, inline])) of
        rabbitmq ->
            Config = envy:get(chef_index, rabbitmq_index_management_service, [], any),
            {LocalConfig, HttpConfig} = proplists:split(Config, [enabled, user]),
            case LocalConfig of
                [[{enabled, true}], [{user, Username}]] ->
                    {ok, Password} = chef_secrets:get(<<"rabbitmq">>, <<"management_password">>),
                    chef_index_queue:create_management_pool(Username, Password, HttpConfig);
                _ ->
                    error_logger:info_msg("Rabbitmq monitoring is disabled. "
                                          "chef_index will not check rabbitmq health.~n"),
                    ok
            end;
        _ ->
            ok
    end.

%% Return a spec for a bunnyc gen_server or the chef_index_batch gen_server based on the
%% search_queue_mode configuration.
%%
%% When rabbitmq is enable, one bunnyc server is started for each vhost in {chef_index,
%% rabbitmq_vhosts}. If no such list of vhosts is found, check for key rabbitmq_vhost. Each
%% vhost induces a locally registered bunnyc server with name `chef_index_queue$VHOST'.
child_spec() ->
    %% Lookup AMQP connection info
    case envy:get(chef_index, search_queue_mode, rabbitmq, envy:one_of([rabbitmq, batch, inline])) of
        rabbitmq ->
            %% This uses the key 'ip_mode' in chef_index to decide how to parse the address
            Host = envy_parse:host_to_ip(chef_index, rabbitmq_host),
            Port = envy:get(chef_index,rabbitmq_port, non_neg_integer),
            User = envy:get(chef_index,rabbitmq_user, binary),
            {ok, Password} = chef_secrets:get(<<"rabbitmq">>, <<"password">>),
            ExchangeName = envy:get(chef_index,rabbitmq_exchange, binary),
            VHost = envy:get(chef_index, rabbitmq_vhost, binary),

            error_logger:info_msg("Connecting to Rabbit at ~p:~p ~p~n",
                                  [Host, Port, {VHost, ExchangeName}]),

            [bunnyc_spec(VHost, Host, Port, User, Password, ExchangeName)];
        _  -> %% TODO should we not start up batch if in inline mode?
            [{chef_index_batch, {chef_index_batch, start_link, []},
              permanent, 5000, worker, [chef_index_batch]}]
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
