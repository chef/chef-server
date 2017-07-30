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

-module(oc_chef_action_queue_config).

-export([
         get_rabbit_management_pool_name/0,
         get_rabbit_management_pool_setting/0,
         get_rabbit_queue_monitor_setting/2,
         set_rabbit_management_setting/2,
         set_rabbit_queue_monitor_setting/2,
         set_app_value/5
        ]).

-define(SERVER, ?MODULE).
-define(POOLNAME, rabbitmq_management_service).

get_rabbit_management_pool_setting() ->
  ServiceConfig = get_rabbit_management_setting(rabbitmq_management_service, []),
  {?POOLNAME, ServiceConfig}.

get_rabbit_management_pool_name() ->
  ?POOLNAME.

get_rabbit_management_setting(Key, Default) ->
    try
        RabbitConfig = envy:get(oc_chef_wm, rabbitmq, [], any),
        MgmtConfig = proplists:get_value(management, RabbitConfig, []),
        Config = proplists:get_value(Key, MgmtConfig, Default),
        add_auth_to_ibrowse_options(Config, MgmtConfig)
    catch Error:Reason ->
        lager:info("Can't get configuration setting ~p ~p: ~p ~p",
                   [Key, Default, Error, Reason]),
        Default
    end.

add_auth_to_ibrowse_options(Config, MgmtConfig) ->
    Username = proplists:get_value(user, MgmtConfig),
    {ok, Password} = chef_secrets:get(<<"rabbitmq">>, <<"management_password">>),
    IbrowseOptions = proplists:get_value(ibrowse_options, Config),
    Config1 = proplists:delete(ibrowse_options, Config),
    IbrowseOptions1 = [{basic_auth, {Username, erlang:binary_to_list(Password)}} | IbrowseOptions],
    [{ibrowse_options, IbrowseOptions1} | Config1].


get_rabbit_queue_monitor_setting(Key, Default) ->
    try
        RabbitConfig = envy:get(oc_chef_wm, rabbitmq, [], any),
        MonitoringConfig = proplists:get_value(monitoring, RabbitConfig, []),
        proplists:get_value(Key, MonitoringConfig, Default)
    catch Error:Reason ->
        lager:info("Can't get configuration setting ~p ~p: ~p ~p",
                   [Key, Default, Error, Reason]),
        Default
    end.

set_rabbit_management_setting(Key, NewVal) ->
    set_app_value(oc_chef_wm, rabbitmq, management, Key, NewVal).

set_rabbit_queue_monitor_setting(Key, NewVal) ->
    set_app_value(oc_chef_wm, rabbitmq, monitoring, Key, NewVal).

%% value MUST already exist in the dict
set_app_value(App, ConfigKey, SubSectionKey, Prop, NewValue) ->
    Cfg = envy:get(App, ConfigKey, [], any),
    NewCfg =
      lists:map(fun ({SectionKey,SectionValue})
                          when SectionKey == SubSectionKey ->
                            {SectionKey,
                            lists:map(fun ({K, _V}) when K == Prop ->
                                                {K, NewValue};
                                            (Val) -> Val
                                        end, SectionValue)
                            };
                    (Val) -> Val
           end, Cfg),
      application:set_env(App, ConfigKey, NewCfg).

