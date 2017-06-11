%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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


%% @doc Provides functions for retrieving application configuration
%% values and validating that they are of the appropriate type.
-module(chef_config).

-export([
         config_option/3
        ]).

-type option_type() :: pos_integer.
-type value_type() :: pos_integer().

-spec config_option(Application :: atom(),
                    OptionName :: atom(),
                    Type :: option_type()) ->
    Value :: value_type().
config_option(Application, OptionName, Type) ->
    envy:get(Application, OptionName, Type).
