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

%% @doc Fetch a configuration value via `application:get_env/2` and
%% verify it is of the given type.  Valid values are returned; invalid
%% values trigger an error @end
-spec config_option(Application :: atom(),
		    OptionName :: atom(),
		    Type :: option_type()) ->
			   Value :: value_type().
config_option(Application, OptionName, Type) ->
    case application:get_env(Application, OptionName) of
        {ok, Value} ->
            case validate(Value, Type) of
		true ->
		    Value;
		false ->
		    error_logger:error_msg("Improper Configuration: ~p / ~p was ~p; should be a ~p~n",
					   [Application, OptionName, Value, Type]),
		    erlang:error({configuration, Application, OptionName, Value, Type})
	    end;
        undefined ->
            error_logger:error_msg("Improper Configuration: ~p / ~p was undefined!~n",
                                   [Application, OptionName]),
            erlang:error({configuration, Application, OptionName, undefined})
    end.

%% @doc Return `true' if `Value' is of the specified `Type'; `false'
%% otherwise.
-spec validate(Value :: term(), Type :: option_type()) -> boolean().
validate(Value, pos_integer) when is_integer(Value) -> Value > 0;
validate(_Value, pos_integer) -> false.
