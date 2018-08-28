%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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

-module(chef_config_tests).

-include_lib("eunit/include/eunit.hrl").


config_option_test_() ->
    TestApplication = chef_config_test,
    TestOption = chef_config_test_option,
    {foreachx,
     fun(Value) ->
	     error_logger:tty(false),
	     case Value of
		 undefined ->
		     ok;
		 _ ->
		     application:set_env(TestApplication, TestOption, Value)
	     end
     end,
     fun(_,_) ->
	     error_logger:tty(true),
	     application:unset_env(TestApplication, TestOption)
     end,
     [{Value,
       fun(_,_) ->
	       {Description,
		fun() ->
			case {Value, ErrorState} of
			    {undefined, _} ->
				?assertError(config_missing_item,
					     chef_config:config_option(TestApplication, TestOption, Type));
			    {_, no_error} ->
				?assertEqual(Value,
					     chef_config:config_option(TestApplication, TestOption, Type));
			    {_, error} ->
				?assertError(config_bad_type,
					     chef_config:config_option(TestApplication, TestOption, Type))
			end
		end}
       end} || {Description, Value, Type, ErrorState} <- 
		   [{"Positive integer: " ++ Label, V, pos_integer, E } || 
		       {Label, V, E} <- [{"Succeeds with positive integer", 5, no_error},
					 {"Fails with 0", 0, error},
					 {"Fails with negative integer", -5, error},
					 {"Fails with positive float", 5.0, error},
					 {"Fails with negative float", -5.0, error},
					 {"Fails with a string", "This is bad", error},
					 {"Fails with a binary", <<"This is also bad">>, error},
					 {"Fails when undefined", undefined, always_error}]
		   ]]}.
	      
