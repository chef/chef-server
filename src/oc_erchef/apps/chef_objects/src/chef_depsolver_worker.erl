%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Stephen Delano <stephen@chef.io>
%%% @doc Worker module for chef_depsolver resource
%%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% Created : 31 Jul 2013 by Stephen Delano <stephen@chef.io>
%%%-------------------------------------------------------------------
-module(chef_depsolver_worker).

-behaviour(gen_server).

%% API
-export([solve_dependencies/4,
         start_link/0]).

%% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).


-define(SERVER, chef_depsolver).

-record(state, {port, os_pid}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl"). %% TODO: remove
-endif.
-include("chef_types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Solve dependencies with the given state and constraints
%% @end
%%--------------------------------------------------------------------
-spec solve_dependencies(AllVersions :: [chef_depsolver:dependency_set()],
                         EnvConstraints :: [chef_depsolver:constraint()],
                         Cookbooks :: [Name::binary() |
                                             {Name::binary(), Version::binary()}],
                         Timeout :: integer()) ->
                                {ok, [ versioned_cookbook()]} | {error, term()}.
solve_dependencies(AllVersions, EnvConstraints, Cookbooks, Timeout) ->
    case pooler:take_member(chef_depsolver, pooler_timeout()) of
        error_no_members ->
            {error, no_depsolver_workers};
        Pid ->
            case gen_server:call(Pid, {solve, AllVersions, EnvConstraints, Cookbooks, Timeout}, infinity) of
                {error, resolution_timeout} ->
                    %% At this point we've already force-killed the Ruby
                    %% sub-process. Returning fail to pooler will clean
                    %% up the worker process and provision a new one.
                    pooler:return_member(chef_depsolver, Pid, fail),
                    {error, resolution_timeout};
                Result ->
                    pooler:return_member(chef_depsolver, Pid, ok),
                    Result
            end
    end.

pooler_timeout() ->
    envy:get(chef_objects, depsolver_pooler_timeout, 0, non_neg_integer).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RubyExecutable = filename:join([code:priv_dir(chef_objects), "depselector_rb", "depselector.rb"]),
    %% - redirect stderr to /dev/null -
    %% The C-level implementation of the ruby depsolver prints out statistics to stderr.
    %% These show up in the erchef console log, and the data we care about here is
    %% already captured in stats_hero and logs.
    Port = open_port({spawn, "ruby " ++ RubyExecutable ++ " 2> /dev/null"},
                     [{packet, 4}, nouse_stdio, exit_status, binary]),


    %% In order to effectively kill the Ruby process if it hangs solving a really hard problem,
    %% we're going to need to get the OS-level PID of the ruby process. The :get_pid command
    %% on the Ruby side will return Process.pid back to the Erlang side. We capture this
    %% info on startup so that we can use it in the event of a timeout. Hard-killing the process
    %% handles the failure case where the Ruby process gets hung and can no longer respond to
    %% STDOUT closing, which would typically cause the process to exit.
    Payload = term_to_binary({get_pid}),
    erlang:port_command(Port, Payload),
    Pid = receive
              {Port, {data, Data}} ->
                  binary_to_term(Data)
          end,

    {ok, #state{port=Port, os_pid=Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({solve, AllVersions, EnvConstraints, Cookbooks, Timeout},
            _From,
            #state{port=Port, os_pid=Pid} = State) ->
    Payload = term_to_binary({solve, [{environment_constraints, EnvConstraints},
                                      {all_versions, AllVersions},
                                      {run_list, Cookbooks},
                                      {timeout_ms, Timeout}]}),
    erlang:port_command(Port, Payload),

    %% The underlying ruby code has the potential to reach nearly 2x the
    %% timeout value passed in. The timeout value is applied first on the
    %% initial solve. If a solution fails within the timeout, the timeout
    %% is then applied again to the culprit search. To account for this
    %% worst case scenario, we will double the receive timeout and add a
    %% small buffer of 50ms to account for inter-process communication
    %% time.
    %%
    %% If we do reach the Erlang-level receive timeout, then we want to
    %% force-kill the ruby process. We handle the erlang timeout with
    %% a forve-kill because there is a chance that the ruby process is
    %% hung and will not be able to respond to stdout closing and clean
    %% itself up. If, instead, the ruby process returns the timeout message,
    %% we know that it remains in a state to respond to further requests
    %% and therefore will be able to clean up after itself.
    ReceiveTimeout = (Timeout * 2) + 50,

    Reply = receive
                {Port, {data, Data}} ->
                    binary_to_term(Data)
            after
                ReceiveTimeout ->
                    os:cmd("kill -9 " ++ integer_to_list(Pid)),
                    {error, resolution_timeout}
            end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
