%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Kevin Smith
%%% @author Seth Falcon <seth@chef.io>
%%% @doc
%%% REST resource for monitoring status of erchef
%%% @end
%% Copyright 2011-2014 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_status).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         init_resource_state/1]).

-include("oc_chef_wm.hrl").
-define(A2B(X), erlang:atom_to_binary(X, utf8)).

init(Config) ->
    oc_chef_wm_base:init(?MODULE, Config).

init_resource_state(_) ->
    % We're as simple as it gets, no state here.
    {ok, undefined}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    case check_health(State) of
        {fail, Body} ->
            {{halt, 500}, wrq:set_resp_body(Body, Req), State};
        {pong, Body} ->
            {Body, Req, State}
    end.

%% private functions

-spec check_health(ServerVersion :: binary()) -> {pong | fail, binary()}.
check_health(#base_state{otp_info = {_, ServerVersion}} = _State) ->
    ServerVersionBinary = list_to_binary(ServerVersion),
    Pings = spawn_health_checks(),
    Status = overall_status(Pings),

    log_failure(Status, Pings),
    KeyGen = chef_keygen_cache:status_for_json(),
    Indexing = chef_index:status(),

    StatList = [{<<"server_version">>, ServerVersionBinary},
                {<<"status">>, ?A2B(Status)},
                {<<"upstreams">>, {Pings}},
                {<<"keygen">>, {KeyGen}},
                {<<"indexing">>, {Indexing}}],
    {Status, chef_json:encode({StatList})}.


overall_status(Pings) ->
    case [ Pang || {_, <<"fail">>}=Pang <- Pings ] of
        [] ->
            pong;
        _Failure ->
            fail
    end.

-spec log_failure(fail | pong, [{binary(), <<_:32>>}]) -> ok.
log_failure(fail, Pings) ->
    FailureData = {{status, fail}, {upstreams, {Pings}}},
    lager:error("/_status~n~p~n", [FailureData]),
    ok;
log_failure(_,_) ->
    ok.

%% Execute health checks in parallel such that no check will exceed `ping_timeout()'
%% milliseconds. This call does not return until all health checks have been executed (or
%% timed out). Each checker process is monitored so crashed checking processes will be
%% reported as fails.
-spec spawn_health_checks() -> [{binary(), <<_:32>>}].
spawn_health_checks() ->
    Parent = self(),
    Workers = [{
        erlang:spawn_monitor(
            fun() -> check_health_worker(Mod, Parent, ping_timeout()) end
        ),
        Mod} || Mod <- ping_modules() ],
    %% Elements of Workers are {{Pid, MonRef}, Mod} tuples
    gather_health_workers(Workers, []).

%% Calls Mod:ping() and sends `{Pid, Mod, Result}' to the specified `Parent'. The worker
%% should be spawned into its own process where it will launch yet another process to
%% actually execute the ping so that it can do so with a timeout.
-spec check_health_worker(atom(), pid(), non_neg_integer()) -> {pid(), atom(), term()}.
check_health_worker(Mod, Parent, Timeout) ->
    Self = self(),
    proc_lib:spawn_link(fun() ->
                                Result = Mod:ping(),
                                Self ! {ok, Result}
                        end),
    receive
        {ok, Result} ->
            Parent ! {Self, Mod, Result}
    after
        Timeout ->
            Parent ! {Self, Mod, timeout}
    end.

%% Receive ping results from spawned workers. Also translates results into EJSON friendly
%% format and converts 'pang' to 'fail' for easier reading.
-spec gather_health_workers([{{pid(), reference()}, atom()}],
                            [{binary(), binary()}]) -> [{binary(), binary()}].
gather_health_workers([{{Pid, Ref}, Mod} | Rest] = List, Acc) ->
    %% Each worker is allotted `ping_timeout()' time to complete its check and report back
    %% to this process. We should always get a reply within this window since either the
    %% worker will send a result, trigger a timeout and send that, or crash in which case
    %% we'll recieve the 'DOWN' message. To protect against blocking in receive in the event
    %% of a bug in which none of the above happen, we set a timeout with some padding -- we
    %% need more than ping_timeout() time to avoid a race condition when a worker has a
    %% legitimate "normal" timeout. The 500 milliseconds here is very conservative.
    Timeout = ping_timeout() + 500,
    receive
        {Pid, Mod, Result} ->
            %% we translate pang => fail to make it easier to distinguish from pong by tired
            %% eyes at 3:00 AM.
            ResultBin = case Result of
                            pong -> <<"pong">>;
                            pang -> <<"fail">>;
                            timeout -> <<"fail">>
                        end,
            demonitor(Ref, [flush]),
            gather_health_workers(Rest, [ {?A2B(Mod), ResultBin} | Acc ]);
        {'DOWN', _MonRef, process, Pid, normal} ->
            %% ignore. should always get the message from the worker before the down
            %% message, but in case not, just ignore the normal exit.
            gather_health_workers(List, Acc);
        {'DOWN', _MonRef, process, Pid, _Info} ->
            gather_health_workers(Rest, [ {?A2B(Mod), <<"fail">>} | Acc ])
    after
        %% Should never get here since a health check worker should either timeout or
        %% crash. But to avoid the possibility of blocking with a bare receive, we set the
        %% timeout and return early.
        Timeout ->
            lager:error({Mod, ping, hard_fail}),
            [ {?A2B(Mod), <<"fail">>} | Acc ]
    end;
gather_health_workers([], Acc) ->
    Acc.

ping_timeout() ->
    envy:get(oc_chef_wm, health_ping_timeout, pos_integer).

ping_modules() ->
    envy:get(oc_chef_wm, health_ping_modules,list).
