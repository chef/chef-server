%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%%% @author Kevin Smith <kevin@opscode.com>
%%% @author Seth Falcon <seth@opscode.com>
%%% @copyright Copyright 2011-2012 Opscode Inc.
%%% @doc
%%% REST resource for monitoring status of erchef
%%% @end
-module(chef_wm_status).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(A2B(X), erlang:atom_to_binary(X, utf8)).

init(_Any) ->
    {ok, <<"{}">>}.

allowed_methods(Req, State) ->
    {['GET'], Req, State}.

content_types_provided(Req, State) ->
    {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->
    case check_health() of
        {<<"fail">>, Body} ->
            {{halt, 500}, wrq:set_resp_body(Body, Req), State};
        {<<"pong">>, Body} ->
            {Body, Req, State}
    end.

%% private functions

check_health() ->
    Pings = spawn_health_checks(),
    Status = overall_status(Pings),
    {Status, ejson:encode({[{<<"status">>, Status}, {<<"upstreams">>, {Pings}}]})}.

overall_status(Pings) ->
    case [ Pang || {_, <<"fail">>}=Pang <- Pings ] of
        [] ->
            %% no fails, we're good
            <<"pong">>;
        _Failure ->
            <<"fail">>
    end.

%% Execute health checks in parallel such that no check will exceed `ping_timeout()'
%% milliseconds. This call does not return until all health checks have been executed (or
%% timed out). Each checker process is monitored so crashed checking processes will be
%% reported as fails.
spawn_health_checks() ->
    Parent = self(),
    Pids = [ {erlang:spawn_monitor(fun() ->
                                          check_health_worker(Mod, Parent, ping_timeout())
                                  end), Mod} || Mod <- ping_modules() ],
    %% NOTE: Pids are in the form {Pid, MonRef}
    gather_health_workers(Pids, []).

%% Calls Mod:ping() and sends `{Pid, Mod, Result}' to the specified `Parent'. The worker
%% should be spawned into its own process where it will launch yet another process to
%% actually execute the ping so that it can do so with a timeout.
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
gather_health_workers([{{Pid, _}, Mod} | Rest] = List, Acc) ->
    Timeout = ping_timeout() + 500,
    receive
        {Pid, Mod, Result} ->
            %% we translate pang => fail to make it easier to distinguish from pong.
            ResultBin = case Result of
                            pong -> <<"pong">>;
                            pang -> <<"fail">>;
                            timeout -> <<"fail">>
                        end,
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
            error_logger:error_report({Mod, ping, hard_fail}),
            [ {?A2B(Mod), <<"fail">>} | Acc ]
    end;
gather_health_workers([], Acc) ->
    Acc.

ping_timeout() ->
    case application:get_env(chef_wm, health_ping_timeout) of
        {ok, Timeout} ->
            Timeout;
        _ ->
            error({missing_config, {chef_wm, health_ping_timeout}})
    end.

ping_modules() ->
    case application:get_env(chef_wm, health_ping_modules) of
        {ok, Modules} ->
            Modules;
        _ ->
            error({missing_config, {chef_wm, health_ping_modules}})
    end.
