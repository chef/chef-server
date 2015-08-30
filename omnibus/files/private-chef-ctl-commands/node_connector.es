#!/usr/bin/env escript
%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% %% ex: ts=4 sw=4 et ft=erlang
%%! -hidden
%%
%% Copyright (c) 2015 Chef Software, Inc
%%
%% @author Marc Pardise <marc@chef.io>
% using compiled mode gives more informative error traces
-mode(compile).

%   TODO data will be encrypted using a key generated on the originating
%   server, and captured by the provisioning tools.  The key should be
%   an argument to the script.
main([MyHost, RemoteHost]) ->
    HomeTeam = binary_to_atom(iolist_to_binary(["guest@", MyHost] ), utf8),
    AwayTeam = binary_to_atom(iolist_to_binary(["erchef@", RemoteHost] ), utf8),
    net_kernel:start([HomeTeam, longnames]),
    erlang:set_cookie(node(), 'erchef'),
    register(guest_node, self()),
    % We'll want some nice pretty error handling here -
    % retries for a sufficient period perhaps, since we mightbe waiting for the
    % node to come online.
    true = net_kernel:connect_node(AwayTeam),
    % { Action, NodeName, Role }
    case rpc:call(AwayTeam, oc_erchef_dist, register_node, [HomeTeam, frontend]) of
        {badrpc, Any} ->
            io:fwrite("rpc to remote node failed: ~p~n", [Any]),
            halt(16);
        ok ->
            capture_config(RemoteHost, 0),
            halt(0)
    end.

% establishconnection to the named remote node
% tell it that we exist and need to be registered.
% wait for our data on a backchannel - we'll listenwith gen_tcp
%   - chef-server.rb
%   - webui_priv.pem
%   - webui_pub.pem
%   - pivotal.pem
%   - pivotal.rb
%   - private-chef-secrets.json
% initially this will not really do a whole lot with disterl
capture_config(Host, Port) ->
    receive
        { proceed_registration, _, NewPort } ->
            % TODO message back with lenght/checcksum?
            capture_config(Host, NewPort);
        { config_data, FileName } ->
            fetch_file(FileName, Host, Port),
            capture_config(Host, Port);
        registration_complete ->
            ok
    end.

fetch_file(Path, RemoteHost, Port) ->
    {ok, Sock} = gen_tcp:connect(RemoteHost, Port, [binary, {packet, 4}, {active, false}]),
    % packet length indicator of 4 means we should be able to fit
    % whatever the host is sending in a single rqeuest.  Still, we'll
    % want to make this more robust when it's for real
    {ok, Data} = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    {ok, Fd} = file:open(Path, write),
    file:write(Fd, Data),
    file:close(Fd).
