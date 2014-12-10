%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @copyright 2011-2012 Opscode Inc.

-module(oc_chef_wm_app).

-behaviour(application).

%% Application callbacks
-export([remsh_welcome/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    oc_chef_wm_sup:start_link().

stop(_State) ->
    ok.

%% @doc Print an informative message about how to use a remote shell attached to a live
%% oc_erchef node. The idea is to call this from a wrapper script used to start a remote
%% shell, like:
%% `erl -name user1@127.0.0.7 -setcookie erchef -remsh erchef@127.0.0.1 -s oc_chef_wm_app remsh_welcome'.
%%
remsh_welcome() ->
    Msg =
        "~n~n==> Welcome to the oc_erchef remote shell <==~n~n"
        "    TO EXIT: hit ctrl-g followed by q~n"
        "~n"
        "    DO NOT use q() or init:stop(), as these will stop the oc_erchef node~n~n",
    io:format(Msg),
    ok.
