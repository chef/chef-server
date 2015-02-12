%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean Rouge <jean@chef.io>
%% Copyright 2015 Chef Software, Inc. All Rights Reserved.

-record(context, {reqid :: binary(),
                  otto_connection,
                  darklaunch = undefined}).
