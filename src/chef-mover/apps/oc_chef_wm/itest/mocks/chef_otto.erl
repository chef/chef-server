%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2013-2018 Chef Software, Inc.

-module(chef_otto).

-export([
         connect/0,
         fetch_org_id/2
        ]).

connect() ->
    mocked_server.

fetch_org_id(_Server, _OrgName) ->
    <<"00000000000000000000000000000000">>.
