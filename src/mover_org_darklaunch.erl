%% -*- erlang-indent-level: 4;indent-tabs-mode: nil;fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@opscode.com>
%% @copyright 2013 Opscode, Inc.
%%

%% Placeholder module for darklaunch redis functions. 
%%
-module(mover_org_darklaunch).

-export([disable_org/1,
         enable_org/1,
         org_to_sql/2]).

disable_org(_OrgName) -> 
    % TODO update darklaunch redis to disable org 
    ok.

enable_org(_OrgName) -> 
    % TODO update darklaunch redis to enable org.
    ok.

org_to_sql(_OrgName, _Components) ->
    % TODO update specified components as 'sql mode'
    ok.
