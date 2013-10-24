%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-include_lib("oc_chef_authz/include/oc_chef_types.hrl").

-record(container_state, {
          container_data,
          container_authz_id,
          oc_chef_container :: #oc_chef_container{}
         }).

-record(group_state, {
          group_data,
          group_authz_id,
          oc_chef_group :: #oc_chef_group{}
         }).

