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


-record(control_state, {
          control_data,
          control_group_id
         }).

-record(group_state, {
          group_data,
          group_authz_id,
          oc_chef_group :: #oc_chef_group{}
         }).

-record(acl_state, {
          type,
          authz_id,
          acl_data
         }).

-record(association_state, {
          data,
          user,
          org_user_association,
          org_user_invite,
          user_name
         }).

-record(organization_state, {
          organization_data,
          organization_authz_id,
          oc_chef_organization :: #oc_chef_organization{}
         }).

%% Since CS12, we have a default-org mode. Under this mode,
%% certain endpoints can be routed from
%% /ENDPOINT -> /organizations/DEFAULT_ORG/ENDPOINT
%% This is intended for backwards compatibility with
%% Open Source Chef 11 and earlier, and is not intended to
%% be carried forward. These are used to match on the strings
%% defined in dispatch.conf
%% NOTE: /users is specifically excluded from this list
-define(OSC11_COMPAT_RESOURCES,
    [
        "cookbooks",
        "clients",
        "data",
        "environments",
        "nodes",
        "principals",
        "roles",
        "sandboxes",
        "search",
        "runs",
        "groups",    %% Not technically OSC, but it was decided to route these too
        "containers",
        "organizations"
    ]
).
