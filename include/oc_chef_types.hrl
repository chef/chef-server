%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.

-record(oc_chef_container, {
          id,
          authz_id,
          org_id,
          name,
          last_updated_by,
          created_at,
          updated_at
         }).

-record(oc_chef_group, {
          id,
          authz_id,
          org_id,
          name,
          last_updated_by,
          created_at,
          updated_at,
          requestor_authz_id,
          clients = [],
          users = [],
          groups = [],
          auth_side_actors = [],
          auth_side_groups = []
          }).
