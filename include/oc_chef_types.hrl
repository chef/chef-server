%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%% Copyright 2014 Chef, Inc. All Rights Reserved.

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
          for_requestor_id,
          authz_id,
          org_id,
          name,
          last_updated_by,
          created_at,
          updated_at,
          clients = [],
          users = [],
          groups = [],
          auth_side_actors = [],
          auth_side_groups = []
          }).

-record(oc_chef_policy, {
          id,
          name,
          org_id,
          authz_id,
          last_updated_by
          }).

-record(oc_chef_policy_group, {
          id,
          name,
          org_id,
          authz_id,
          last_updated_by
          %% NOTE: the schema has a serialized_object field but it's not
          %% currently used.
          }).

-record(oc_chef_policy_revision, {
          id,
          org_id,
          name,
          revision_id,
          serialized_object,
          last_updated_by
          }).

-record(oc_chef_policy_group_revision_association, {
          % Database fields
          id,
          org_id,
          policy_revision_revision_id,
          policy_revision_name,
          policy_group_name,
          last_updated_by,

          % "virtual", populated by JOIN, belongs to policy_revision
          serialized_object,

          % records that represent the objects we're joining in this table. We
          % need them so we can do on-demand creates
          policy,
          policy_revision,
          policy_group
         }).

-record(oc_chef_cookbook_artifact_version, {
          id,
          identifier,
          metadata,
          serialized_object,
          created_at,
          created_by,
          org_id,
          name,
          authz_id,
          checksums :: [ Checksum::binary() ]
         }).

-record(oc_chef_organization, {
          id,
          authz_id,
          name,
          full_name,
          assigned_at,
          last_updated_by,
          created_at,
          updated_at
         }).

-record(oc_chef_org_user_association, {
          org_id,
          user_id,
          user_name, % Not part of the table but retrieved via join
          last_updated_by,
          created_at,
          updated_at
         }).

-record(oc_chef_org_user_invite, {
          id,
          org_id,
          org_name,  % Not  part of table - retrieved via join
          user_id,
          user_name, % Not  part of table - retrieved via join
          last_updated_by,
          created_at,
          updated_at
         }).
