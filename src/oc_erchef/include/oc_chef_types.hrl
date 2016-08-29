%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Stephen Delano <stephen@chef.io>
%% Copyright 2014-2016 Chef Software, Inc. All Rights Reserved.


-type chef_authz_type() :: 'actor' |
                           'container' |
                           'group' |
                           'object'.

-record(oc_chef_container, {
          server_api_version,
          id,
          authz_id,
          org_id,
          name,
          last_updated_by,
          created_at,
          updated_at
         }).

-record(oc_chef_group, {
          server_api_version,
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
          server_api_version,
          id,
          name,
          org_id,
          authz_id,
          last_updated_by
          }).

-record(oc_chef_policy_group, {
          server_api_version,
          id,
          name,
          org_id,
          authz_id,
          last_updated_by
          %% NOTE: the schema has a serialized_object field but it's not
          %% currently used.
          }).

-record(oc_chef_policy_revision, {
          server_api_version,
          id,
          org_id,
          name,
          policy_authz_id,
          revision_id,
          serialized_object,
          last_updated_by
         }).

-record(oc_chef_policy_group_revision_association, {
          server_api_version,
          % Database fields
          id,
          org_id,

          policy_group_authz_id,
          policy_revision_revision_id,
          policy_revision_name,
          policy_group_name,
          last_updated_by,

          % "virtual", populated by JOIN, belongs to policy_revision
          policy_authz_id,
          serialized_object,

          % records that represent the objects we're joining in this table. We
          % need them so we can do on-demand creates
          policy,
          policy_revision,
          policy_group
         }).

-record(oc_chef_cookbook_artifact, {
          server_api_version,
          id,
          org_id,
          name,
          authz_id,
          version_identifiers :: [ Identifier::binary() ]
         }).

-record(oc_chef_cookbook_artifact_version, {
          server_api_version,
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
          server_api_version,
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
          server_api_version,
          org_id,
          user_id,
          user_name, % Not part of the table but retrieved via join
          last_updated_by,
          created_at,
          updated_at
         }).

-record(oc_chef_org_user_invite, {
          server_api_version,
          id,
          org_id,
          org_name,  % Not  part of table - retrieved via join
          user_id,
          user_name, % Not  part of table - retrieved via join
          last_updated_by,
          created_at,
          updated_at
         }).
