%% -*- mode:erlang, erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% Copyright 2012-2014 Chef Software, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-include("server_api_version.hrl").


%% Authentication Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(KEY_VERSION,  0).
-define(CERT_VERSION, 1).

%% Misc Defines
-define(EMPTY_EJSON_HASH, {[]}).

%% Global Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(GLOBAL_PLACEHOLDER_ORG_ID, <<"00000000000000000000000000000000">>).
-define(INFINITY_TIMESTAMP, {{294277,1,9},{4,0,54.775807}}).

-type infinity() :: {{294277,1,9},{4,0,float()}}.

%% Custom Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type db_type() :: mysql | pgsql.

-type id() :: binary().

%% object ids are always 32 characters hex. This spec matches the
%% length, might be able to constrain further for range of elements.
-type object_id() :: <<_:256>>.

%% This is just because there are so many places that take both
%% strings and binaries.
-type bin_or_string() :: binary() | string().

%% A standard Major, Minor, Patch version tuple
-type version() :: { non_neg_integer(), non_neg_integer(), non_neg_integer() }.

-type versioned_cookbook() :: { binary(), version() }.

-type chef_object_name() :: 'chef_data_bag' |
                            'chef_data_bag_item' |
                            'chef_environment' |
                            'chef_client' |
                            'chef_role' |
                            'chef_node'|
                            'chef_user' |
                            'chef_key' |
                            %% these belong to EC and should
                            %% eventually be included in a pluggable
                            %% fashion.
                            'policy' |
                            'policy_group' |
                            'organization' |
                            'oc_chef_container' |
                            'oc_chef_group'.

-type chef_type() :: 'data_bag' |
                     'data_bag_item' |
                     'environment' |
                     'client' |
                     'node' |
                     'role' |
                     'user'.

-type ejson_term() :: {maybe_improper_list()}.

%% @doc Acceptable values for the `num_versions' parameter that
%% several cookbook-related operations can accept.
-type num_versions() :: 'all' | non_neg_integer().

%% @doc Valid operators for specifying version constraints on cookbooks
-type comparison_operator() :: '>' | '<' | '=' | '<=' | '>=' | '~>'.

%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These records are used in either the authz or SQL layers

-record(chef_client, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id' :: object_id(),              % guid for object (unique)
          'authz_id' :: object_id(),        % authorization guid (unique)
          'org_id' :: object_id(),          % organization guid
          'name' :: binary(),               % name of client
          'validator' = false :: boolean(),         % boolean; true if this is a validator
           % BEGIN DEPRECATED APIv1
          'admin' = false :: boolean(),             % true if this is an admin user
          'public_key' :: binary(),         % public key
          'pubkey_version' :: ?KEY_VERSION | ?CERT_VERSION, % 0 = key, 1 = cert %
           % END DEPRECATED APIv1
          'last_updated_by' :: object_id(), % authz guid of last actor to update object
          'created_at' :: binary(), % time created at
          'updated_at' :: binary()  % time created at
         }).

-record(chef_cookbook_version, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',                % guid for object (unique)
          'major',             % major version
          'minor',             % minor version
          'patch',             % patch version
          'frozen',            % boolean; true if the cookbook version is frozen
          'meta_attributes',   % json blob of attributes
          'meta_deps',         % json string of dependencies
          'meta_long_desc',    % string
          'metadata',          % json blob of metadata
          'serialized_object', % json blob of object data
          'last_updated_by',   % authz guid of last actor to update object
          'created_at',        % time created at
          'updated_at',        % time updated at
          %% Add any new fields before here - for mysql prepared statements
          %%the fields need to be  in order and the following three fields
          %% are used in the WHERE clause of fetch_cookbook_version
          'authz_id',          % authorization guid (unique)
          'org_id',            % organization guid
          'name',              % cookbook name
          'checksums' :: [ Checksum::binary()] % file checksums from segments
         }).

-record(chef_data_bag, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',               % guid for object (unique)
          'authz_id',         % authorization guid (unique)
          'org_id',           % organization guid
          'name',             % data_bag name
          'last_updated_by',  % authz guid of last actor to update object
          'created_at',       % time created at
          'updated_at'        % time created at
         }).

-record(chef_data_bag_item, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',               % guid for object (unique)
          %% right now authz for items is done via the parent data_bag
          %% 'authz_id',         % authorization guid (unique)
          'org_id',           % organization guid
          'data_bag_name',    % parent data_bag name
          'item_name',        % data_bag_item name
          'last_updated_by',  % authz guid of last actor to update object
          'created_at',       % time created at
          'updated_at',       % time created at
          'serialized_object' % json blob of object data
         }).

-record(chef_environment, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',               % guid for object (unique)
          'authz_id',         % authorization guid (unique)
          'org_id',           % organization guid
          'name',             % environment name
          'last_updated_by',  % authz guid of last actor to update object
          'created_at',       % time created at
          'updated_at',       % time created at
          'serialized_object' % json blob of object data
         }).

-record(chef_node, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',               % guid for object (unique)
          'authz_id',         % authorization guid (unique)
          'org_id',           % organization guid
          'name',             % node name
          'environment',      % environment
          'policy_name',      % name of policyfile used by this node
          'policy_group',     % name of policy group this node is in
          'last_updated_by',  % authz guid of last actor to update object
          'created_at',       % time created at
          'updated_at',       % time created at
          'serialized_object' % json blob of object data
         }).

-record(chef_role, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id',               % guid for object (unique)
          'authz_id',         % authorization guid (unique)
          'org_id',           % organization guid
          'name',             % role name
          'last_updated_by',  % authz guid of last actor to update object
          'created_at',       % time created at
          'updated_at',       % time created at
          'serialized_object' % json blob of object data
         }).

-record(chef_sandbox, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id' :: binary(),         %% sandbox id, 32-char hex string
          'org_id' :: binary(),     %% organization guid,
          'created_at', %% time record was created; useful mainly for debugging / garbage collection
          'checksums' :: [{Checksum::binary(), Uploaded::boolean()}]
         }).

-record(chef_user, {
        'server_api_version' :: api_version(), % Internal use, api version level expected by server
        'id',                               %% guid for object (unique)
        'authz_id',                         %% authorization guid (placeholder - not used)
        'username',                         %% username
        'email',                            %% email - left null
        'public_key',                       %% public key - might be null
        'pubkey_version' :: ?KEY_VERSION | ?CERT_VERSION, %% public key version
        'hashed_password',                  %% password
        'salt',                             %% password salt
        'hash_type',                        %% hash used to scramble password
        'last_updated_by',                  %% authz guid of last actor to update object -
        'created_at',                       %% time created at
        'updated_at',                       %% time updated at
        'external_authentication_uid',      %% External UID, such as LDAP - nullable
        'recovery_authentication_enabled',  %%

        'serialized_object'                 %%
       }).


%% Not a true chef object, but corresponds to  the view keys_by_type.
-record(chef_requestor, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id' :: object_id(),
          'org_id' :: object_id() | global,
          'name',
          'authz_id',
          'type' ,
          'key_name',
          'public_key',
          'key_version' :: ?KEY_VERSION | ?CERT_VERSION
         }
       ).

%% Not a true chef object either, keys are owned by their associated user
%% or client object and are managed in that context.
-record(chef_key, {
          'server_api_version' :: api_version(), % Internal use, api version level expected by server
          'id'  :: object_id(),     %% guid of user or client, unique with key_name
          'key_name',               %% user named string describing key
          'public_key',             %% PKCS#1 public key or a certificate
          'key_version',            %% 0 for public_key, 1 for cert
          'expires_at' :: calendar:datetime() | infinity(), %% expiration time (utc)
          'last_updated_by' :: binary(), %% authz id of updater
          'created_at' :: binary(), %% when created
          'updated_at' :: binary(),  %% when last updated

          %% Important note - in 'chef_key:flatten' we explcitly drop this last field
          %% since it's not part of the DB table for insert. If you add more fields
          %% that are part of the DB table, add them above this comment.
          'old_name' :: binary()    %% In case of update, this will contain the original name
        }).

-type chef_key() :: #chef_key{}.

%% These types and records are just convenient shorthands for subsets of our
%% records that are used in the SQL layers.

-type chef_object() :: #chef_data_bag{} |
                       #chef_data_bag_item{} |
                       #chef_environment{} |
                       #chef_client{} |
                       #chef_role{} |
                       #chef_node{} |
                       #chef_user{} |
                       #chef_requestor{} |
                       #chef_key{}.

-type chef_indexable_object() :: #chef_environment{} |
                                 #chef_data_bag_item{} |
                                 #chef_client{} |
                                 #chef_role{} |
                                 #chef_node{}.

-type chef_updatable_object() :: tuple().
