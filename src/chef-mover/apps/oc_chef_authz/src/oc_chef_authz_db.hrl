%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
%%
%%
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-record(oc_chef_authz_context,
        {reqid :: binary(),
         otto_connection :: couchbeam:server(),
         darklaunch :: term()}).

-type oc_chef_authz_context() :: #oc_chef_authz_context{}.

-type requestor_id() :: binary().
-type actor_id() :: binary().
-type object_id() :: <<_:256>>.
-type db_key() :: binary() | string().

-type authz_type() :: 'authz_client' |
                      'authz_container' |
                      'authz_cookbook' |
                      'authz_data_bag' |
                      'authz_environment' |
                      'authz_group' |
                      'authz_node' |
                      'authz_role'.

-type container_name() :: binary().
%% no binary literals in type specs?
%%                           <<"clients">> |
%%                           <<"containers">> |
%%                           <<"cookbooks">> |
%%                           <<"data">> |
%%                           <<"environments">> |
%%                           <<"groups">> |
%%                           <<"nodes">> |
%%                           <<"roles">> |
%%                           <<"sandboxes">> |
%%                           <<"search">>.
