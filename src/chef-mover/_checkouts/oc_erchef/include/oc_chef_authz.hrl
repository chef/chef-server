%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @version 0.0.1
%% @end
%%
%% @doc authorization - Interface to the opscode authorization service
%%
%% This module is an Erlang port of the mixlib-authorization Ruby gem.
%%
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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

-type resource_type() :: 'actor'|'container'|'group'|'object'.
-type access_method() :: 'create'|'read'|'update'|'delete'|'grant'.
-type actor_list() :: [ binary() ].
-type group_list() :: [ binary() ].

-type oc_authz_id() :: <<_:256>>.

-record(authz_group, {actors = [] :: actor_list(),
                      groups = [] :: group_list()}).

-record(authz_ace,  {actors = [] :: actor_list(),
                     groups = [] :: group_list()}).

-type authz_ace() :: #authz_ace{}.
-type authz_acl() :: [{access_method(), #authz_ace{}},...].

-define(access_methods, [create, read, update, delete, grant]).

-record(chef_container, {
          'id',             % guid for object (unique)
          'authz_id',       % authorization guid (unique)
          'org_id',         % organization guid
          'name',           % name of container
          'path',           % 'path' of container (not used? Orig part of inheritance mech?; safe to delete? Yea!)
          'last_updated_by' % authz guid of last actor to update object
         }).

