%% @author Mark Mzyk <mm@chef.io>
%%
%% Copyright 2015 Chef Software, Inc.
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

-module(oc_chef_authz_acl_constraints_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(ORGID, <<"some_org_id">>).

%% Test generators that setup and run the tests
oc_chef_authz_acl_constraints_test_() ->
  [
    { "Reports that admins group is found", contains_admins_group_true() },
    { "Reports that admins group is not found", contains_admins_group_false() },
    { "Check admins group removal returns admins group was not removed", check_admins_group_removal_not_removed() },
    { "Check admins group removal returns admins group was removed", check_admins_group_removal_removed() },
    { "Check that the acl constraints reports no failures when there should be none", check_acl_constraints_no_failures() },
    { "Check that the acl constraints reports failures when they are present", check_acl_constraints_failures() },
    { "Check that removal of grant ace passes if not grant ace", check_acl_constraints_not_grant_ace() }
  ].


%% contains_admins_group tests
contains_admins_group_true() ->
  [
    ?_assertEqual(true, oc_chef_authz_acl_constraints:contains_admins_group([<<"admins">>])),
    ?_assertEqual(true, oc_chef_authz_acl_constraints:contains_admins_group([<<"admins">>, <<"another_group">>]))
  ].

contains_admins_group_false() ->
  [
    ?_assertEqual(false, oc_chef_authz_acl_constraints:contains_admins_group([])),
    ?_assertEqual(false, oc_chef_authz_acl_constraints:contains_admins_group([<<"another_group">>]))
  ].


%% check_admins_group_removal tests
check_admins_group_removal_not_removed() ->
  %% Not a true unit test, since it calls through to contains_admins_group,
  %% but meck can't currently mock intra module calls.
  [
    %% CurrentGroup doesn't contain the admins group
    ?_assertEqual(not_removed, oc_chef_authz_acl_constraints:check_admins_group_removal([], [])),
    %% CurrentGroup doesn't contain the admins group, NewGroup does
    ?_assertEqual(not_removed, oc_chef_authz_acl_constraints:check_admins_group_removal([], [<<"admins">>])),
    %% CurrentGroup contains admins group, NewGroup also contains admins group
    ?_assertEqual(not_removed, oc_chef_authz_acl_constraints:check_admins_group_removal([<<"admins">>], [<<"admins">>]))
  ].

check_admins_group_removal_removed() ->
  %% Not a true unit test, since it calls through to contains_admins_group,
  %% but meck can't currently mock intra module calls.
  [
    %% CurrentGroup contains the admins group, NewGroup does not
    ?_assertEqual(removed, oc_chef_authz_acl_constraints:check_admins_group_removal([<<"admins">>], []))
  ].

%% check_acl_constraints tests
check_acl_constraints_no_failures() ->
  %% All dummy values, but based on real values.
  %% Key to the test is AclChecks, which give the return value
  %% check_acl_constraints operates on
  AuthzId = <<"10000000000000000000000000000000">>,
  Type = group,
  AclPerm = <<"grant">>,
  Ace = {[{<<"grant">>,{[{<<"actors">>,[<<"pivotal">>]},{<<"groups">>,[]}]}}]},
  AclChecks = [ fun(_OrgId, _AuthzId, _Type, _AclPerm, _Ace) -> false end ],
  [
    ?_assertEqual(ok, oc_chef_authz_acl_constraints:check_acl_constraints(?ORGID, AuthzId, Type, AclPerm, Ace, AclChecks))
  ].

check_acl_constraints_failures() ->
  %% All dummy values, but based on real values.
  %% Key to the test is AclChecks, which give the return value
  %% check_acl_constraints operates on
  AuthzId = <<"10000000000000000000000000000000">>,
  Type = group,
  AclPerm = <<"grant">>,
  Ace = {[{<<"grant">>,{[{<<"actors">>,[<<"pivotal">>]},{<<"groups">>,[]}]}}]},
  AclChecks = [ fun(_OrgId, _AuthzId, _Type, _AclPerm, _Ace) -> {true, failure_message_here} end ],
  Test1 = ?_assertEqual([failure_message_here], oc_chef_authz_acl_constraints:check_acl_constraints(?ORGID, AuthzId, Type, AclPerm, Ace, AclChecks)),
  AclChecks2 = [
                fun(_OrgId, _AuthzId, _Type, _AclPerm, _Ace) -> false end,
                fun(_OrgId, _AuthzId, _Type, _AclPerm, _Ace) -> {true, failure_one} end,
                fun(_OrgId, _AuthzId, _Type, _AclPerm, _Ace) -> {true, failure_two} end
               ],
  Test2 = ?_assertEqual([failure_one, failure_two], oc_chef_authz_acl_constraints:check_acl_constraints(?ORGID, AuthzId, Type, AclPerm, Ace, AclChecks2)),
  [ Test1, Test2 ].

check_acl_constraints_not_grant_ace() ->
  %% Not a true unit test, as this is more of an integration
  %% test within this module, but this tests catches an observed
  %% failure that bubbled up in pedant.

  %% The opposite tests that checks if this works with the grant ace
  %% can't be run as an eunit test because it relies on calling out
  %% to another module

  %% All dummy values, but based on real values.
  AuthzId = <<"10000000000000000000000000000000">>,
  Type = organization,
  AclPerm = <<"create">>,
  Ace = {[{<<"create">>,{[{<<"actors">>,[<<"pivotal">>]},{<<"groups">>,[]}]}}]},
  [
    ?_assertEqual(ok, oc_chef_authz_acl_constraints:check_acl_constraints(?ORGID, AuthzId, Type, AclPerm, Ace, oc_chef_authz_acl_constraints:acl_checks()))
  ].


