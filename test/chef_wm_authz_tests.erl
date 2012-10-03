%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
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


%% Tests around authz logic and helpers for chef_wm endpoints
%%


-module(chef_wm_authz_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_wm.hrl").

make_client(Name, Admin, Validator) ->
  #chef_client{name = Name,
               admin = Admin,
               validator = Validator}.
make_user(Name, Admin) ->
  #chef_user{username = Name,
             admin = Admin}.

-define(ADMIN, make_client(<<"admin">>, true, false)).
-define(VALIDATOR, make_client(<<"validator">>, false, true)).
-define(NONADMIN, make_client(<<"normal">>, false, false)).

-define(ADMIN_USER, make_user(<<"admin_user">>, true)).
-define(NON_ADMIN_USER, make_user(<<"admin_user">>, false)).

allow_admin_test_() ->
    [
     {"allow_admin Admin is true",
      fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin(?ADMIN)) end},
     {"allow_admin Validator is false",
      fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin(?VALIDATOR)) end},
     {"A pathological validator that is also an admin is forbidden anyway",
      fun() -> ?assertEqual(forbidden,
                            chef_wm_authz:allow_admin(#chef_client{name = <<"weird_validator">>,
                                                                   validator = true,
                                                                   admin = true}))
      end},
     {"allow_admin non-admin is false",
      fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin(?NONADMIN)) end},
     {"no match for allow_admin with non-client",
      fun() -> ?assertError(function_clause, chef_wm_authz:allow_admin(#chef_node{name= <<"foo">>})) end}
    ].

is_admin_test_() ->
  [
    {"is_admin Admin is true",
    fun() -> ?assertEqual(true, chef_wm_authz:is_admin(?ADMIN)) end},
    {"is_admin Validator is false",
     fun() -> ?assertEqual(false, chef_wm_authz:is_admin(?VALIDATOR)) end},
    {"is_admin non-admin is false",
     fun() -> ?assertEqual(false, chef_wm_authz:is_admin(?NONADMIN)) end},
    {"no match for is_admin with non-client",
     fun() -> ?assertError(function_clause, chef_wm_authz:is_admin(#chef_node{name= <<"foo">>})) end}
    ].

is_validator_test_() ->
  [
    {"is_validator Admin is false",
     fun() -> ?assertEqual(false, chef_wm_authz:is_validator(?ADMIN)) end},
    {"is_validator Validator is true",
     fun() -> ?assertEqual(true, chef_wm_authz:is_validator(?VALIDATOR)) end},
    {"is_validator non-admin is false",
     fun() -> ?assertEqual(false, chef_wm_authz:is_validator(?NONADMIN)) end},
    {"no match for is_validator with non-client",
     fun() -> ?assertError(function_clause, chef_wm_authz:is_validator(#chef_node{name= <<"foo">>})) end}
    ].

allow_admin_or_requesting_node_test_() ->
  NodeName = <<"foo">>,
  NotNode  = <<"not_foo">>,
    [
     {"allow_admin_or_requesting_node Admin is true with node name",
      fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_requesting_node(make_client(NodeName, true, false),
                                                                                     NodeName)) end},
     {"allow_admin_or_requesting_node Admin is true with different node name",
      fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_requesting_node(make_client(NotNode, true, false),
                                                                                     NodeName)) end},
     {"allow_admin_or_requesting_node Validator is false with same node name",
      fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin_or_requesting_node(make_client(NodeName, false, true),
                                                                                    NodeName)) end},
     {"allow_admin_or_requesting_node Validator is false with different node name",
      fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin_or_requesting_node(make_client(NotNode, false, true),
                                                                                    NodeName)) end},
     {"allow_admin_or_requesting_node non-admin is true with node name",
      fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_requesting_node(make_client(NodeName, false, false),
                                                                                     NodeName)) end},
     {"allow_admin_or_requesting_node non-admin is false with different node name",
      fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin_or_requesting_node(make_client(NotNode, false, false),
                                                                                    NodeName)) end},
     {"no match for allow_admin_or_requesting_node with non-client",
      fun() -> ?assertError(function_clause, chef_wm_authz:allow_admin_or_requesting_node(#chef_node{name= <<"foo">>},
                                                                                          NodeName)) end}
    ].

all_but_validators_test_() ->
    [
     {Message,
      fun() -> ?assertEqual(Expected,
                            chef_wm_authz:all_but_validators(Requestor))
      end}
     || {Message, Requestor, Expected} <- [
                                           {"Validator is not allowed", ?VALIDATOR, forbidden},
                                           {"Admin client is allowed", ?ADMIN, authorized},
                                           {"Non-admin client is allowed", ?NONADMIN, authorized},
                                           {"Admin user is allowed", ?ADMIN_USER, authorized},
                                           {"Non-admin user is allowed", ?NON_ADMIN_USER, authorized}
                                          ]
    ].

setup_no_custom_acls_env() ->
    %%application:set_env(oc_chef_wm, custom_acls_always_for_modification, false),
    application:set_env(oc_chef_wm, custom_acls_depsolver, false),
    application:set_env(oc_chef_wm, custom_acls_roles, false),
    application:set_env(oc_chef_wm, custom_acls_cookbooks, true),
    application:set_env(oc_chef_wm, custom_acls_data, true),
    ok.

set_default_custom_acls_env() ->
    application:set_env(oc_chef_wm, custom_acls_always_for_modification, true),
    application:set_env(oc_chef_wm, custom_acls_depsolver, true),
    application:set_env(oc_chef_wm, custom_acls_roles, true),
    application:set_env(oc_chef_wm, custom_acls_cookbooks, true),
    application:set_env(oc_chef_wm, custom_acls_data, true),
    ok.

customize_for_modification_maybe_with_no_custom_acls_test_() ->
    {setup,
     fun() -> 
             setup_no_custom_acls_env()
     end,
     fun(_) -> 
             set_default_custom_acls_env()
     end,
     [
         {"GET case always returns authorized",
          fun() -> Got = chef_wm_authz:customize_for_modification_maybe('GET', a, r, s),
                   ?assertEqual({authorized, r, s}, Got)
         end
         },
         {"PUT case returns authorized when always_for_modification == false",
          fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, false),
                   Got = chef_wm_authz:customize_for_modification_maybe('PUT', a, r, s),
                   ?assertEqual({authorized, r, s}, Got)
          end
         },
         {"PUT case returns auth rules when always_for_modification == true",
          fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, true), Got = chef_wm_authz:customize_for_modification_maybe('PUT', a, r, s),
                   ?assertEqual({a, r, s}, Got)
          end
         }
     ]}.

use_custom_acls_test_() ->
    {setup,
     fun() -> 
             set_default_custom_acls_env()
     end,
     fun(_) -> ok end,
     [
        {"Clients always return original auth with default env",
         fun() -> Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({a, r, State}, Got)
         end}
     ]}.

use_custom_acls_with_no_custom_acls_test_() ->
    {setup,
     fun() -> meck:new(wrq) end,
     fun(_) -> meck:unload(wrq) end,
     [
        {"Clients depsolver returns original auth with no_custom_acls and PUT",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, true),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'PUT' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({a, r, State}, Got)
         end
        },
        {"Clients depsolver returns authorized with no always_for_modification and PUT",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, false),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'PUT' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({authorized, r, State}, Got)
         end
        },
        {"Clients depsolver returns authorized with no_custom_acls env and GET",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, true),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'GET' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({authorized, r, State}, Got)
         end
        },
        {"Clients depsolver returns authorized with no_custom_acls env and GET",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, false),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'GET' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({authorized, r, State}, Got)
         end
        },
        {"Clients depsolver returns authorized with no_custom_acls env and POST",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, true),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'POST' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({authorized, r, State}, Got)
         end
        },
        {"Clients depsolver returns authorized with no_custom_acls env and POST",
         fun() -> application:set_env(oc_chef_wm, custom_acls_always_for_modification, false),
                  application:set_env(oc_chef_wm, custom_acls_depsolver, false),
                  meck:expect(wrq, method, fun(_Req) -> 'POST' end),
                  Client = #chef_client{name = <<"alice">>,
                                        admin = true,
                                        validator = false,
                                        public_key = <<"key is here">>},
                  State = #base_state{requestor = Client},
                  Got = chef_wm_authz:use_custom_acls(depsolver, a, r, State),
                  ?assertEqual({authorized, r, State}, Got)
         end
        }
     ]}.

