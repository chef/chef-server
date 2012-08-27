%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author James Casey <james@opscode.com>
%% @copyright 2012 Opscode, Inc.

%% Tests around authz logic and helpers for chef_wm endpoints
%%


-module(chef_wm_authz_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chef_wm.hrl").

make_client(Name, Admin, Validator) ->
  #chef_client{name = Name,
               admin = Admin,
               validator = Validator}.

-define(ADMIN, make_client(<<"admin">>, true, false)).
-define(VALIDATOR, make_client(<<"validator">>, false, true)).
-define(NONADMIN, make_client(<<"normal">>, false, false)).

allow_admin_test_() ->
  [
    {"allow_admin Admin is true",
    fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin(?ADMIN)) end},
    {"allow_admin Validator is false",
     fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin(?VALIDATOR)) end},
    {"allow_admin non-admin is false",
     fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin(?NONADMIN)) end},
    {"no match for allow_admin with non-client",
     fun() -> ?assertError(function_clause, chef_wm_authz:allow_admin(#chef_node{name= <<"foo">>})) end}
    ].

allow_validator_test_() ->
  [
    {"allow_validator Admin is false",
     fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_validator(?ADMIN)) end},
    {"allow_validator Validator is true",
     fun() -> ?assertEqual(authorized, chef_wm_authz:allow_validator(?VALIDATOR)) end},
    {"allow_validator non-admin is false",
     fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_validator(?NONADMIN)) end},
    {"no match for allow_validator with non-client",
     fun() -> ?assertError(function_clause, chef_wm_authz:allow_validator(#chef_node{name= <<"foo">>})) end}
    ].

allow_admin_or_validator_test_() ->
  [
    {"allow_admin_or_validator Admin is true",
     fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_validator(?ADMIN)) end},
    {"allow_admin_or_validator Validator is true",
     fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_validator(?VALIDATOR)) end},
    {"allow_admin_or_validator non-admin is false",
     fun() -> ?assertEqual(forbidden, chef_wm_authz:allow_admin_or_validator(?NONADMIN)) end},
    {"no match for allow_admin_or_validator with non-client",
     fun() -> ?assertError(function_clause, chef_wm_authz:allow_admin_or_validator(#chef_node{name= <<"foo">>})) end}
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
    {"allow_admin_or_requesting_node Validator is true with node name",
     fun() -> ?assertEqual(authorized, chef_wm_authz:allow_admin_or_requesting_node(make_client(NodeName, false, true),
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
     fun() -> ?assertError(function_clause, chef_wm_authz:allow_admin_or_requesting_node(#chef_node{name= <<"foo">>}, NodeName)) end}
    ].

