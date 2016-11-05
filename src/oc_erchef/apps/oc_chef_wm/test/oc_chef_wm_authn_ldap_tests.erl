%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Steven Danna <steve@chef.io>
%% Copyright 2015 Chef Software, Inc
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

-module(oc_chef_wm_authn_ldap_tests).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_CONFIG, [{host,"192.168.33.152"},
                         {port,389},
                         {timeout,60000},
                         {bind_dn,"cn=admin,dc=chef-server,dc=dev"},
                         {bind_password,"H0\\/\\/!|\\/|3tY0ur|\\/|0th3r"},
                         {base_dn,"ou=chefs,dc=chef-server,dc=dev"},
                         {group_dn,[]},
                         {login_attribute,"uid"},
                         {display_name_attribute,"displayname"},
                         {first_name_attribute,"givenname"},
                         {last_name_attribute,"sn"},
                         {common_name_attribute,"cn"},
                         {country_attribute,"c"},
                         {city_attribute,"l"},
                         {email_attribute,"mail"},
                         {case_sensitive_login_attribute,false},
                         {encryption,none}]).

-define(CUSTOM_CONFIG, [{host,"192.168.33.152"},
                        {port,389},
                        {timeout,60000},
                        {bind_dn,"cn=admin,dc=chef-server,dc=dev"},
                        {bind_password,"H0\\/\\/!|\\/|3tY0ur|\\/|0th3r"},
                        {base_dn,"ou=chefs,dc=chef-server,dc=dev"},
                        {group_dn,[]},
                        {login_attribute,"uid"},
                        %% This is changed from the default of 'displayname'
                        {display_name_attribute,"nomdeguerre"},
                        %% This is changed from the default of 'givenname'
                        {first_name_attribute,"nomdeplume"},
                        %% This is changed from the default of 'sn'
                        {last_name_attribute,"surname"},
                        %% This is changed from the default of 'cn'
                        {common_name_attribute,"uncommonname"},
                        %% This is changed from the default of 'c'
                        {country_attribute,"notc"},
                        %% This is changed from the default of 'l'
                        {city_attribute,"homebase"},
                        %% This is changed from the default of 'mail'
                        {email_attribute,"email"},
                        {case_sensitive_login_attribute,false},
                        {encryption,none}]).


value_of_test_() ->
    Data = [{"key1", ["a_value"]}, {"key2", ["first", "second"]}],
    [{"returns a scalar (binary) value for the given key in a proplist where the values are arrays",
      fun()->
              ?assertEqual(<<"a_value">>, oc_chef_wm_authn_ldap:value_of("key1", Data, "default"))
      end
     },
     {"returns the first value when there are multiple items in the array",
      fun()->
              ?assertEqual(<<"first">>, oc_chef_wm_authn_ldap:value_of("key2", Data, "default"))
      end
     },
     {"returns the default if the key is missing",
      fun()->
              ?assertEqual(<<"default">>, oc_chef_wm_authn_ldap:value_of("key3", Data, "default"))
      end
     }
    ].

canonical_username_test_() ->
    [{"returns a lowercased bindary",
      fun()->
              ?assertEqual(<<"foobar">>, oc_chef_wm_authn_ldap:canonical_username("FOOBAR"))
      end},
     {"replaces special characters with _",
      fun()->
              ?assertEqual(<<"f_o_o_b_a_r">>, oc_chef_wm_authn_ldap:canonical_username("f^o&o)b@a$r"))
      end},
     {"does not replace 0-9",
      fun()->
              ?assertEqual(<<"0123456789">>, oc_chef_wm_authn_ldap:canonical_username("0123456789"))
      end},
     {"does not replace -",
      fun()->
              ?assertEqual(<<"foo-bar">>, oc_chef_wm_authn_ldap:canonical_username("foo-bar"))
      end}
    ].

result_to_user_ejson_test_() ->
    application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG),
    LoginAttr = "uid",
    UserName = <<"bob^bob">>,
    LdapUser = [{eldap_entry, "uid=bob^bob,ou=Person,dc=example,dc=com",
                 [{"c", ["USA"]},
                  {"l",["Seattle"]},
                  {"sn", ["Rabbit"]},
                  {"mail", ["bob@example.com"]},
                  {"givenName",["Bob"]},
                  {"displayName", ["Bobby"]},
                  {"cn", ["Bobby Bob"]},
                  {"o",["BigCorporation"]},
                  {"objectClass", ["person","organizationalPerson","inetOrgPerson"]},
                  {"uid",["bob^bob"]}]}],
    StrangeLdapUser = [{eldap_entry, "uid=bob^bob,ou=Person,dc=example,dc=com",
                          [{"notc", ["USA"]},
                           {"homebase",["Seattle"]},
                           {"surname", ["Rabbit"]},
                           {"email", ["bob@example.com"]},
                           {"nomdeplume",["Bob"]},
                           {"nomdeguerre", ["Bobby"]},
                           {"uncommonname", ["Bobby Bob"]},
                           {"o",["BigCorporation"]},
                           {"objectClass", ["person","organizationalPerson","inetOrgPerson"]},
                           {"uid",["bob^bob"]}]}],
    LdapUserExtraUid = [{eldap_entry, "uid=bob^bob,ou=Person,dc=example,dc=com",
                         [{"c", ["USA"]},
                          {"l",["Seattle"]},
                          {"sn", ["Rabbit"]},
                          {"mail", ["bob@example.com"]},
                          {"givenName",["Bob"]},
                          {"displayName", ["Bobby"]},
                          {"cn", ["Bobby Bob"]},
                          {"o",["BigCorporation"]},
                          {"objectClass", ["person","organizationalPerson","inetOrgPerson"]},
                          {"uid",["bob^bob", "bobby"]}]}],
    LdapUserExtraEmail = [{eldap_entry, "uid=bob^bob,ou=Person,dc=example,dc=com",
                           [{"c", ["USA"]},
                            {"l",["Seattle"]},
                            {"sn", ["Rabbit"]},
                            {"mail", ["bob@example.com"]},
                            {"givenName",["Bob"]},
                            {"displayName", ["Bobby"]},
                            {"cn", ["Bobby Bob"]},
                            {"o",["BigCorporation"]},
                            {"objectClass", ["person","organizationalPerson","inetOrgPerson"]},
                            {"uid",["bob^bob", "bobby"]}]}],
    [{"sets display_name in returned user from displayName in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"Bobby">>, proplists:get_value(<<"display_name">>, RetUser))
      end},
     {"sets common_name in returned user from cn in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"Bobby Bob">>, proplists:get_value(<<"common_name">>, RetUser))
      end},
     {"sets first_name in returned user from givenName in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"Bob">>, proplists:get_value(<<"first_name">>, RetUser))
      end},
     {"sets last_name in returned user from sn in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"Rabbit">>, proplists:get_value(<<"last_name">>, RetUser))
      end},
     {"sets country in returned user from c in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"USA">>, proplists:get_value(<<"country">>, RetUser))
      end},
     {"sets city in returned user from l in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"Seattle">>, proplists:get_value(<<"city">>, RetUser))
      end},
     {"sets email in returned user from mail in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"bob@example.com">>, proplists:get_value(<<"email">>, RetUser))
      end},
     {"sets username in returned user from normalized form of LoginAttr in the LDAP record",
      fun() ->
              {_, RetUserName, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"bob_bob">>, proplists:get_value(<<"username">>, RetUser)),
              ?assertEqual(<<"bob_bob">>, RetUserName)
      end},
     {"sets external_authentication_uid in returned user from them LoginAttr in the LDAP record",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(<<"bob^bob">>, proplists:get_value(<<"external_authentication_uid">>, RetUser))
      end},
     {"sets recovery_authentication_enabled in returned user to false",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUser),
              ?assertEqual(false, proplists:get_value(<<"recovery_authentication_enabled">>, RetUser))
      end},
     {"uses the first value of LoginAttr if multiple are given",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUserExtraEmail),
              ?assertEqual(<<"bob@example.com">>, proplists:get_value(<<"email">>, RetUser))
      end},
     {"uses the first value of mail if multiple are given",
      fun() ->
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,LdapUserExtraUid),
              ?assertEqual(<<"bob@example.com">>, proplists:get_value(<<"email">>, RetUser))
      end},
     {"uses a non-default display_name field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"Bobby">>, proplists:get_value(<<"display_name">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default first_name field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"Bob">>, proplists:get_value(<<"first_name">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default last_name field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"Rabbit">>, proplists:get_value(<<"last_name">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default common_name field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"Bobby Bob">>, proplists:get_value(<<"common_name">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default country field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"USA">>, proplists:get_value(<<"country">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default city field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"Seattle">>, proplists:get_value(<<"city">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end},
     {"uses a non-default mail field when configurd to",
      fun() ->
              application:set_env(oc_chef_wm, ldap, ?CUSTOM_CONFIG),
              {_, _, {RetUser}} = oc_chef_wm_authn_ldap:result_to_user_ejson(LoginAttr,UserName,StrangeLdapUser),
              ?assertEqual(<<"bob@example.com">>, proplists:get_value(<<"email">>, RetUser)),
              application:set_env(oc_chef_wm, ldap, ?DEFAULT_CONFIG)
      end}
    ].
