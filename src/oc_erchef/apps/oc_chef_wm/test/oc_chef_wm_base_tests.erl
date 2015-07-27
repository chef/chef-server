%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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

-module(oc_chef_wm_base_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").

stats_hero_label_test_() ->

    application:set_env(chef_objects, s3_url, "http://s3.amazonaws.com"),
    application:set_env(chef_objects, s3_platform_bucket_name, "test.bucket"),

    GoodTests = [
                 ?_assertEqual(Expect, oc_chef_wm_base:stats_hero_label(In))
                 || {In, Expect} <- [
                                     {{chef_sql, fetch_client}, <<"rdbms.chef_sql.fetch_client">>},
                                     {{chef_solr, some_fun}, <<"solr.chef_solr.some_fun">>},
                                     {{oc_chef_authz, some_other_fun}, <<"authz.oc_chef_authz.some_other_fun">>},
                                     {{chef_s3, delete_checksums}, <<"s3.s3_amazonaws_com.test_bucket.chef_s3.delete_checksums">>}
                                    ]],

    BadTests = [
                ?_assertError({bad_prefix, {bad, juju}},
                              oc_chef_wm_base:stats_hero_label({bad, juju})) ],
    GoodTests ++ BadTests.

verify_request_signature_test_() ->
    {foreach,
     fun() ->
             meck:expect(wrq, get_req_header, fun(_Header, _Req) ->
                                                      "username"
                                              end)
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"returns {halt, 503} on no_connections",
       fun() ->
               Req = make_req_data(),
               State = make_base_state(),
               meck:expect(chef_db, fetch_requestors, fun(_Context, _OrgId, _Name) ->
                                                              {error, no_connections}
                                                      end),
               {Return, _Req1, State1} = oc_chef_wm_base:verify_request_signature(Req, State),
               ?assertEqual({halt, 503}, Return),
               ?assertEqual({error_finding_user_or_client, no_connections}, State1#base_state.log_msg)
       end},
      {"returns {halt, 500} on unknown errors",
       fun() ->
               Req = make_req_data(),
               State = make_base_state(),
               meck:expect(chef_db, fetch_requestors, fun(_Context, _OrgId, _Name) ->
                                                              {error, uh_oh}
                                                      end),
               {Return, _Req1, State1} = oc_chef_wm_base:verify_request_signature(Req, State),
               ?assertEqual({halt, 500}, Return),
               ?assertEqual({error_finding_user_or_client, uh_oh}, State1#base_state.log_msg)
       end},
      {"returns false (401) when user / client not found",
       fun() ->
               Req = make_req_data(),
               State = make_base_state(),
               meck:expect(chef_db, fetch_requestors, fun(_Context, _OrgId, _Name) ->
                                                              not_found
                                                      end),
               {Return, _Req1, State1} = oc_chef_wm_base:verify_request_signature(Req, State),
               ?assertEqual(false, Return),
               ?assertEqual({not_found, user_or_client}, State1#base_state.log_msg)
       end}
     ]}.

make_req_data() ->
    #wm_reqdata{}.

make_base_state() ->
    #base_state{
       organization_name = <<"ponyville">>,
       organization_guid = <<"12341234123412341234123412341234">>,
       auth_skew = 900
      }.
