%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2013-2018 Chef Software, Inc.
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
