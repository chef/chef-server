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

-module(chef_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

sanitize_label_test_() ->
    [?_assertEqual(Sanitized,
                   chef_metrics:sanitize_label_component(Component))
  || {Component, Sanitized} <- [{"foobar", <<"foobar">>},
                                {"foo.bar", <<"foo_bar">>},
                                {"foo.bar.baz.buz", <<"foo_bar_baz_buz">>},

                                %% and just for fun...
                                {"........", <<"________">>}]].

extract_host_test_() ->
    [?_assertEqual(Host,
                   chef_metrics:extract_host(Url))
     || {Url, Host} <- [{"http://www.chef.io", "www.chef.io"},
                        {"https://www.chef.io", "www.chef.io"},
                        {"http://www.chef.io/", "www.chef.io"},
                        {"https://www.chef.io/", "www.chef.io"},
                        {"http://www.chef.io:1234", "www.chef.io"},
                        {"https://www.chef.io:1234", "www.chef.io"},
                        {"http://www.chef.io:1234/", "www.chef.io"},
                        {"https://www.chef.io:1234/", "www.chef.io"},
                        {"http://www.chef.io/foo/bar/baz", "www.chef.io"},
                        {"https://www.chef.io/foo/bar/baz", "www.chef.io"},
                        {"http://www.chef.io:1234/foo/bar/baz", "www.chef.io"},
                        {"https://www.chef.io:1234/foo/bar/baz" "www.chef.io"},

                        %% Make sure IP addresses are kosher, if that's how you roll
                        {"http://123.123.123.123", "123.123.123.123"},
                        {"http://123.123.123.123:666", "123.123.123.123"}
                       ]
    ].

label_test_() ->
    {
      foreachx,
      fun({S3Url, Bucket}) ->
              application:set_env(chef_objects, s3_url, S3Url),
              application:set_env(chef_objects, s3_platform_bucket_name, Bucket)
      end,
      fun(_,_) -> ok end,
      [{{Url, Bucket},
        fun(_,_) ->
                ?_assertEqual(Label,
                             chef_metrics:label(Upstream, {Mod, Fun}))
        end} || {Url, Bucket, Upstream, Mod, Fun, Label} <- [{"http://s3.amazonaws.com", "i.haz.a.bukkit",
                                                              rdbms, chef_sql, create_node,
                                                              <<"rdbms.chef_sql.create_node">>},
                                                             {"http://s3.amazonaws.com", "i.haz.a.bukkit",
                                                              solr, chef_solr, search,
                                                              <<"solr.chef_solr.search">>},
                                                             {"http://s3.amazonaws.com", "i.haz.a.bukkit",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.s3_amazonaws_com.i_haz_a_bukkit.chef_s3.delete_checksums">>},
                                                             {"http://s3.amazonaws.com", "bukkit",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.s3_amazonaws_com.bukkit.chef_s3.delete_checksums">>},
                                                             {"http://api.chef.io", "cookbook_storage",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.api_chef_io.cookbook_storage.chef_s3.delete_checksums">>},
                                                             {"https://my.private_chef.com:9999", "cookbook_storage",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.my_private_chef_com.cookbook_storage.chef_s3.delete_checksums">>},
                                                             {"https://127.0.0.1:9999", "cookbook_storage",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.127_0_0_1.cookbook_storage.chef_s3.delete_checksums">>},
                                                             {"https://localhost", "cookbook_storage",
                                                               s3, chef_s3, delete_checksums,
                                                              <<"s3.localhost.cookbook_storage.chef_s3.delete_checksums">>}
                                                            ]
      ]}.
