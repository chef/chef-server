%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%-------------------------------------------------------------------
%% @author Eric B Merritt <ericbmerritt@gmail.com>
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

-module(bkswt_api_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bookshelf_wi/src/internal.hrl").

-define(STR_CHARS, "abcdefghijklmnopqrstuvwxyz").

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(sec_fail, Config0) ->
    Config1 = init_per_testcase(not_sec_fail, Config0),
    AccessKeyID = random_string(10, "abcdefghijklmnopqrstuvwxyz"),
    SecretAccessKey = random_string(30, "abcdefghijklmnopqrstuvwxyz"),
    Port = 4321,
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://127.0.0.1:~p",
                                                      [Port])),
                         path),
    lists:keyreplace(s3_conf, 1, Config1, {s3_conf, S3State});
init_per_testcase(_TestCase, Config) ->
    %% This fixes another rebar brokenness. We cant specify any options to
    %% common test in rebar
    Seed = now(),
    random:seed(Seed),
    error_logger:info_msg("Using random seed: ~p~n", [Seed]),
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),
    LogDir = filename:join(proplists:get_value(priv_dir, Config),
                           "logs"),
    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    error_logger:info_msg("Using disk_store: ~p~n", [DiskStore]),
    AccessKeyID = random_string(10, "abcdefghijklmnopqrstuvwxyz"),
    SecretAccessKey = random_string(30, "abcdefghijklmnopqrstuvwxyz"),
    application:set_env(bookshelf_wi, disk_store, DiskStore),
    application:set_env(bookshelf_wi, keys, {AccessKeyID, SecretAccessKey}),
    application:set_env(bookshelf_wi, log_dir, LogDir),
    application:set_env(bookshelf_wi, stream_download, true),
    ok = bksw_app:manual_start(),
    %% force webmachine to pickup new dispatch_list. I don't understand why it
    %% isn't enough to do application:stop/start for webmachine, but it isn't.
    bksw_conf:reset_dispatch(),
    %% increase max sessions per server for ibrowse
    application:set_env(ibrowse, default_max_sessions, 256),
    %% disable request pipelining for ibrowse.
    application:set_env(ibrowse, default_max_pipeline_size, 1),
    Port = 4321,
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://127.0.0.1:~p",
                                                      [Port])),
                          path),
    [{s3_conf, S3State}, {disk_store, DiskStore} | Config].

end_per_testcase(_TestCase, _Config) ->
    bksw_app:manual_stop(),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [head_object, put_object, wi_basic, sec_fail, signed_url, signed_url_fail].

%%====================================================================
%% TEST CASES
%%====================================================================

wi_basic(doc) ->
    ["should be able to create, list & delete buckets"];
wi_basic(suite) ->
    [];
wi_basic(Config) when is_list(Config) ->
    {Timings, _} =
        timer:tc(fun() ->
                         S3Conf = proplists:get_value(s3_conf, Config),
                         %% Get much more then about 800 here and you start running out of file
                         %% descriptors on a normal box
                         Count = 50,
                         Buckets = [random_binary() || _ <- lists:seq(1, Count)],
                         error_logger:error_msg("~p~n", [Buckets]),
                         Res = ec_plists:map(fun(B) ->
                                                     mini_s3:create_bucket(B, public_read_write, none, S3Conf)
                                             end,
                                             Buckets),
                         ?assert(lists:all(fun(Val) -> ok == Val end, Res)),
                         [{buckets, Details}] = mini_s3:list_buckets(S3Conf),
                         BucketNames = lists:map(fun(Opts) -> proplists:get_value(name, Opts) end,
                                                 Details),
                         ?assert(lists:all(fun(Name) -> lists:member(Name, BucketNames) end, Buckets)),
                         [DelBuck | _] = Buckets,
                         ?assertEqual(ok, mini_s3:delete_bucket(DelBuck, S3Conf)),
                         [{buckets, NewBuckets}] = mini_s3:list_buckets(S3Conf),
                         error_logger:info_msg("NewBuckets: ~p~n", [NewBuckets]),
                         ?assertEqual(Count - 1, length(NewBuckets))
                 end),
    error_logger:info_msg("WI_BASIC TIMING ~p", [Timings]).


put_object(doc) ->
    ["should be able to put and list objects"];
put_object(suite) ->
    [];
put_object(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = "bukkit",
    ?assertEqual(ok, mini_s3:create_bucket(Bucket, public_read_write, none, S3Conf)),
    BucketContents = mini_s3:list_objects(Bucket, [], S3Conf),
    ?assertEqual(Bucket, proplists:get_value(name, BucketContents)),
    ?assertEqual([], proplists:get_value(contents, BucketContents)),
    Count = 50,
    Objs = [filename:join(random_binary(), random_binary()) ||
               _ <- lists:seq(1,Count)],
    ec_plists:map(fun(F) ->
                          mini_s3:put_object(Bucket, F, F, [], [], S3Conf)
                  end, Objs),
    Result = mini_s3:list_objects(Bucket, [], S3Conf),
    ObjList = proplists:get_value(contents, Result),
    ?assertEqual(Count, length(ObjList)),
    ec_plists:map(fun(Obj) ->
                          Key = proplists:get_value(key, Obj),
                          ObjDetail = mini_s3:get_object(Bucket, Key, [], S3Conf),
                          ?assertMatch(Key,
                                       erlang:binary_to_list(proplists:get_value(content, ObjDetail)))
                  end, ObjList).

head_object(doc) ->
    ["supports HEAD operations with PUT"];
head_object(suite) ->
    [];
head_object(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = "head-put-tests",
    ?assertEqual(ok, mini_s3:create_bucket(Bucket, public_read_write, none, S3Conf)),
    BucketContents = mini_s3:list_objects(Bucket, [], S3Conf),
    ?assertEqual(Bucket, proplists:get_value(name, BucketContents)),
    ?assertEqual([], proplists:get_value(contents, BucketContents)),
    Count = 50,
    Objs = [filename:join(random_binary(), random_binary()) ||
               _ <- lists:seq(1,Count)],
    ec_plists:map(fun(F) ->
                          mini_s3:put_object(Bucket, F, F, [], [], S3Conf)
                  end, Objs),
    Got = ec_plists:ftmap(fun(Obj) ->
                                  mini_s3:get_object_metadata(Bucket, Obj, [], S3Conf)
                          end, Objs, 10000),
    error_logger:info_msg("Got: ~p~n", [Got]),
    [ ?assertMatch({value, _}, Item) || Item <- Got ].



sec_fail(doc) ->
    ["Check authentication failure on the part of the caller"];
sec_fail(suite) ->
    [];
sec_fail(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_binary(),
    ?assertError({aws_error, {http_error, 403, _}},
                 mini_s3:create_bucket(Bucket, public_read_write, none, S3Conf)).

signed_url(doc) ->
    ["Test that signed urls actually work"];
signed_url(suite) ->
    [];
signed_url(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_binary(),
    mini_s3:create_bucket(Bucket, public_read_write, none, S3Conf),
    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(crypto:md5(Content)))}],
    SignedUrl = mini_s3:s3_url('put', Bucket, "foo", 1000,
                               Headers,
                               S3Conf),
    Response = httpc:request(put, {erlang:binary_to_list(SignedUrl),
                                   Headers,
                                   "text/xml", Content}, [], []),
    ?assertMatch({ok, _}, Response),
    Response2 = httpc:request(put, {erlang:binary_to_list(SignedUrl),
                                   [{"content-type", "text/xml"},
                                    {"content-md5",
                                     erlang:binary_to_list(base64:encode(
                                                             crypto:md5("Something Else")))}],
                                    "text/xml", Content}, [], []),
  ?assertMatch({ok,{{"HTTP/1.1",403,"Forbidden"}, _, _}},
               Response2).

signed_url_fail(doc) ->
    ["Test that signed url expiration actually works"];
signed_url_fail(suite) ->
    [];
signed_url_fail(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_binary(),
    mini_s3:create_bucket(Bucket, public_read_write, none, S3Conf),
    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(crypto:md5(Content)))}],
    SignedUrl = mini_s3:s3_url('put', Bucket, "foo", -1,
                               Headers,
                               S3Conf),
    Response = httpc:request(put, {erlang:binary_to_list(SignedUrl),
                                   Headers,
                                   "text/xml", Content}, [], []),
    ?assertMatch({ok,{{"HTTP/1.1",403,"Forbidden"}, _, _}},
                 Response).


%%====================================================================
%% Utility Functions
%%====================================================================
random_binary() ->
    random_string(30, ?STR_CHARS).

random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars) | Acc]
                end, [], lists:seq(1, Length)).
