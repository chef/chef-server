%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
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
                          lists:flatten(io_lib:format("http://localhost.localdomain:~p",
                                                      [Port]))),
    lists:keyreplace(s3_conf, 1, Config1, {s3_conf, S3State});
init_per_testcase(_TestCase, Config) ->
    %% This fixes another rebar brokenness. We cant specify any options to
    %% common test in rebar
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),
    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    AccessKeyID = random_string(10, "abcdefghijklmnopqrstuvwxyz"),
    SecretAccessKey = random_string(30, "abcdefghijklmnopqrstuvwxyz"),
    application:set_env(bookshelf_store, disk_store, DiskStore),
    application:set_env(bookshelf_wi, keys, {AccessKeyID, SecretAccessKey}),
    ok = bksw_app:manual_start(),
    Port = 4321,
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://localhost.localdomain:~p",
                                                      [Port]))),
    [{s3_conf, S3State} | Config].

end_per_testcase(_TestCase, _Config) ->
    bksw_app:manual_stop(),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [wi_basic, sec_fail, signed_url].

%%====================================================================
%% TEST CASES
%%====================================================================

wi_basic(doc) ->
    ["should be able to create, list & delete buckets"];
wi_basic(suite) ->
    [];
wi_basic(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    %% Get much more then about 800 here and you start running out of file
    %% descriptors on a normal box
    Count = 100,
    Buckets = [random_binary() || _ <- lists:seq(1, Count)],
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
    ?assertEqual(Count - 1, length(NewBuckets)).


sec_fail(doc) ->
    ["Check authentication failure on the part of the caller"];
sec_fail(suite) ->
    [];
sec_fail(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_binary(),
    ?assertError({aws_error, {http_error, 403, _, _}},
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

seed(Num) ->
        random:seed(Num, erlang:phash2(erlang:now()), erlang:phash2(erlang:now())).
