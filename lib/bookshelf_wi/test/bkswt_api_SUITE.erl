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

init_per_testcase(_TestCase, Config) ->
    %% This fixes another rebar brokenness. We cant specify any options to
    %% common test in rebar
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),
    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    DataDir = proplists:get_value(data_dir, Config),
    application:set_env(bookshelf_store, disk_store, DiskStore),
    application:set_env(bookshelf_wi, certfile, filename:join(DataDir, "cert.pem")),
    application:set_env(bookshelf_wi, keyfile, filename:join(DataDir, "key.pem")),
    application:set_env(bookshelf_wi, password, "cowboy"),
    ok = bksw_app:manual_start(),
    AccessKeyID = "",
    SecretAccessKey = "",
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
    [wi_basic].

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
    Count = 500,
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
