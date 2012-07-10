%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(bksst_store_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bookshelf_store/include/bookshelf_store.hrl").

-define(STR_CHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIGKLMNOPQRSTUVWXYZ").

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
    seed(erlang:phash2({erlang:now(), erlang:node()})),
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),

    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    ok = application:set_env(bookshelf_store, disk_store, DiskStore),
    ok = bkss_app:manual_start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    bkss_app:manual_stop(),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [bookshelf_concurrent,
     bookshelf_concurrent_bucket,
     bookshelf_basic,
     bookshelf_object,
     bookshelf_copy,
     bookshelf_corruption,
     bookshelf_concurrent_access,
     bookshelf_stream].

%%====================================================================
%% TEST CASES
%%====================================================================

bookshelf_basic(doc) ->
    ["should be able to create, list & delete buckets"];
bookshelf_basic(suite) ->
    [];
bookshelf_basic(Config) when is_list(Config) ->
    Buckets = [<<"lol">>, <<"cat">>, <<"walrus">>, <<"bukkit">>],
    Res = ec_plists:map(fun(B) ->
                                ?assertEqual(ok, bookshelf_store:bucket_create(B)),
                                ?assert(bookshelf_store:bucket_exists(B))
                        end,
                        Buckets),
    ?assert(lists:all(fun(Val) -> ok == Val end, Res)),
    ?assertEqual(ok, bookshelf_store:bucket_delete(<<"cat">>)),
    ?assertEqual(3, length(bookshelf_store:bucket_list())),
    ?assertNot(bookshelf_store:bucket_exists(<<"cat">>)).

bookshelf_object(doc) ->
    ["should be able to list objects"];
bookshelf_object(suite) ->
    [];
bookshelf_object(Config) when is_list(Config) ->
    Bucket = <<"bukkit">>,
    ?assertEqual(ok, bookshelf_store:bucket_create(Bucket)),
    ?assertEqual([], bookshelf_store:obj_list(Bucket)),
    Objs = [filename:join(random_binary(), random_binary()) ||
               _ <- lists:seq(1,100)],
    ec_plists:map(fun(F) ->
                          {ok, _} = bookshelf_store:obj_create(Bucket, F, F)
                  end, Objs),
    ?assertEqual(100, length(bookshelf_store:obj_list(Bucket))),
    Records = bookshelf_store:obj_list(Bucket),
    ?assertEqual(100, length(Records)),
    ec_plists:map(fun(#object{name=Path}) ->
                          ?assertEqual({ok, Path},
                                       bookshelf_store:obj_get(Bucket,
                                                               Path))
                  end, Records).

bookshelf_copy(doc) ->
    ["Copy is complex lets make sure we get it right"];
bookshelf_copy(suite) ->
    [];
bookshelf_copy(Config) when is_list(Config) ->
    ec_plists:map(fun bksst_store_SUITE:copy_t1/1, lists:seq(1,100)),

    %% Here we do something a bit different. We start with 1 source bucket, but
    %% then copy that to a hundred or so destination buckets. This should work.
    SourceBucket = random_binary(),
    Path = filename:join(random_binary(), random_binary()),
    Contents = random_binary(),
    ConSize = erlang:size(Contents),
    bookshelf_store:bucket_create(SourceBucket),
    {ok, _} = bookshelf_store:obj_create(SourceBucket, Path, Contents),
    ec_plists:map(fun(Sq) ->
                          seed(Sq),
                          TargetBucket = random_binary(),
                          bookshelf_store:bucket_create(TargetBucket),
                          {ok, ConSize} =
                              bookshelf_store:obj_copy(SourceBucket, Path,
                                                       TargetBucket, Path),
                          ?assertEqual(bookshelf_store:obj_get(SourceBucket, Path),
                                       bookshelf_store:obj_get(TargetBucket, Path))
                  end, lists:seq(1,100)).

bookshelf_concurrent_bucket(doc) ->
    ["Test pure writing of multiple objects same bucket"];
bookshelf_concurrent_bucket(suite) ->
    [];
bookshelf_concurrent_bucket(Config) when is_list(Config) ->
    ProcessCount = 200,
    BucketName = random_binary(),
    bookshelf_store:bucket_create(BucketName),
    Action = fun(Sq) ->
                     seed(Sq),
                     Path = filename:join(random_binary(), random_binary()),
                     Data = erlang:iolist_to_binary(random_string(100, ?STR_CHARS)),
                     bookshelf_store:obj_create(BucketName, Path, Data),
                     {ok, WrittenData} = bookshelf_store:obj_get(BucketName, Path),
                     %% Again we dont need to check the data. the fact that it
                     %% inflates without throwing an error is a good corruption
                     %% check for us.
                     ?assertMatch(Data, WrittenData)
             end,
    ec_plists:map(Action, lists:seq(1,ProcessCount)).

bookshelf_concurrent(doc) ->
    ["Test pure writing of multiple objects in different buckets"];
bookshelf_concurrent(suite) ->
    [];
bookshelf_concurrent(Config) when is_list(Config) ->
    ProcessCount = 200,
    Action = fun(Sq) ->
                     seed(Sq),
                     BucketName = random_binary(),
                     bookshelf_store:bucket_create(BucketName),
                     Path = filename:join(random_binary(), random_binary()),
                     Data = erlang:iolist_to_binary(random_string(100, ?STR_CHARS)),
                     bookshelf_store:obj_create(BucketName, Path, Data),
                     {ok, WrittenData} = bookshelf_store:obj_get(BucketName, Path),
                     %% Again we dont need to check the data. the fact that it
                     %% inflates without throwing an error is a good corruption
                     %% check for us.
                     ?assertMatch(Data, WrittenData)
             end,
    ec_plists:map(Action, lists:seq(1,ProcessCount)).


bookshelf_corruption(doc) ->
    ["Multiple processes writing to the same object"];
bookshelf_corruption(suite) ->
    [];
bookshelf_corruption(Config) when is_list(Config) ->
    {Timings, _} = timer:tc(fun() ->
                                    ProcessCount = 200,
                                    BucketName = random_binary(),
                                    bookshelf_store:bucket_create(BucketName),
                                    Path = filename:join(random_binary(), random_binary()),
                                    Action = fun(Sq) ->
                                                     seed(Sq),
                                                     Data = random_string(100, ?STR_CHARS),
                                                     %% Our goal is to check and make sure there is no
                                                     %% corruption to that end we create a bunch of data, write
                                                     %% it and then read it.  Since there are <ProcessCount>
                                                     %% processes doing this at the same time the probability
                                                     %% that we read the same data we write is low. However,
                                                     %% that doesn't matter as long as things decompress
                                                     %% correctly. We are basically relying on zlibs crc-32
                                                     %% checks to do the corruption checks for us.
                                                     CompressedData = zlib:compress(Data),
                                                     bookshelf_store:obj_create(BucketName, Path, CompressedData),
                                                     {ok, WrittenData} = bookshelf_store:obj_get(BucketName, Path),
                                                     %% Again we dont need to check the data. the fact that it
                                                     %% inflates without throwing an error is a good corruption
                                                     %% check for us.
                                                     zlib:uncompress(WrittenData)
                                             end,
                                    ec_plists:map(Action, lists:seq(1,ProcessCount))
                            end),
    error_logger:info_msg("BOOKSHELF CORRUPTION~p", [Timings]).

bookshelf_concurrent_access(doc) ->
    ["Multiple processes reading and writing to different files in the same bucket"];
bookshelf_concurrent_access(suite) ->
    [];
bookshelf_concurrent_access(Config) when is_list(Config) ->
    ProcessCount = 200,
    BucketName = random_binary(),
    bookshelf_store:bucket_create(BucketName),
    Action = fun(Sq) ->
                     seed(Sq),
                     Path = filename:join(random_binary(), random_binary()),
                     Data = random_string(100, ?STR_CHARS),
                     CompressedData = zlib:compress(Data),
                     bookshelf_store:obj_create(BucketName, Path, CompressedData),
                     {ok, WrittenData} = bookshelf_store:obj_get(BucketName, Path),
                     zlib:uncompress(WrittenData)
             end,
    ec_plists:map(Action, lists:seq(1,ProcessCount)).

bookshelf_stream(doc) ->
    ["Test the bookshelf_store streaming protocols"];
bookshelf_stream(suite) ->
    [];
bookshelf_stream(Config) when is_list(Config) ->
    random:seed(erlang:now()),
    ProcessCount = 100,
    Bucket = random_binary(),
    ?assertEqual(ok,  bookshelf_store:bucket_create(Bucket)),
    ?assertEqual([], bookshelf_store:obj_list(Bucket)),
    Action = fun(Sq) ->
                     seed(Sq),
                     Path = filename:join(random_binary(), random_binary()),
                     ListData = [erlang:list_to_binary(random_string(100, ?STR_CHARS)) || _ <- lists:seq(1,100)],
                     MD5 = erlang:md5(erlang:iolist_to_binary(ListData)),
                     {ok, Ref} = bookshelf_store:obj_in_start(Bucket, Path),
                     lists:foreach(fun(Data) ->
                                           ok = bookshelf_store:obj_in(Ref, Data)
                                   end, ListData),
                     {ok, MD5} = bookshelf_store:obj_in_end(Ref),
                     {ok, Ref2} = bookshelf_store:obj_out_start(Bucket, Path, 100),
                     lists:foreach(fun(Data) ->
                                           R = bookshelf_store:obj_out(Ref2),
                                           ?assertMatch({ok, Data}, R)
                                   end, ListData),
                     done = bookshelf_store:obj_out(Ref2)
             end,
    ec_plists:map(Action, lists:seq(1,ProcessCount)).

%%====================================================================
%% Utility Functions
%%====================================================================
copy_t1(Sq) ->
    %% make sure we do get unique(ish) random names
    seed(Sq),
    FromBucket = random_binary(),
    ToBucket = random_binary(),
    Path1 = filename:join(random_binary(), random_binary()),
    Path2 = filename:join(random_binary(), random_binary()),
    Contents = random_binary(),
    ConSize = erlang:size(Contents),
    ?assertEqual(ok, bookshelf_store:bucket_create(FromBucket)),
    ?assertEqual(ok, bookshelf_store:bucket_create(ToBucket)),
    ?assertEqual([], bookshelf_store:obj_list(FromBucket)),
    ?assertEqual([], bookshelf_store:obj_list(ToBucket)),
    {ok, _} = bookshelf_store:obj_create(FromBucket, Path1, Contents),
    {ok, ReturnedConSize0} =
        bookshelf_store:obj_copy(FromBucket, Path1, FromBucket, Path2),
    ?assertMatch(ConSize, ReturnedConSize0),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path1),
                 bookshelf_store:obj_get(FromBucket, Path2)),
    {ok, ReturnedConSize1} =
        bookshelf_store:obj_copy(FromBucket, Path1, ToBucket, Path1),
    ?assertMatch(ConSize, ReturnedConSize1),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path1),
                 bookshelf_store:obj_get(ToBucket, Path1)),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path2),
                 bookshelf_store:obj_get(ToBucket, Path1)).

random_binary() ->
    erlang:list_to_binary(random_string(30, ?STR_CHARS)).

random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars) | Acc]
                end, [], lists:seq(1, Length)).

seed(Num) ->
        random:seed(Num, erlang:phash2(erlang:now()), erlang:phash2(erlang:now())).
