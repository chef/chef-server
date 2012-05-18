%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(bksst_store_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bookshelf_store/src/internal.hrl").
-include_lib("bookshelf_store/include/bookshelf_store.hrl").

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = bkss_app:manual_start(),
    %% This fixes another rebar brokenness. We cant specify any options to
    %% common test in rebar
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),
    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    ok = opset:set_value(disk_store, DiskStore, ?BOOKSHELF_CONFIG),
    Config.

end_per_testcase(_TestCase, _Config) ->
    bkss_app:manual_stop(),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [bookshelf_basic,bookshelf_object, bookshelf_copy].

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
    io:format("5"),
    ?assertNot(bookshelf_store:bucket_exists(<<"cat">>)).

bookshelf_object(doc) ->
    ["should be able to list objects"];
bookshelf_object(suite) ->
    [];
bookshelf_object(Config) when is_list(Config) ->
    Bucket = <<"bukkit">>,
    ?assertEqual(ok, bookshelf_store:bucket_create(Bucket)),
    ?assertEqual([], bookshelf_store:obj_list(Bucket)),
    Objs = [<<"testing/123/hello">>, <<"hello">>],
    ec_plists:map(fun(F) ->
                         {ok, _} = bookshelf_store:obj_create(Bucket, F, F)
                  end, Objs),
    ?assertEqual(2, length(bookshelf_store:obj_list(Bucket))),
    Records = bookshelf_store:obj_list(Bucket),
    ?assertEqual(2, length(Records)),
    ec_plists:map(fun(#object{name=Path}) ->
                          ?assertEqual({ok, Path}, bookshelf_store:obj_get(Bucket, Path))
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
    {ok, ConSize} =
        bookshelf_store:obj_copy(FromBucket, Path1, FromBucket, Path2),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path1),
                 bookshelf_store:obj_get(FromBucket, Path2)),
    {ok, ConSize} =
        bookshelf_store:obj_copy(FromBucket, Path1, ToBucket, Path1),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path1),
                 bookshelf_store:obj_get(ToBucket, Path1)),
    ?assertEqual(bookshelf_store:obj_get(FromBucket, Path2),
                 bookshelf_store:obj_get(ToBucket, Path1)).

random_binary() ->
    erlang:list_to_binary(
      random_string(30,
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIGKLMNOPQRSTUVWXYZ")).

random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars) | Acc]
                end, [], lists:seq(1, Length)).

seed(Num) ->
        random:seed(Num, erlang:phash2(erlang:now()), erlang:phash2(erlang:now())).
