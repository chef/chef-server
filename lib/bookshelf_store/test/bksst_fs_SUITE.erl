%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(bksst_fs_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bookshelf_store/include/bookshelf_store.hrl").

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all(doc) ->
    ["This does something very similar to bksst_fs_SUITE but "
     "does it via the full OTP Stack"].

all() ->
    [bookshelf_fs, bookshelf_fs_object, bookshelf_fs_stream].

%%====================================================================
%% TEST CASES
%%====================================================================

bookshelf_fs(doc) ->
    ["should be able to create, list & delete buckets"];
bookshelf_fs(suite) ->
    [];
bookshelf_fs(Config) when is_list(Config) ->
    PrivDir = list_to_binary(filename:join(proplists:get_value(priv_dir, Config),
                                           "fs-store")),
    file:make_dir(PrivDir),
    Store0 = bkss_store:new(bkss_fs, PrivDir),
    Buckets = [<<"lol">>, <<"cat">>, <<"walrus">>, <<"bukkit">>],
    Store4 =
        lists:foldl(
          fun(B, Store1) ->
                  {Store2, R1} = bkss_store:bucket_create(Store1, B),
                  ?assertEqual(ok, R1),
                  ?assert(bkss_store:bucket_exists(Store2, B)),
                  {Store3, Result} = bkss_store:bucket_create(Store2, B),
                  ?assertMatch({error, _}, Result),
                  Store3
          end,
          Store0,
          Buckets),
    {Store5, R2} = bkss_store:bucket_delete(Store4, <<"cat">>),
    ?assertEqual(ok, R2),
    Pass2 = bkss_store:bucket_list(Store5),
    ?assert(lists:all(fun(#bucket{name=Name}) ->
                              lists:member(Name,  [<<"lol">>, <<"walrus">>, <<"bukkit">>])
                      end, Pass2)),
    ?assertEqual(3, length(Pass2)),
    ?assertNot(bkss_store:bucket_exists(Store5, <<"cat">>)).

bookshelf_fs_object(doc) ->
    ["should be able to list objects"];
bookshelf_fs_object(suite) ->
    [];
bookshelf_fs_object(Config) when is_list(Config) ->
    PrivDir = list_to_binary(filename:join(proplists:get_value(priv_dir, Config),
                                           "fs-store-object")),
    file:make_dir(PrivDir),
    Store0 = bkss_store:new(bkss_fs, PrivDir),
    Bucket = <<"bukkit">>,
    {Store1, R0} = bkss_store:bucket_create(Store0, Bucket),
    ?assertEqual(ok, R0),
    ?assertEqual([], bkss_store:obj_list(Store1, Bucket)),
    Objs = [<<"testing/123/hello">>, <<"hello">>],
    lists:foreach(
      fun(F) ->
              {_, {ok, _}} = bkss_store:obj_create(Store1, Bucket, F, F)
      end,
      Objs
     ),
    Records = bkss_store:obj_list(Store1, Bucket),
    ?assertEqual(2, length(Records)),
    lists:foreach(fun(#object{name=Path}) ->
                          Data = bkss_store:obj_get(Store1, Bucket, Path),
                          ?assertEqual({ok, Path}, Data)
                  end, Records).


bookshelf_fs_stream(doc) ->
    ["Test the bkss_store streaming protocols"];
bookshelf_fs_stream(suite) ->
    [];
bookshelf_fs_stream(Config) when is_list(Config) ->
    PrivDir = list_to_binary(filename:join(proplists:get_value(priv_dir, Config),
                                           "fs-store-object")),
    file:make_dir(PrivDir),
    Store0 = bkss_store:new(bkss_fs, PrivDir),
    Bucket = <<"bukkit_fs">>,
    {Store1, R0} = bkss_store:bucket_create(Store0, Bucket),
    ?assertEqual(ok, R0),
    ?assertEqual([], bkss_store:obj_list(Store1, Bucket)),
    Objs = [<<"test-foo">>, <<"hello-kitti">>],
    Trans = bkss_transport:new(bksst_test_transport, [<<"eeeeeeeeeeeeeeeeeeeeeeee"
                                                        "eeeeeeeeeeeeeeeeeeeeeeee"
                                                        "eeeeeeeeeeeeeeeeeeeeeeee"
                                                        "eeeeeeeeeeeeeeeeeeeeeeee">>]),
    lists:foreach(
      fun(F) ->
              {_, {ok, _}} = bkss_store:obj_recv(Store1, Bucket, F, Trans, <<>>, 100)
      end,
      Objs),

    lists:foreach(
      fun(F) ->
              {_, {ok, _}} = bkss_store:obj_send(Store1, Bucket, F, Trans)
      end,
      Objs),

    Records = bkss_store:obj_list(Store1, Bucket),
    ?assertEqual(2, length(Records)).

%%====================================================================
%% Utility Functions
%%====================================================================
