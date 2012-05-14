%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%%-------------------------------------------------------------------
-module(bksst_fs_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    io:format("~p", [Config]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [bookshelf_fs,bookshelf_fs_object].

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
    ?assertEqual(3, length(Pass2)),
    ?assertNot(bkss_store:bucket_exists(Store5, <<"cat">>)).

bookshelf_fs_object(doc) ->
    ["should be able to list objects"];
bookshelf_fs_object(suite) ->
    [];
bookshelf_fs_object(Config) ->
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
              ?assertEqual(ok, fixture_file(PrivDir, Bucket, F, F))
      end,
      Objs
     ),
    Records = bkss_store:obj_list(Store1, Bucket),
    ?assertEqual(2, length(Records)).

%%====================================================================
%% Utility Functions
%%====================================================================

fixture_file(PrivDir, Bucket, ObjectPath, Contents) ->
    FilePath = filename:join([PrivDir, Bucket, ObjectPath]),
    ?assertEqual(ok, filelib:ensure_dir(FilePath)),
    case file:open(FilePath, [write]) of
        {ok, IODevice} ->
            ?assertEqual(ok, file:write(IODevice, Contents)),
            ?assertEqual(ok, file:close(IODevice));
        E ->
            E
    end.
