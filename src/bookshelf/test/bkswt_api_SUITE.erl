%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%-------------------------------------------------------------------
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2012-5 Opscode, Inc. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/internal.hrl").

-define(STANDALONE_BOOKSHELF, true).

-define(STR_CHARS, "abcdefghijklmnopqrstuvwxyz").

%% Loads the environment from a config file
load_default_config() ->
    ConfigPath = case os:getenv("BOOKSHELF_CT_CONFIG_PATH") of
                     false -> "/var/opt/opscode/bookshelf/sys.config";
                     X -> X
                 end,
    load_config(ConfigPath).

load_config(File) ->
    {ok, [Terms]} = file:consult(File),
    [subkeys_to_env(Term, Subkeys) || {Term, Subkeys} <- Terms].

subkeys_to_env(Term, Subkeys) ->
    [application:set_env(Term, Subkey, Value) || {Subkey, Value} <- Subkeys].

start_bookshelf() ->
    %% For common test, we don't want to stop sasl or else we end up not getting
    %% complete logs in the test reports. So we ensure sasl is started, but omit
    %% it from the ?APPS list so that we don't start/stop on each test.
    application:start(sasl),

    %% we start lager since we depend on it for the release. However,
    %% we want to keep error_logger on its own so that we continue to
    %% see messages in common test output.
    lager_common_test_backend:bounce(debug),

    case application:ensure_all_started(bookshelf) of
        {ok, Apps} ->
            ct:pal("Apps Started: ~p", [Apps]),
            {ok, Apps};
        {error, Es} ->
            erlang:error({application_start_failed, Es})
    end.

stop_bookshelf(Config) ->
    [application:stop(A)|| A <- ?config(apps, Config)].

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config0) ->
    load_default_config(),
    application:set_env(bookshelf, storage_type, sql),

    ct:print("Initing:~n~p~n", [application:get_all_env(bookshelf)]),

    Apps =
        case ?STANDALONE_BOOKSHELF of
            true ->
                application:start(sasl),
                application:ensure_all_started(mini_s3),
                [xmerl,mochiweb,webmachine,erlsom,ibrowse,mini_s3,envy,epgsql,pooler,sqerl,
                 opscoderl_wm,iso8601,runtime_tools,tools,bookshelf];
            false ->
                {ok, Apps0} = start_bookshelf(),
                %% force webmachine to pickup new dispatch_list. I don't understand why it
                %% isn't enough to do application:stop/start for webmachine, but it isn't.
                bksw_conf:reset_dispatch(),
                Apps0
        end,

    ct:print("Apps: ~n~p", [Apps]),

    %% increase max sessions per server for ibrowse
    application:set_env(ibrowse, default_max_sessions, 256),
    %% disable request pipelining for ibrowse.
    application:set_env(ibrowse, default_max_pipeline_size, 1),

    {port, Port} = bksw_conf:port(),
    {ip, Ip} = bksw_conf:ip(),
    {keys, {AccessKeyID, SecretAccessKey}} = bksw_conf:keys(),
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://~s:~p", [Ip, Port])),
                          path),
    [{s3_conf, S3State}, {apps, lists:reverse(Apps)} | Config0].

end_per_testcase(_TestCase, Config) ->
    stop_bookshelf(Config),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    [
%     bucket_basic,
     %% bucket_many, % Intermittednt
     %% bucket_encoding,
     %% bucket_delete,
     %% head_object,
     %% put_object,
     %% object_roundtrip,
     %% object_delete,

     %% sec_fail, % Failing
     %% signed_url,
     %% signed_url_fail,
     at_the_same_time,
%     upgrade_from_v0
     noop
].


%%====================================================================
%% TEST CASES
%%====================================================================

noop(doc) ->
    ["No Op"];
noop(suite) ->
    [];
noop(_) ->
    ok.

bucket_basic(doc) ->
    ["should create, view, and delete a bucket"];
bucket_basic(suite) ->
    [];
bucket_basic(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    BucketName = "testbucket",
    ?assertEqual(ok, mini_s3:create_bucket(BucketName, public_read_write, none, S3Conf)),
    ?assert(bucket_exists(BucketName, S3Conf)),

    ?assertEqual(ok, mini_s3:delete_bucket(BucketName, S3Conf)),

    ?assert(not bucket_exists(BucketName, S3Conf)).

bucket_many(doc) ->
    ["should create, view, and delete multiple buckets"];
bucket_many(suite) ->
    [];
bucket_many(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),

    BucketsBefore = bucket_list(S3Conf),
    ct:print("Prebucket: ~p~n", [ BucketsBefore ] ),

    % create
    Buckets = [random_binary() || _ <- lists:seq(1, 50)],
    NewBuckets = lists:subtract(Buckets, Buckets),
    Res = ec_plists:map(fun(B) ->
                                mini_s3:create_bucket(B, public_read_write, none, S3Conf)
                        end,
                        NewBuckets),
    ?assert(lists:all(fun(Val) -> ok == Val end, Res)),

    BucketsAfter = bucket_list(S3Conf),
    BucketsExpected = lists:usort(BucketsBefore ++ NewBuckets),
    ?assertEqual(BucketsExpected, BucketsAfter),

    % delete
    DRes = ec_plists:map(fun(B) ->
                                mini_s3:delete_bucket(B, S3Conf)
                         end,
                         NewBuckets),
    ?assert(lists:all(fun(Val) -> ok == Val end, DRes)),

    % sanity check
    ?assertEqual(BucketsBefore, bucket_list(S3Conf)).

bucket_encoding(doc) ->
    ["should be able to create buckets with URL encoding"];
bucket_encoding(suite) ->
    [];
bucket_encoding(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),

    OddBucket = "a bucket",
    OddBucketEnc = "a%20bucket",
    mini_s3:create_bucket(OddBucketEnc, public_read_write, none, S3Conf),
    ?assert(bucket_exists(OddBucket, S3Conf)),

    OddResult = mini_s3:list_objects(OddBucketEnc, [], S3Conf),
    ?assertEqual(OddBucket, ?config(name, OddResult)),
    ?assertEqual([], ?config(contents, OddResult)),
    mini_s3:delete_bucket(OddBucketEnc, S3Conf).

head_object(doc) ->
    ["supports HEAD operations with PUT"];
head_object(suite) ->
    [];
head_object(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = "head-put-tests",

    ensure_bucket(Bucket, S3Conf),
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
    [ ?assertMatch({value, _}, Item) || Item <- Got ],
    %% verify 404 behavior
    V = try
            mini_s3:get_object_metadata(Bucket, "no-such-object", [], S3Conf)
        catch
            error:Why ->
                Why
        end,
    ct:pal("HEAD 404: ~p", [V]),

    % Cleanup
    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)).


put_object(doc) ->
    ["should be able to put and list objects"];
put_object(suite) ->
    [];
put_object(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    ct:print("S3Conf hahaha ~p~n", [S3Conf]),

    Bucket = random_bucket(),
    ct:print("Bucket ~p~n",[Bucket]),

    ensure_bucket(Bucket, S3Conf),

    BucketContents = mini_s3:list_objects(Bucket, [], S3Conf),
    ?assertEqual(Bucket, proplists:get_value(name, BucketContents)),
    ?assertEqual([], proplists:get_value(contents, BucketContents)),

    Count = 50,
    Objs = [random_path() || _ <- lists:seq(1,Count)],
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
                  end, ObjList),

    % Cleanup
    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)).


object_roundtrip(doc) ->
    ["Can put a object and get it back"];
object_roundtrip(suite) ->
    [];
object_roundtrip(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_bucket(),
    ensure_bucket(Bucket,S3Conf),
    Name = random_path(),
    Data = erlang:iolist_to_binary(test_data_text(128*1024)),
    mini_s3:put_object(Bucket, Name, Data, [], [], S3Conf),
    Result = mini_s3:get_object(Bucket, Name, [], S3Conf),
    ct:print("Fetched ~p~n", [Name]),
    DataRoundtrip = proplists:get_value(content, Result),
    ?assertEqual(Data, DataRoundtrip).

object_delete(doc) ->
    ["Can create and delete an object"];
object_delete(suite) ->
    [];
object_delete(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),

    Bucket = random_bucket(),
    ensure_bucket(Bucket,S3Conf),
    Name = random_path(),
    Data = "TestData\nMore test data",
    mini_s3:put_object(Bucket, Name, Data, [], [], S3Conf),
    ?assert(file_exists(Bucket, Name, S3Conf)),

    mini_s3:delete_object(Bucket, Name, S3Conf),

    ?assert(not file_exists(Bucket, Name, S3Conf)).


bucket_delete(doc) ->
    ["Can create and delete an bucket"];
bucket_delete(suite) ->
    [];
bucket_delete(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),

    Bucket = random_bucket(),
    ensure_bucket(Bucket,S3Conf),

    %% Add a file
    Name = random_path(),
    Data = "TestData\nMore test data",
    mini_s3:put_object(Bucket, Name, Data, [], [], S3Conf),
    ?assert(file_exists(Bucket, Name, S3Conf)),

    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)),

    %% Verify it's gone
    ?assert(not bucket_exists(Bucket, S3Conf)),

    %% Recreate bucket and check that no files reappear
    ensure_bucket(Bucket,S3Conf),
    ?assert(not file_exists(Bucket, Name, S3Conf)),

    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)).


sec_fail(doc) ->
    ["Check authentication failure on the part of the caller"];
sec_fail(suite) ->
    [];
sec_fail(Config) when is_list(Config) ->
    BogusS3Conf = {config,
                   "http://127.0.0.1:4321",
                   <<"nopenope">>,
                   <<"evenmorenope">>,
                   path},
    Bucket = "thisshouldfail",
    ct:print("trying: mini_s3:create_bucket(~p, public_read_write, none, ~p)~n", [Bucket, BogusS3Conf]),
    ?assertError({aws_error, {http_error, 403, _}},
                 mini_s3:create_bucket(Bucket, public_read_write, none, BogusS3Conf)),
    %% also verify that unsigned URL requests don't crash
    {ok, Status, _H, Body} = ibrowse:send_req("http://127.0.0.1:4321/foobar", [],
                                              get),
    ?assertEqual("403", Status),
    ?assert(string:str(Body, "<Message>Access Denied</Message>") > 0).

signed_url(doc) ->
    ["Test that signed urls actually work"];
signed_url(suite) ->
    [];
signed_url(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = random_binary(),
    ensure_bucket(Bucket, S3Conf),
    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(crypto:hash(md5, Content)))}],
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
                                                             crypto:hash(md5, "Something Else")))}],
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
    ensure_bucket(Bucket, S3Conf),

    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(crypto:hash(md5, Content)))}],
    SignedUrl = mini_s3:s3_url('put', Bucket, "foo", -1,
                               Headers,
                               S3Conf),
    Response = httpc:request(put, {erlang:binary_to_list(SignedUrl),
                                   Headers,
                                   "text/xml", Content}, [], []),
    ?assertMatch({ok,{{"HTTP/1.1",403,"Forbidden"}, _, _}},
                 Response).

at_the_same_time(doc) ->
    ["should handle concurrent reads and writes"];
at_the_same_time(suite) -> [];
at_the_same_time(Config) when is_list(Config) ->
    S3Conf = proplists:get_value(s3_conf, Config),
    Bucket = "bukkit",

    ensure_bucket(Bucket, S3Conf),
    BucketContents = mini_s3:list_objects(Bucket, [], S3Conf),
    ?assertEqual(Bucket, proplists:get_value(name, BucketContents)),
    ?assertEqual([], proplists:get_value(contents, BucketContents)),
    Count = 100,
    BigData = list_to_binary(lists:duplicate(200000, 2)),
    Key = filename:join(random_binary(), random_binary()),
    error_logger:info_report({at_the_same_time, key, Key}),
    mini_s3:put_object(Bucket, Key, BigData, [], [], S3Conf),
    DoOp = fun(read) ->
                   Res = mini_s3:get_object(Bucket, Key, [], S3Conf),
                   ResContent = proplists:get_value(content, Res),
                   ?assertEqual(BigData, ResContent),
                   ok;
              (write) ->
                   mini_s3:put_object(Bucket, Key, BigData, [], [], S3Conf),
                   ok
           end,
    Ops = lists:flatten([[write, read] || _ <- lists:seq(1, Count)]),
    Results = ec_plists:map(fun(O) -> DoOp(O) end, Ops),
    ?assertEqual([ ok || _ <- lists:seq(1, 2 * Count)], Results),
    error_logger:info_msg("done with plists map of ops"),
    Result = mini_s3:list_objects(Bucket, [], S3Conf),
    ObjList = proplists:get_value(contents, Result),
    ?assertEqual(1, length(ObjList)),
    %% Cleanup
    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)).

upgrade_from_v0(doc) ->
    ["Upgrades from version 0 disk format to current version"];
upgrade_from_v0(suite) -> [];
upgrade_from_v0(Config) ->
    ShouldExist = [
                   {"bucket-1", "xjbrpodcionabrzhikgliowdzvbvbc/kqvfgzhnlkizzvbidsxwavrktxcasx"},
                   {"bucket-1", "zrcsghibdgwjghkqsdajycrjwitntu/ahnsvorjeauuwusthkdunsslzffkfn"},
                   {"bucket-2", "drniwxjwkasvovjjoafthnoqgtlung/lhfivdpsosyjybnmfpxkgplycrclmz"},
                   {"bucket-2", "nbmxbspdkbubastgtzzkhtunqznkcg/afbtmzfyyftrdxfbnmkslckewisxns"},
                   {"bucket%20space", "xjbrpodcionabrzhikgliowdzvbvbc/kqvfgzhnlkizzvbidsxwavrktxcasx"}
                  ],

    S3Conf = ?config(s3_conf, Config),

    AssertCount = fun(Bucket, Count) ->
                           Res = mini_s3:list_objects(Bucket, [], S3Conf),
                           Contents = proplists:get_value(contents, Res),
                           ?assertEqual(Count, length(Contents))
                   end,

    AssertCount("bucket-1", 2),
    AssertCount("bucket-2", 45),
    AssertCount("bucket-3", 1),
    AssertCount("bucket-4", 0),
    AssertCount("bucket%20space", 2),

    [ begin
          Res = mini_s3:get_object(Bucket, Key, [], S3Conf),
          ct:pal("Found: ~p~n", [Res])
      end || {Bucket, Key} <- ShouldExist ],

    ok.


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

random_bucket() ->
    RandomSuffix  = random_string(10, ?STR_CHARS),
    string:concat("bukkit-", RandomSuffix).

random_path() ->
    filename:join(random_binary(), random_binary()).

test_data(Size) ->
    crypto:random_bytes(Size).

test_data_text(Size) ->
    random_string(Size, " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ).

bucket_list(S3Conf) ->
    [{buckets, Details}] = mini_s3:list_buckets(S3Conf),
    lists:map(fun(Opts) -> proplists:get_value(name, Opts) end, Details).
bucket_exists(Name, S3Conf) ->
    BucketNames = bucket_list(S3Conf),
    lists:member(Name, BucketNames).

ensure_bucket(Bucket, Config) ->
    case bucket_exists(Bucket, Config) of
        true -> ?assertEqual(ok, mini_s3:delete_bucket(Bucket, Config));
        _ -> ok
    end,
    ?assertEqual(ok, mini_s3:create_bucket(Bucket, public_read_write, none, Config)).

file_exists(Bucket, Name, S3Conf) ->
    List = mini_s3:list_objects(Bucket, [], S3Conf),
    Files = [ proplists:get_value(key, I) || I <- proplists:get_value(contents, List)],
    lists:member(Name, Files).
