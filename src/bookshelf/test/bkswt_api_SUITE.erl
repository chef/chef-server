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
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include("../src/internal.hrl").

-define(STANDALONE_BOOKSHELF, false).

-define(STR_CHARS, "abcdefghijklmnopqrstuvwxyz").

-define(accesskeyid,     "e1efc99729beb175").
-define(secretaccesskey, "fc683cd9ed1990ca").

%% Loads the environment from a config file
load_default_config() ->
    case ?STANDALONE_BOOKSHELF of
        true ->
            load_config_from_file();
        false ->
            load_inline_config()
    end.

load_config_from_file() ->
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

%%
%% We expect an external instance of postgres set up and the following environment variables
%% This makes it easy to use something like pg_virtualenv
%% PGHOST:     default localhost
%% PGDATABASE: default bookshelf
%% PGUSER:     default bookshelf
%% PGPASSWORD: default test_sql_password
get_db_config() ->
    Host =     os:getenv("PGHOST", "localhost"),
    Db =       os:getenv("PGDATABASE", "bookshelf"),
    User =     os:getenv("PGUSER", "bookshelf"),
    Password = os:getenv("PGPASSWORD", "test_sql_password"),
    #{host => Host,
      db => Db,
      user => User,
      password => Password
     }.

load_inline_config() ->
    DbConf = get_db_config(),
    StaticBookshelfConfig = [{ip, "127.0.0.1"},
                             {port, 4321},
                             {disk_store, "/tmp/bukkits"},
                             {sql_retry_count, 0},
                             {sql_retry_delay, 5000},
                             {reqid_header_name, "X-Request-Id"}],
    StaticSqerlConfig = [{db_host, maps:get(host, DbConf)},
                         {db_port, 5432},
                         {db_user, maps:get(user, DbConf)},
                         {db_name, maps:get(db, DbConf)},
                         {idle_check, 10000},
                         {pooler_timeout, 4000},
                         {prepared_statements, {bksw_sql, statements, [pgsql]}},
                         {column_transforms,
                          [{<<"created_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}},
                           {<<"updated_at">>, {sqerl_transformers, convert_YMDHMS_tuple_to_datetime}}]}],

    application:set_env(pooler, pools, [[{name, sqerl},
                                         {max_count, 20},
                                         {init_count, 20},
                                         {queue_max, 200}, %% Use a very high queue-max to avoid test failures for high-concurrency tests
                                         {start_mfa, {sqerl_client, start_link, []}}]]),
    [application:set_env(sqerl, Key, Value) || {Key, Value} <- StaticSqerlConfig],
    [application:set_env(bookshelf, Key, Value) || {Key, Value} <- StaticBookshelfConfig].

start_bookshelf() ->
    %% For common test, we don't want to stop sasl or else we end up not getting
    %% complete logs in the test reports. So we ensure sasl is started, but omit
    %% it from the ?APPS list so that we don't start/stop on each test.
    application:start(sasl),

    %% we start lager since we depend on it for the release. However,
    %% we want to keep error_logger on its own so that we continue to
    %% see messages in common test output.
    lager_common_test_backend:bounce(error),

    case application:ensure_all_started(bookshelf) of
        {ok, Apps} ->
            {ok, Apps};
        {error, Es} ->
            erlang:error({application_start_failed, Es})
    end.

stop_bookshelf(Config) ->
    [application:stop(A)|| A <- lists:flatten([?config(apps, Config), pooler, sqerl])].

run_cmd(Args) ->
    os:cmd(string:join(Args, " ")).

setup_chef_secrets() ->
    application:set_env(sqerl, config_cb, {chef_secrets_sqerl, config, [{<<"bookshelf">>, <<"sql_password">>}]}),
    application:set_env(chef_secrets, provider, chef_secrets_mock_provider),
    application:set_env(chef_secrets, provider_config, []),
    {ok, FileContent} = file:read_file(filename:join(code:priv_dir(bookshelf), "../test/secrets.json")),
    FakeSecretsData = jiffy:decode(FileContent),
    meck:new(chef_secrets_mock_provider, [non_strict]),
    meck:expect(chef_secrets_mock_provider, read, fun(_Config) -> {ok, FakeSecretsData} end).

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

set_storage_type_for_test_case(Casename) ->
    case lists:last(string:tokens(atom_to_list(Casename), "_")) of
        "sql" ->
            application:set_env(bookshelf, storage_type, sql);
        _ ->
            application:set_env(bookshelf, storage_type, filesystem)
    end.

init_per_testcase(upgrade_from_v0, Config) ->
    %% This fixes another rebar brokenness. We cant specify any options to
    %% common test in rebar
    setup_chef_secrets(),
    Format0Data = filename:join([?config(data_dir, Config),
                                 "format_0_data"]),
    DiskStore = filename:join(proplists:get_value(priv_dir, Config),
                              random_string(10, "abcdefghijklmnopqrstuvwxyz")),
    LogDir = filename:join(proplists:get_value(priv_dir, Config),
                           "logs"),
    filelib:ensure_dir(filename:join(DiskStore, "tmp")),
    error_logger:info_msg("Using disk_store: ~p~n", [DiskStore]),
    CMD = ["cd ", Format0Data, "; tar cf - * | (cd ", DiskStore, "; tar xf -; mkdir bucket-4)"],
    ct:pal("copying format 0 data into disk store with command:~n~s~n", [CMD]),
    os:cmd(CMD),
    AccessKeyID = "e1efc99729beb175",
    SecretAccessKey = "fc683cd9ed1990ca",

    application:set_env(bookshelf, reqid_header_name, "X-Request-Id"),
    application:set_env(bookshelf, disk_store, DiskStore),
    application:set_env(bookshelf, log_dir, LogDir),
    application:set_env(bookshelf, stream_download, true),
    {ok, Apps} = start_bookshelf(),
    bksw_conf:reset_dispatch(),
    Port = 4321,
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://127.0.0.1:~p",
                                                      [Port])),
                          path),
    [{s3_conf, S3State}, {disk_store, DiskStore}, {apps, lists:reverse(Apps)} | Config];
init_per_testcase(Casename, Config0) ->
    load_default_config(),
    setup_chef_secrets(),
    set_storage_type_for_test_case(Casename),
    Apps = case ?STANDALONE_BOOKSHELF of
               true ->
                   application:start(sasl),
                   application:ensure_all_started(mini_s3),
                   [xmerl,mochiweb,webmachine,erlsom,mini_s3,envy,epgsql,pooler,sqerl,
                    opscoderl_wm,iso8601,runtime_tools,tools,bookshelf];
               false ->
                   {ok, Apps0} = start_bookshelf(),
                   %% force webmachine to pickup new dispatch_list. I don't understand why it
                   %% isn't enough to do application:stop/start for webmachine, but it isn't.
                   bksw_conf:reset_dispatch(),
                   Apps0
           end,

    Port = bksw_conf:port(),
    Ip = bksw_conf:ip(),
% NOTES FOR CODE REVIEW: this didn't work for some reason?
%    {AccessKeyID, SecretAccessKey} = bksw_conf:keys(),
    AccessKeyID = ?accesskeyid,
    SecretAccessKey = ?secretaccesskey,
    S3State = mini_s3:new(AccessKeyID, SecretAccessKey,
                          lists:flatten(io_lib:format("http://~s:~p", [Ip, Port])),
                          path),
    [{s3_conf, S3State}, {apps, lists:reverse(Apps)} | Config0].

end_per_testcase(_TestCase, Config) ->
    meck:unload(),
    stop_bookshelf(Config),
    ok.

all(doc) ->
    ["This test is runs the fs implementation of the bkss_store signature"].

all() ->
    lists:flatten([
                   filesystem_tests(),
                   sql_tests()]).

sql_tests() ->
    [ bucket_basic_sql,
      bucket_many_sql,
      bucket_encoding_sql,
      bucket_delete_sql,
      head_object_sql,
      put_object_sql,
      object_roundtrip_sql,
      object_delete_sql,
      cache_control_sql,
      sec_fail_sql,
      signed_url_sql,
      signed_url_fail_sql,
      at_the_same_time_sql,
      noop ].

filesystem_tests() ->
    [ bucket_basic,
      bucket_many,
      bucket_encoding,
      bucket_delete,
      head_object,
      put_object,
      object_roundtrip,
      object_delete,
      cache_control,
      sec_fail,
      signed_url,
      signed_url_fail,
      at_the_same_time,
      upgrade_from_v0].

%%====================================================================
%% TEST CASES
%%====================================================================
noop(doc) ->
    ["No Op"];
noop(suite) ->
    [];
noop(_) ->
    ok.

bucket_basic_sql(Arg) ->
    bucket_basic(Arg).
bucket_many_sql(Arg) ->
    bucket_many(Arg).
bucket_encoding_sql(Arg) ->
    bucket_encoding(Arg).
bucket_delete_sql(Arg) ->
    bucket_delete(Arg).
head_object_sql(Arg) ->
    head_object(Arg).
put_object_sql(Arg) ->
    put_object(Arg).
cache_control_sql(Arg) ->
    cache_control(Arg).
object_roundtrip_sql(Arg) ->
    object_roundtrip(Arg).
object_delete_sql(Arg) ->
    object_delete(Arg).
sec_fail_sql(Arg) ->
    sec_fail(Arg).
signed_url_sql(Arg) ->
    signed_url(Arg).
signed_url_fail_sql(Arg) ->
    signed_url_fail(Arg).
at_the_same_time_sql(Arg) ->
    at_the_same_time(Arg).

bucket_basic(doc) ->
    ["should create, view, and delete a bucket"];
bucket_basic(suite) ->
    [];
bucket_basic(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
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
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},

    BucketsBefore = bucket_list(S3Conf),
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

% NOTES FOR CODE REVIEW:
% 1) from what i've seen, erlcloud url-encodes things before sending them off.
% 2) if so, then *if* this test is sending a url-encoded bucket name, we would then
%    eventually be url-encoding something that is already url-encoded.
% 3) if this test *instead* wants to test whether an 'odd' but non url-encoded name works,
%    then the '%' character in the bucket name violates s3 object and bucket naming guidelines:
%    https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html
%    https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-s3-bucket-naming-requirements.html
%    https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html
% for now, changing this test until more clarification is forthcoming.
bucket_encoding(doc) ->
    ["should be able to create buckets with URL encoding"];
bucket_encoding(suite) ->
    [];
bucket_encoding(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},

    OddBucket = "a bucket",
    OddBucketEnc = OddBucket,
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
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
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
    [ ?assertMatch({value, _}, Item) || Item <- Got ],
    %% verify 404 behavior
    try
        mini_s3:get_object_metadata(Bucket, "no-such-object", [], S3Conf)
    catch
        error:Why ->
            Why
    end,
    ?assertEqual(ok, mini_s3:delete_bucket(Bucket, S3Conf)).


put_object(doc) ->
    ["should be able to put and list objects"];
put_object(suite) ->
    [];
put_object(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    Bucket = random_bucket(),
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

has_header(_Header, []) ->
    false;
has_header(Header, [{Name, _Value}|T]) ->
    Needle = string:to_lower(Header),
    case string:to_lower(Name) of
        Needle ->
            true;
        _Other ->
            has_header(Header, T)
    end.

cache_control(doc) ->
    ["Send a Cache-Control header "];
cache_control(suite) ->
    [];
cache_control(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    Bucket = random_bucket(),
    ensure_bucket(Bucket, S3Conf),

    NameExists = random_path(),
    NameMissing = random_path(),

    Data = "TestData\nMore test data",
    mini_s3:put_object(Bucket, NameExists, Data, [], [], S3Conf),

    SignedUrl = mini_s3:s3_url('get', Bucket, NameMissing, 1000, [], S3Conf),
    {ok, Result} = httpc:request(erlang:binary_to_list(SignedUrl)),
    {{_, 404, _}, HeadersMissing, _} = Result,

    SignedUrl2 = mini_s3:s3_url('get', Bucket, NameExists, 1000, [], S3Conf),
    {ok, Result2} = httpc:request(erlang:binary_to_list(SignedUrl2)),
    {{_, 200, _}, HeadersExists, _} = Result2,

    ?assertMatch(false, has_header("Cache-Control", HeadersMissing)),
    ?assertMatch(true, has_header("Cache-Control", HeadersExists)).

object_roundtrip(doc) ->
    ["Can put a object and get it back"];
object_roundtrip(suite) ->
    [];
object_roundtrip(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    Bucket = random_bucket(),
    ensure_bucket(Bucket,S3Conf),
    Name = random_path(),
    Data = erlang:iolist_to_binary(test_data_text(128*1024)),
    mini_s3:put_object(Bucket, Name, Data, [], [], S3Conf),
    Result = mini_s3:get_object(Bucket, Name, [], S3Conf),
    DataRoundtrip = proplists:get_value(content, Result),
    ?assertEqual(Data, DataRoundtrip).

object_delete(doc) ->
    ["Can create and delete an object"];
object_delete(suite) ->
    [];
object_delete(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},

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
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},

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
    BogusS3Conf = mini_s3:new(<<"nopenope">>, <<"evenmorenope">>, "http://127.0.0.1:4321", path, []),
    Bucket = "thisshouldfail",
    ?assertError({aws_error, {http_error, 403, _, _}},
                 mini_s3:create_bucket(Bucket, public_read_write, none, BogusS3Conf)),
    %% also verify that unsigned URL requests don't crash
    {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} = httpc:request("http://127.0.0.1:4321/foobar"),
    ?assertEqual(403, Status),
    ?assert(string:str(Body, "<Message>Access Denied</Message>") > 0).

signed_url(doc) ->
    ["Test that signed urls actually work"];
signed_url(suite) ->
    [];
signed_url(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    Bucket = random_binary(),
    ensure_bucket(Bucket, S3Conf),
    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(erlang:md5(Content)))}],
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
                                                             erlang:md5("Something Else")))}],
                                    "text/xml", Content}, [], []),
  ?assertMatch({ok,{{"HTTP/1.1",403,"Forbidden"}, _, _}},
               Response2).

signed_url_fail(doc) ->
    ["Test that signed url expiration actually works"];
signed_url_fail(suite) ->
    [];
signed_url_fail(Config) when is_list(Config) ->
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    Bucket = random_binary(),
    ensure_bucket(Bucket, S3Conf),

    Content = "<x>Super Foo</x>",
    Headers = [{"content-type", "text/xml"},
               {"content-md5",
                erlang:binary_to_list(base64:encode(erlang:md5(Content)))}],
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
    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
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

                   % 1) see bucket naming conventions
                   % 2) erlcloud url-encodes before sending
                   % https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html
                   % https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-s3-bucket-naming-requirements.html
                   % https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html
                   %{"bucket%20space", "xjbrpodcionabrzhikgliowdzvbvbc/kqvfgzhnlkizzvbidsxwavrktxcasx"}
                   {"bucket space", "xjbrpodcionabrzhikgliowdzvbvbc/kqvfgzhnlkizzvbidsxwavrktxcasx"}
                  ],

    S3Conf0 = proplists:get_value(s3_conf, Config),
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},

    AssertCount = fun(Bucket, Count) ->
                           Res = mini_s3:list_objects(Bucket, [], S3Conf),
                           Contents = proplists:get_value(contents, Res),
                           ?assertEqual(Count, length(Contents))
                   end,

    AssertCount("bucket-1", 2),
    AssertCount("bucket-2", 45),
    AssertCount("bucket-3", 1),
    AssertCount("bucket-4", 0),
    % NOTES for code review:
    % see bucket naming conventions
    % 2) erlcloud url-encodes before sending
    % https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html
    % https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-s3-bucket-naming-requirements.html
    % https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html
    %AssertCount("bucket%20space", 2),
    AssertCount("bucket space", 2),

    [ mini_s3:get_object(Bucket, Key, [], S3Conf) || {Bucket, Key} <- ShouldExist ],
    ok.

%%====================================================================
%% Utility Functions
%%====================================================================
random_binary() ->
    random_string(30, ?STR_CHARS).

random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
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

bucket_list(S3Conf0) ->
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    [{buckets, Details}] = mini_s3:list_buckets(S3Conf),
    lists:map(fun(Opts) -> proplists:get_value(name, Opts) end, Details).

bucket_exists(Name, S3Conf) ->
    BucketNames = bucket_list(S3Conf),
    lists:member(Name, BucketNames).

ensure_bucket(Bucket, Config) ->
    case bucket_exists(Bucket, Config) of
        true -> ?assertEqual(ok, mini_s3:delete_bucket(Bucket, Config));
        _    -> ok
    end,
    ?assertEqual(ok, mini_s3:create_bucket(Bucket, public_read_write, none, Config)).

file_exists(Bucket, Name, S3Conf0) ->
    S3Conf = S3Conf0#aws_config{access_key_id = ?accesskeyid, secret_access_key = ?secretaccesskey},
    List = mini_s3:list_objects(Bucket, [], S3Conf),
    Files = [ proplists:get_value(key, I) || I <- proplists:get_value(contents, List)],
    lists:member(Name, Files).



%%%%
%% Helpers
%%%%

% stolen from chef_test_db_helper
the_real_root_dir("") ->
    {error, not_found};
the_real_root_dir(Dir) ->
    AllFiles = maybe_find_files(Dir),
    case {lists:member("_build", AllFiles), lists:member("config", AllFiles)} of
        {true, true} ->
            Dir;
        _ ->
            Tokens = filename:split(Dir),
            NewDir = filename:join(lists:droplast(Tokens)),
            the_real_root_dir(NewDir)
    end.

maybe_find_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, AllFiles} ->
            AllFiles;
        _ ->
            []
    end.
