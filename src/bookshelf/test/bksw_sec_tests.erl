-module(bksw_sec_tests).
-include_lib("eunit/include/eunit.hrl").

% due to a security vulnerability described by Mark Anderson, we should compare signatures
% in constant time, and not 'early out' on the first mismatched character.  this means we
% are purposefully using a 'deoptimized' string compare function.
const_time_compare_test() ->
    % atom arguments return false
    false = bksw_sec:const_time_compare(a,      "test", true ),
    false = bksw_sec:const_time_compare(a,      "test", false),
    false = bksw_sec:const_time_compare("test", a,      true ),
    false = bksw_sec:const_time_compare("test", a,      false),

    % ensure that binaries are properly dealt with
    true  = bksw_sec:const_time_compare(<<"test">>, "test", true ),
    true  = bksw_sec:const_time_compare("test", <<"test">>, true ),
    true  = bksw_sec:const_time_compare(<<"test">>, <<"test">>, true ),

    % basic sanity checks, edge cases, etc.
    true  = bksw_sec:const_time_compare([],    [],    true ),
    false = bksw_sec:const_time_compare([],    [],    false),

    false = bksw_sec:const_time_compare([],    [x],   true ),
    false = bksw_sec:const_time_compare([],    [x],   false),
    false = bksw_sec:const_time_compare([x],   [],    true ),
    false = bksw_sec:const_time_compare([x],   [],    false),

    true  = bksw_sec:const_time_compare([x],   [x],   true ),
    false = bksw_sec:const_time_compare([x],   [x],   false),

    false = bksw_sec:const_time_compare([x],   [y],   true ),
    false = bksw_sec:const_time_compare([x],   [y],   false),

    true  = bksw_sec:const_time_compare([x,x], [x,x], true ),
    false = bksw_sec:const_time_compare([x,x], [x,x], false),
    false = bksw_sec:const_time_compare([x,x], [x,y], true ),
    false = bksw_sec:const_time_compare([x,x], [x,y], false),
    false = bksw_sec:const_time_compare([x,y], [x,x], true ),
    false = bksw_sec:const_time_compare([x,y], [x,x], false),
    true  = bksw_sec:const_time_compare([x,y], [x,y], true ),
    false = bksw_sec:const_time_compare([x,y], [x,y], false),
    false = bksw_sec:const_time_compare([x,y], [z,a], true ),
    false = bksw_sec:const_time_compare([x,y], [z,a], false),

    % same length equal strings
    true   = bksw_sec:const_time_compare("this is a test", "this is a test", true  ),
    false  = bksw_sec:const_time_compare("this is a test", "this is a test", false ),

    % same length unequal strings
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu",  "garbage blah blah abc123jcyhfoiahfx", true ),
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu",  "garbage blah blah abc123jcyhfoiahfx", false),

    % different length unequal strings
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu",  "garbage blah blah abc123jcyhfoiahfx1", true),
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu",  "garbage blah blah abc123jcyhfoiahfx1", false),
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu1", "garbage blah blah abc123jcyhfoiahfx",  true ),
    false  = bksw_sec:const_time_compare("garbage blah blah sihiufiusadfgfihu1", "garbage blah blah abc123jcyhfoiahfx",  false),
    false  = bksw_sec:const_time_compare("garbage", "garbage blah blah abc123jcyhfoiahfx", true),
    false  = bksw_sec:const_time_compare("garbage", "garbage blah blah abc123jcyhfoiahfx", false),
    false  = bksw_sec:const_time_compare("garbage blah blah abc123jcyhfoiahfx", "garbage", true),
    false  = bksw_sec:const_time_compare("garbage blah blah abc123jcyhfoiahfx", "garbage", false).

% split "<bucketname>/<key>" (possibly leading-trailing /) into {"bucketname", "key"}
get_bucket_key_test() ->
    {"",                      ""} = bksw_sec:get_bucket_key(""                      ),
    {"",                      ""} = bksw_sec:get_bucket_key("/"                     ),
    {"bucket",                ""} = bksw_sec:get_bucket_key("/bucket"               ),
    {"bucket",                ""} = bksw_sec:get_bucket_key("bucket/"               ),
    {"bucket",                ""} = bksw_sec:get_bucket_key("/bucket/"              ),
    {"bucket",             "key"} = bksw_sec:get_bucket_key("bucket/key"            ),
    {"bucket",             "key"} = bksw_sec:get_bucket_key("/bucket/key"           ),
    {"bucket",             "key"} = bksw_sec:get_bucket_key("bucket/key/"           ),
    {"bucket",             "key"} = bksw_sec:get_bucket_key("/bucket/key/"          ),
    {"bucket",        "key/more"} = bksw_sec:get_bucket_key("bucket/key/more"       ),
    {"bucket",        "key/more"} = bksw_sec:get_bucket_key("/bucket/key/more"      ),
    {"bucket",        "key/more"} = bksw_sec:get_bucket_key("bucket/key/more/"      ),
    {"bucket",        "key/more"} = bksw_sec:get_bucket_key("/bucket/key/more/"     ),
    {"bucket",   "key/more/moar"} = bksw_sec:get_bucket_key("bucket/key/more/moar"  ),
    {"bucket",   "key/more/moar"} = bksw_sec:get_bucket_key("/bucket/key/more/moar" ),
    {"bucket",   "key/more/moar"} = bksw_sec:get_bucket_key("bucket/key/more/moar/" ),
    {"bucket",   "key/more/moar"} = bksw_sec:get_bucket_key("/bucket/key/more/moar/").

% get host and toggle the port (add port or remove it)
get_host_toggleport_test() ->
    Config0 = mini_s3:new("", "", "host"),
    "host:443" = bksw_sec:get_host_toggleport("host", Config0),
    Config1 = mini_s3:new("", "", "host:123"),
    "host" = bksw_sec:get_host_toggleport("host:123", Config1),
    Config2 = mini_s3:new("", "", "http://host"),
    "http://host:80" = bksw_sec:get_host_toggleport("http://host", Config2),
    Config3 = mini_s3:new("", "", "http://host:123"),
    "http://host" = bksw_sec:get_host_toggleport("http://host:123", Config3),
    Config4 = mini_s3:new("", "", "https://host:123"),
    "https://host" = bksw_sec:get_host_toggleport("https://host:123", Config4).

% 100 seconds into the future from now should not be expired.
is_expired_false_test() ->
    Time = erlcloud_aws:iso_8601_basic_time(),
    false = bksw_sec:is_expired(Time, 100).

% 10 seconds into the the past from now should be expired.
is_expired_true_test() ->
    Time = erlcloud_aws:iso_8601_basic_time(),
    true = bksw_sec:is_expired(Time, -10).

% split authorization header into component parts
% https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
parse_authorization_test() ->
    Good = "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-date, Signature=fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024",
    {ok, ["AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request", "host;range;x-amz-date", "fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024"]} = bksw_sec:parse_authorization(Good),
    Bad = "blah Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-date, Signature=fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024",
    {error, parse_authorization} = bksw_sec:parse_authorization(Bad).
