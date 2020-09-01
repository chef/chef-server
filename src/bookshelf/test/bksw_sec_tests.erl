-module(bksw_sec_tests).
-include_lib("eunit/include/eunit.hrl").

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
