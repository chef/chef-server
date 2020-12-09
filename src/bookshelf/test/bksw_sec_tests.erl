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
add_port_to_host_test() ->

Blah1 = mini_s3:new("", "", "127.0.0.1"),
"127.0.0.1:443" = bksw_sec:add_port_to_host("127.0.0.1", Blah1),
Blah2 = mini_s3:new("", "", "127.0.0.1:4321"),
"127.0.0.1:4321" = bksw_sec:add_port_to_host("127.0.0.1:4321", Blah2),
Blah3 = mini_s3:new("", "", "http://127.0.0.1"),
"http://127.0.0.1:80" = bksw_sec:add_port_to_host("http://127.0.0.1", Blah3),
Blah4 = mini_s3:new("", "", "http://127.0.0.1:4321"),
"http://127.0.0.1:4321" = bksw_sec:add_port_to_host("http://127.0.0.1:4321", Blah4),

    Config0 = mini_s3:new("", "", "host"),
    "host:443" = bksw_sec:add_port_to_host("host", Config0),

    Config1 = mini_s3:new("", "", "host:123"),
    "host:123" = bksw_sec:add_port_to_host("host:123", Config1),

    Config2 = mini_s3:new("", "", "http://host"),
?debugFmt("~n~nbksw_sec:add_port_to_host('http://host') - ~p~n", [bksw_sec:add_port_to_host("http://host", Config2)]),
    "http://host:80" = bksw_sec:add_port_to_host("http://host", Config2),

    Config3 = mini_s3:new("", "", "http://host:123"),
    "http://host:123" = bksw_sec:add_port_to_host("http://host:123", Config3),

    Config4 = mini_s3:new("", "", "https://host"),
    "https://host:443" = bksw_sec:add_port_to_host("https://host", Config4),

    Config5 = mini_s3:new("", "", "https://host:123"),
    "https://host:123" = bksw_sec:add_port_to_host("https://host:123", Config5).

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
