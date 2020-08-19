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

% https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
% host header is required
% x-amz-content-sha256 header is required
% if content-type header is present in request, it is required
% any x-amz-* headers present in request are required
check_signed_headers_test() ->
    % no host
    false = bksw_sec:check_signed_headers_authhead([], []),
    % no x-amz-content-sha256
    false = bksw_sec:check_signed_headers_authhead([{"host", x}], []),
    % no content-type
    false = bksw_sec:check_signed_headers_authhead([{"host", x}, {"x-amz-content-sha256", x}], [{"content-type", x}]),
    % no x-amz-*
    false = bksw_sec:check_signed_headers_authhead([{"host", x}, {"x-amz-content-sha256", x}, {"content-type", x}], [{"content-type", x}, {"x-amz-blah", x}]),

    true =  bksw_sec:check_signed_headers_authhead([{"host", x}, {"x-amz-content-sha256", x}, {"content-type", x}, {"x-amz-blah", x}], [{"content-type", x}, {"x-amz-blah", x}]),
    true =  bksw_sec:check_signed_headers_authhead([{"host", x}, {"x-amz-content-sha256", x}], []).

%https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
get_check_date_test() ->
    ISO8601Date             = "20151014T235959Z",
    CredentialScopeDate     = "20151014",
    DateIfUndefined         = ISO8601Date,
    Impossible              = impossible,
    {ok, ISO8601Date}       = bksw_sec:get_check_date(ISO8601Date, Impossible,         CredentialScopeDate),
    {ok, ISO8601Date}       = bksw_sec:get_check_date(undefined,   DateIfUndefined,    CredentialScopeDate),
    {error, get_check_date} = bksw_sec:get_check_date(undefined,   undefined,          CredentialScopeDate).

% get key-value pairs (headers) associated with specified keys.
% for each key, get first occurance of key-value. for duplicated
% keys, get corresponding key-value pairs. results are undefined
% for nonexistent key(s).
get_signed_headers_test() ->
    [{a, 1}, {b, 2}, {b, 3}, {c, 3}] =
        bksw_sec:get_signed_headers([a, b, b, c], [{a, 1}, {x, y}, {b, 2}, {x, y}, {b, 3}, {x, y}, {c, 3}, {x, y}, {a, 2}, {b, 4}, {c, 4}], []).

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

% split credentials string into component parts
parse_x_amz_credential_test() ->
    Good = "access-key-id/date/AWS-region/s3/aws4_request",
    {ok, ["access-key-id", "date", "AWS-region", "s3", "aws4_request"]} = bksw_sec:parse_x_amz_credential(Good),
    Bad = "access-key-id/date/AWS-region/1/2",
    {error, _} = bksw_sec:parse_x_amz_credential(Bad).

% split signed header list into component parts
parse_x_amz_signed_headers_test() ->
    Headers = "header1;header2;header3",
    ["header1", "header2", "header3"] = bksw_sec:parse_x_amz_signed_headers(Headers).

% convert the keys of key-value pairs to all lowercase strings
process_headers_test() ->
    Headers = [{'ATOM-UPPERCASE', "untouched"}, {"STRING-UPPERCASE", "UNTOUCHED"}, {"string-untouched", "untouched"}],
    [{"atom-uppercase", "untouched"}, {"string-uppercase", "UNTOUCHED"}, {"string-untouched", "untouched"}]
        = bksw_sec:process_headers(Headers).
