-module(bksw_sec_tests).
-include_lib("eunit/include/eunit.hrl").

% split "string1/string2" or "/string1/string2" into {"string1", "string2"}
bucketname_key_from_path_test() ->
    Path1 = "/bucketname/key",
    Path2 =  "bucketname/key",
    Result = {"bucketname", "key"},
    Result = bksw_sec:bucketname_key_from_path(Path1),
    Result = bksw_sec:bucketname_key_from_path(Path2).

%https://docs.aws.amazon.com/general/latest/gr/sigv4-date-handling.html
get_check_date_test() ->
    ISO8601Date =         "20151014T235959Z",
    CredentialScopeDate = "20151014",
    DateIfUndefined =      fun() -> ISO8601Date end,
    Impossible =           fun() -> impossible  end,
    ISO8601Date =          bksw_sec:get_check_date(ISO8601Date, Impossible,      CredentialScopeDate),
    ISO8601Date =          bksw_sec:get_check_date(undefined,   DateIfUndefined, CredentialScopeDate).

% get key-value pairs associated with specified keys
get_signed_headers_test() ->
    [{a, 1}, {b, 2}, {c, 3}] = bksw_sec:get_signed_headers([a, b, c], [{a, 1}, {x, y}, {b, 2}, {x, y}, {c, 3}, {x, y}]).

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
    Auth = "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request, SignedHeaders=host;range;x-amz-date, Signature=fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024",
    ["AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request", "host;range;x-amz-date", "fe5f80f77d5fa3beca038a248ff027d0445342fe2855ddc963176630326f1024"] = bksw_sec:parse_authorization(Auth).

% split credentials string into component parts
parse_x_amz_credential_test() ->
    Cred = "access-key-id/date/AWS-region/s3/aws4_request",
    ["access-key-id", "date", "AWS-region", "s3", "aws4_request"] = bksw_sec:parse_x_amz_credential(Cred).

% split signed header list into component parts
parse_x_amz_signed_headers_test() ->
    Headers = "header1;header2;header3",
    ["header1", "header2", "header3"] = bksw_sec:parse_x_amz_signed_headers(Headers).

% convert the keys of key-value pairs to all lowercase strings
process_headers_test() ->
    Headers = [{'ATOM-UPPERCASE', "untouched"}, {"STRING-UPPERCASE", "UNTOUCHED"}, {"string-untouched", "untouched"}],
    [{"atom-uppercase", "untouched"}, {"string-uppercase", "UNTOUCHED"}, {"string-untouched", "untouched"}]
        = bksw_sec:process_headers(Headers).
