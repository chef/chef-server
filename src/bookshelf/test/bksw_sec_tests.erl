-module(bksw_sec_tests).
-include_lib("eunit/include/eunit.hrl").

% 100 seconds into the future from now should not be expired.
is_expired_false_test() ->
    Time = erlcloud_aws:iso_8601_basic_time(),
    false = bksw_sec:is_expired(Time, 100).

% 10 seconds into the the past from now should be expired.
is_expired_true_test() ->
    Time = erlcloud_aws:iso_8601_basic_time(),
    true = bksw_sec:is_expired(Time, -10).

% get the key-value pairs associated with particular keys
get_signed_headers_test() ->
    [{a, 1}, {b, 2}, {c, 3}] = bksw_sec:get_signed_headers([a, b, c], [{a, 1}, {x, y}, {b, 2}, {x, y}, {c, 3}, {x, y}]).

% split-up credentials string into component parts
parse_x_amz_credential_test() ->
    Cred = "access-key-id/date/AWS-region/s3/aws4_request",
    ["access-key-id", "date", "AWS-region", "s3", "aws4_request"] = bksw_sec:parse_x_amz_credential(Cred).

% split-up signed header list into component parts
parse_x_amz_signed_headers_test() ->
    Headers = "header1;header2;header3",
    ["header1", "header2", "header3"] = bksw_sec:parse_x_amz_signed_headers(Headers).

% convert the keys of key-value pairs to all lowercase strings
process_headers_test() ->
    Headers = [{'ATOM-UPPERCASE', "untouched"}, {"STRING-UPPERCASE", "UNTOUCHED"}, {"string-untouched", "untouched"}],
    [{"atom-uppercase", "untouched"}, {"string-uppercase", "UNTOUCHED"}, {"string-untouched", "untouched"}]
        = bksw_sec:process_headers(Headers).

% split "string1/string2" or "/string1/string2" into {"string1", "string2"}
bucketname_key_from_path_test() ->
    Path1 = "/bucketname/key",
    Path2 =  "bucketname/key",
    Result = {"bucketname", "key"},
    Result = bksw_sec:bucketname_key_from_path(Path1),
    Result = bksw_sec:bucketname_key_from_path(Path2).
