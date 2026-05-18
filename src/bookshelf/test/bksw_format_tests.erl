-module(bksw_format_tests).
-include_lib("eunit/include/eunit.hrl").

%% to_hex/1 — binary input produces a lowercase hex string of the correct length and content
to_hex_produces_lowercase_hex_string_test() ->
    ?assertEqual("0a1b2c3d", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>)).

%% to_hex/1 — empty binary produces empty string
to_hex_empty_binary_produces_empty_string_test() ->
    ?assertEqual("", bksw_format:to_hex(<<>>)).

%% to_hex/1 — non-binary input is rejected at the boundary
to_hex_rejects_string_input_test() ->
    ?assertError(function_clause, bksw_format:to_hex("not a binary")).

%% to_hex/1 — non-binary integer input is rejected at the boundary
to_hex_rejects_integer_input_test() ->
    ?assertError(function_clause, bksw_format:to_hex(42)).

%% to_base64/1 — round-trips through base64 correctly
to_base64_encodes_binary_test() ->
    ?assertEqual("dGVzdA==", bksw_format:to_base64(<<"test">>)).

%% to_base64/1 — non-binary input is rejected at the boundary
to_base64_rejects_string_input_test() ->
    ?assertError(function_clause, bksw_format:to_base64("not a binary")).

%% to_etag/1 — binary input is hex-encoded then wrapped in double quotes
to_etag_wraps_hex_in_quotes_test() ->
    ?assertEqual("\"0a1b2c3d\"",
                 lists:flatten(bksw_format:to_etag(<<16#0a, 16#1b, 16#2c, 16#3d>>))).

%% to_date/1 — undefined input returns the epoch sentinel string
to_date_undefined_returns_epoch_test() ->
    ?assertEqual(<<"1970-01-01T00:00:00.000Z">>, bksw_format:to_date(undefined)).

%% Toggle ON: hex_encoding_case=uppercase produces uppercase hex digits
to_hex_uppercase_toggle_produces_uppercase_test() ->
    application:set_env(bookshelf, hex_encoding_case, uppercase),
    try
        ?assertEqual("0A1B2C3D", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>))
    after
        application:unset_env(bookshelf, hex_encoding_case)
    end.

%% Toggle OFF: default (no env set) preserves lowercase behavior
to_hex_default_is_lowercase_when_toggle_unset_test() ->
    application:unset_env(bookshelf, hex_encoding_case),
    ?assertEqual("0a1b2c3d", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>)).

%% ── Exercise 15: Resilience & failure tests ───────────────────────────────

%% Resilience: unrecognised toggle value falls back to lowercase without crashing.
%% The wildcard clause (_ -> Hex) is the fail-safe; this test proves it fires.
to_hex_unknown_toggle_value_falls_back_to_lowercase_test() ->
    application:set_env(bookshelf, hex_encoding_case, not_a_valid_case),
    try
        ?assertEqual("0a1b2c3d", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>))
    after
        application:unset_env(bookshelf, hex_encoding_case)
    end.

%% Edge case: to_etag/1 on an empty binary produces a valid (empty) quoted ETag.
to_etag_empty_binary_produces_empty_quoted_string_test() ->
    ?assertEqual("\"\"", lists:flatten(bksw_format:to_etag(<<>>))).

%% Boundary: to_hex/1 on a 1 KiB binary does not crash and returns the correct length.
to_hex_large_binary_does_not_crash_test() ->
    LargeBin = binary:copy(<<16#ab>>, 1024),
    Result = bksw_format:to_hex(LargeBin),
    ?assertEqual(2048, length(Result)),
    ?assertEqual("ab", lists:sublist(Result, 2)).

%% Failure path: the {datetime, Date} tagged form of to_date/1 was previously untested.
%% Verifies the unwrap clause delegates to iso8601 and returns a binary ISO 8601 string.
to_date_datetime_tag_unwraps_and_formats_test() ->
    DateTime = {{2024, 1, 15}, {10, 30, 0}},
    Result = bksw_format:to_date({datetime, DateTime}),
    ?assertMatch(<<_/binary>>, Result),
    ?assertNotEqual(nomatch, binary:match(Result, <<"2024-01-15">>)).
