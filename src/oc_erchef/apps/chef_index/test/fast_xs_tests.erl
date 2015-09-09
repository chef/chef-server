%% -*- coding: utf-8 -*-
-module(fast_xs_tests).

-include_lib("eunit/include/eunit.hrl").

expand_test() ->
        Expect = <<"k1__=__ "
               "k1__=__0 k1__=__1.123 "
               "k1__=__2 k1__=__a k1__=__b k1__=__c "
               "k1__=__false k1__=__true ">>,
    ?assertEqual(Expect, fast_xs:escape(Expect)).

expand_2_test() ->
    Expect = <<"k1">>,
    ?assertEqual(Expect, fast_xs:escape(Expect)).

expand_3_test() ->
    Expect = <<"chef_db1">>,
    ?assertEqual(Expect, fast_xs:escape(Expect)).

ascii_test() ->
    ?assertEqual(<<"abc">>, fast_xs:escape(<<"abc">>)).

predefined_test() ->
    ?assertEqual(<<"&amp;">>, fast_xs:escape(<<"&">>)),              % ampersand
    ?assertEqual(<<"&lt;">>,  fast_xs:escape(<<"<">>)),              % left angle bracket
    ?assertEqual(<<"&gt;">>,  fast_xs:escape(<<">">>)),              % right angle bracket
    ?assertEqual(<<"&quot;">>, fast_xs:escape(<<"\"">>)).            % double quote

invalid_test() ->
    ok = io:setopts([{encoding, unicode}]),
    ?assertEqual(<<"*">>, fast_xs:escape(<<"\x00">>)),               % null
    ?assertEqual(<<"*">>, fast_xs:escape(<<"\x0C">>)),               % form feed
    ?assertEqual(<<"*">>, fast_xs:escape(<<65536>>)).

iso_8859_misc_test() ->
    ?assertEqual(<<"&#239;&#191;&#189;">>, fast_xs:escape(<<"�"/utf8>>)),
    ?assertEqual(<<"name__=__Microsoft&#194;&#174;">>, fast_xs:escape(<<"name__=__Microsoft®"/utf8>>)),
    ?assertEqual(<<"Server&#174;">>, fast_xs:escape(<<"Server\xAE">>)).          % registered

iso_8859_test() ->
    ?assertEqual(<<"&#231;">>, fast_xs:escape(<<"\xE7">>)),          % small c cedilla
    ?assertEqual(<<"&#169;">>, fast_xs:escape(<<"\xA9">>)).          % copyright symbol

win_1252_test() ->
    ?assertEqual(<<"&#8217;">>, fast_xs:escape(<<"\x92">>)),         % smart quote
    ?assertEqual(<<"&#8364;">>, fast_xs:escape(<<"\x80">>)).         % euro

utf8_test() ->
    ?assertEqual(<<"&#169;">>, fast_xs:escape(<<"\x{A9}">>)),     % copy
    ?assertEqual(<<"&#8217;">>, fast_xs:escape(<<146>>)).         % right single quote


