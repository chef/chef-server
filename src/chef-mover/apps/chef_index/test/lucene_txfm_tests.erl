%% Copyright 2012-2018 Chef Software, Inc.
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

-module(lucene_txfm_tests).

-include_lib("eunit/include/eunit.hrl").
-define(i2b(X), iolist_to_binary(X)).
-define(gv(X, PL), proplists:get_value(X, PL)).

read_example_queries(File) ->
    {ok, FH} = file:open(File, [read, binary]),
    Queries = read_queries(FH, []),
    file:close(FH),
    Queries.

read_queries(FH, Acc) ->
    case file:read_line(FH) of
        {ok, LF} when LF == <<"\n">> orelse LF == "\n" ->
            read_queries(FH, Acc);
        {ok, Line} ->
            read_queries(FH, [trimre(Line)|Acc]);
        eof ->
            make_pairs(lists:reverse(Acc), []);
        {error, Why} -> {error, Why}
    end.

make_pairs([A, B |Rest], Acc) ->
    make_pairs(Rest, [{A, B} | Acc]);
make_pairs([], Acc) ->
    Acc.

trimre(Bin) ->
    re:replace(Bin, "^\\s+|\\s+$", % FIX for emacs "
               "",
               [{return, binary}, global]).

query_example_test_() ->
    % run the example transform tests as we run in Ruby Chef
    Queries = read_example_queries(filename:join([".", "apps", "chef_index", "test", "search_queries.txt"])),
    [ ?_assertEqual(E, chef_lucene:parse(I)) || {I, E} <- Queries ].

lucene_query_test_() ->
    Tests = [{"aterm", <<"aterm">>},
             {"a1 b2 c3", <<"a1 b2 c3">>},
             {"afield:aterm", <<"content:afield__=__aterm">>}
            ],
    [ ?_assertEqual(Want, chef_lucene:parse(In)) || {In, Want} <- Tests ].

term_whitespace_test_() ->
    % we just preserve space for now, no need to be more particular
    RawTerms = [<<" leading">>, <<"trailing ">>],
    Terms = [<<" leading">>, <<"trailing ">>],
    Tests = lists:zip(RawTerms, Terms),
    [ ?_assertEqual(T, chef_lucene:parse(R)) || {R, T} <- Tests ].

term_keyword_test_() ->
    Keywords = [<<"AND">>, <<"OR">>, <<"NOT">>],
    Prefixed = [ <<"X", K/binary>> || K <- Keywords ],
    Suffixed = [ <<K/binary, "X">> || K <- Keywords ],
    Tests = Prefixed ++ Suffixed,
    [ ?_assertEqual(K, chef_lucene:parse(K)) || K <- Tests ].

term_special_chars_test_() ->
    SpecialChars = ["!", "(", ")", "{", "}", "[", "]",
                    "^", "\"", "~", "*", "?", ":", "\\"],
    Formats = ["foo~sbar", "~sb", "a~s", "a~sb"],
    Terms = [ ?i2b(io_lib:format(F, ["\\" ++ C])) ||
                F <- Formats, C <- SpecialChars ],
    [ ?_assertEqual(T, chef_lucene:parse(T)) || T <- Terms ].

field_range_test_() ->
    Queries = [{"afield:[start TO end]",
                <<"content:[afield__=__start TO afield__=__end]">>},
               {"afield:{start TO end}",
                <<"content:{afield__=__start TO afield__=__end}">>},

               {"afield:[start TO *]",
                <<"content:[afield__=__start TO afield__=__\\ufff0]">>},
               {"afield:{start TO *}",
                <<"content:{afield__=__start TO afield__=__\\ufff0}">>},

               {"afield:[* TO end]",
                <<"content:[afield__=__ TO afield__=__end]">>},
               {"afield:{* TO end}",
                <<"content:{afield__=__ TO afield__=__end}">>},

               % NOTE: for the * TO * case, we're just returning
               % everything and ignoring inclusive/exclusive
               {"afield:[* TO *]", <<"content:afield__=__*">>},
               {"afield:{* TO *}", <<"content:afield__=__*">>}

               ],
    [ ?_assertEqual(E, chef_lucene:parse(I)) || {I, E} <- Queries ].

groups_test_() ->
    Tests = [<<"(aterm)">>,
             <<"(a1 b1 c1)">>,
             <<"abc (x y z) jj">>],
    [ ?_assertEqual(I, chef_lucene:parse(I)) || I <- Tests ].

boolean_ops_on_single_terms_test_() ->
    Ops = ["AND", "&&", "OR", "||"],
    Tests = [ ?i2b(io_lib:format("foo ~s bar", [Op])) || Op <- Ops ],
    [ ?_assertEqual(I, chef_lucene:parse(I)) || I <- Tests ].

multiple_booleans_test() ->
    Query = <<"t1 AND t2 OR t3 AND t4">>,
    ?assertEqual(Query, chef_lucene:parse(Query)).

groups_and_booleans_test_() ->
    Tests = ["(a && b)", "(a && b) OR c",
             "c OR (a AND b)", "(a AND d) OR (a && b)"],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Tests ].

not_queries_test_() ->
    Tests = ["a NOT b" "a ! b", "a !b",
             "a NOT (b OR c)", "a !(b OR c)"],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Tests ].

required_and_prohibited_prefixes_test_() ->
    % FIXME: should we change the parsing to only accept prohibited
    % operator when there is an adjacent term?
    Prefixes = ["+", "-"],
    Formats = [{"~saterm", [{op, <<"aterm">>}]},
               {"first ~ssecond", [<<"first">>, {op, <<"second">>}]},
               {"~sfirst second", [{op, <<"first">>}, <<"second">>]}],
    Tests = [ ?i2b(io_lib:format(In, [Op])) || Op <- Prefixes, {In, _L} <- Formats ],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Tests ].

ignore_inner_special_chars_in_terms_test_() ->
    Specials = ["+", "-", "*", "?", "_", "."],
    Terms = [ ?i2b(io_lib:format("foo~sbar", [S])) || S <- Specials ],
    [ ?_assertEqual(T, chef_lucene:parse(T)) || T <- Terms].

phrase_query_test_() ->
    Phrases = ["\"single\"",
               "\"one two three\"",
               "\"has \\\"escaped\\\" quotes\\\"s\"",
               "+\"one two\"",
               "-\"one two\""],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Phrases ].

not_queries_on_phrases_test_() ->
    Queries = ["a NOT \"b c\"", "a !\"b c\"", "a ! \"b c\""],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Queries ].

field_labeled_queries_test_() ->
    Tests = [
             {"a_field:aterm", <<"content:a_field__=__aterm">>},
             % FIXME: should we bother to support this or explicitly
             % say it is unsupported?
             % {"afield:(a b c)", <<"content:(afield__=__a afield__=__b afield__=__c)">>},
             {"afield:\"a b\"", <<"content:\"afield__=__a b\"">>}
            ],
    [ ?_assertEqual(E, chef_lucene:parse(I)) || {I, E} <- Tests ].

term_boosting_test_() ->
    Tests = ["word^0.5", "\"one two\"^10"],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Tests ].

fuzzy_query_test_() ->
    Tests = ["word~", "word~0.5"],
    [ ?_assertEqual(?i2b(I), chef_lucene:parse(I)) || I <- Tests ].

fielded_fuzzy_query_test_() ->
    Tests = [{"afield:word~", <<"content:afield__=__word~">>},
             {"afield:word~0.5", <<"content:afield__=__word~0.5">>}],
    [ ?_assertEqual(Expect, chef_lucene:parse(I)) || {I, Expect} <- Tests ].

star_star_test() ->
    ?assertEqual(<<"*:*">>, chef_lucene:parse(<<"*:*">>)).

leading_star_test() ->
    ?assertEqual(<<"content:afield__=__*">>, chef_lucene:parse("afield:*")).

wildcard_query_test_() ->
    Tests = [
             {"afield:cr?p", <<"content:afield__=__cr?p">>},
             {"afield:c???", <<"content:afield__=__c???">>}
            ],
    [ ?_assertEqual(Expect, chef_lucene:parse(I)) || {I, Expect} <- Tests ].
