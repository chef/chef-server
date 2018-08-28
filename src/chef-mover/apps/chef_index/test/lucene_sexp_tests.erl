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

-module(lucene_sexp_tests).

-include_lib("eunit/include/eunit.hrl").
-define(i2b(X), iolist_to_binary(X)).
-define(gv(X, PL), proplists:get_value(X, PL)).

%% @ should be a legal character
at_sign_test() ->
    ?assertEqual([<<"foo@bar.com">>],
                 lucene:parse("foo@bar.com")).

lucene_query_test_() ->
    Tests = [{"aterm", [<<"aterm">>]},
             {"a1 b2 c3", [<<"a1">>, <<"b2">>, <<"c3">>]},
             {"afield:aterm", [[{field,<<"afield">>}, {term,<<"aterm">>}]]}
            ],
    [ ?_assertEqual(Want, lucene:parse(In)) || {In, Want} <- Tests ].

term_whitespace_test_() ->
    RawTerms = [<<" leading">>, <<"trailing ">>],
    Terms = [<<"leading">>, <<"trailing">>],
    Tests = lists:zip(RawTerms, Terms),
    [ ?_assertEqual(T, hd(lucene:parse(R))) || {R, T} <- Tests ].

term_keyword_test_() ->
    Keywords = [<<"AND">>, <<"OR">>, <<"NOT">>],
    Prefixed = [ <<"X", K/binary>> || K <- Keywords ],
    Suffixed = [ <<K/binary, "X">> || K <- Keywords ],
    Tests = Prefixed ++ Suffixed,
    [ ?_assertEqual(K, hd(lucene:parse(K))) || K <- Tests ].

term_special_chars_test_() ->
    SpecialChars = ["!", "(", ")", "{", "}", "[", "]",
                    "^", "\"", "~", "*", "?", ":", "\\"],
    Formats = ["foo~sbar", "~sb", "a~s", "a~sb"],
    Terms = [ ?i2b(io_lib:format(F, ["\\" ++ C])) ||
                F <- Formats, C <- SpecialChars ],
    [ ?_assertEqual(T, hd(lucene:parse(T))) || T <- Terms ].

field_range_test_() ->
    Kinds = [{{incl, left}, "["},
             {{incl, right}, "]"},
             {{excl, left}, "{"},
             {{excl, right}, "}"}],
    ExpectFun = fun(Kind, Field, S, E) ->
                        [{range, Kind},
                         {field, ?i2b(Field)},
                         {start, ?i2b(S)},
                         {'end', ?i2b(E)}]
                end,
    QueryFun = fun(Kind, Field, S, E) ->
                       Left = ?gv({Kind, left}, Kinds),
                       Right = ?gv({Kind, right}, Kinds),
                       ?i2b(io_lib:format("~s:~s~s TO ~s~s",
                                          [Field, Left, S, E, Right]))
               end,
    Queries = [{"afield", "start", "end"},
               {"afield", "start", "*"},
               {"afield", "*", "end"},
               {"afield", "*", "*"}],
    [ ?_assertEqual(ExpectFun(Kind, Field, S, E),
                    hd(lucene:parse(QueryFun(Kind, Field, S, E))))
      || Kind <- [incl, excl], {Field, S, E} <- Queries ].

groups_test_() ->
    Tests = [{<<"(aterm)">>, [{group, [<<"aterm">>]}]},
             {<<"(a1 b1 c1)">>, [{group, [<<"a1">>, <<"b1">>, <<"c1">>]}]},
             {<<"abc (x y z) jj">>,
              [<<"abc">>, {group, [<<"x">>, <<"y">>, <<"z">>]}, <<"jj">>]}],
    [ ?_assertEqual(Expect, lucene:parse(I)) || {I, Expect} <- Tests ].

boolean_ops_on_single_terms_test_() ->
    Ops = ["AND", "&&", "OR", "||"],
    Tests = [{?i2b(io_lib:format("foo ~s bar", [Op])),
              {list_to_atom(Op), <<"foo">>, [<<"bar">>]}} || Op <- Ops ],
    [ ?_assertEqual(E, hd(lucene:parse(I))) || {I, E} <- Tests ].

multiple_booleans_test() ->
    Query = "t1 AND t2 OR t3 AND t4",
    Expect = [{'AND',<<"t1">>,
               [{'OR',<<"t2">>, [{'AND',<<"t3">>, [<<"t4">>]}]}]}],
    ?assertEqual(Expect, lucene:parse(Query)).

groups_and_booleans_test_() ->
    Tests = [
             {"(a && b)", [{group, [{'&&', <<"a">>, [<<"b">>]}]}]},
             {"(a && b) OR c", [{'OR', {group, [{'&&', <<"a">>, [<<"b">>]}]}, [<<"c">>]}]},
             {"c OR (a AND b)", [{'OR', <<"c">>,
                                  [{group, [{'AND', <<"a">>, [<<"b">>]}]}]}]},
             {"(a AND d) OR (a && b)",
              [{'OR',
                {group, [{'AND', <<"a">>, [<<"d">>]}]},
                [{group, [{'&&', <<"a">>, [<<"b">>]}]}]}]}
            ],
    [ ?_assertEqual(E, lucene:parse(I)) || {I, E} <- Tests ].

not_queries_test_() ->
    Tests = [
             {"a NOT b", [<<"a">>, {'NOT', <<"b">>}]},
             {"a ! b", [<<"a">>, {'!', <<"b">>}]},
             {"a !b", [<<"a">>, {'!', <<"b">>}]},
             {"a NOT (b OR c)", [<<"a">>,
                                 {'NOT', {group,
                                          [{'OR', <<"b">>, [<<"c">>]}]}}]},
             {"a !(b OR c)", [<<"a">>,
                              {'!', {group, [{'OR', <<"b">>, [<<"c">>]}]}}]}
            ],
    [ ?_assertEqual(E, lucene:parse(I)) || {I, E} <- Tests ].

required_and_prohibited_prefixes_test_() ->
    % FIXME: should we change the parsing to only accept prohibited
    % operator when there is an adjacent term?
    Prefixes = ["+", "-"],
    Formats = [{"~saterm", [{op, <<"aterm">>}]},
               {"first ~ssecond", [<<"first">>, {op, <<"second">>}]},
               {"~sfirst second", [{op, <<"first">>}, <<"second">>]}],
    InsertOp = fun(L, Op) when is_list(L) ->
                       lists:keyreplace(op, 1, L, {Op,?gv(op, L)})
               end,
    Tests = [ {?i2b(io_lib:format(In, [Op])), InsertOp(L, ?i2b(Op))}
              || Op <- Prefixes,
                 {In, L} <- Formats ],
    [ ?_assertEqual(E, lucene:parse(I)) || {I, E} <- Tests ].

ignore_inner_special_chars_in_terms_test_() ->
    Specials = ["+", "-", "*", "?", "_", "."],
    Terms = [ ?i2b(io_lib:format("foo~sbar", [S])) || S <- Specials ],
    [ ?_assertEqual([T], lucene:parse(T)) || T <- Terms].

phrase_query_test_() ->
    Phrases = [{"\"single\"", {phrase, <<"single">>}},
               {"\"one two three\"", {phrase, <<"one two three">>}},
               {"\"has \\\"escaped\\\" quotes\\\"s\"",
                {phrase, <<"has \\\"escaped\\\" quotes\\\"s">>}},
               % required phrase
               {"+\"one two\"", {<<"+">>, {phrase, <<"one two">>}}},
               % prohibited phrase
               {"-\"one two\"", {<<"-">>, {phrase, <<"one two">>}}}
               ],
    [ ?_assertEqual(E, hd(lucene:parse(I))) || {I, E} <- Phrases ].

not_queries_on_phrases_test_() ->
    Queries = [
               {"a NOT \"b c\"", [<<"a">>, {'NOT', {phrase, <<"b c">>}}]},
               {"a !\"b c\"", [<<"a">>, {'!', {phrase, <<"b c">>}}]},
               {"a ! \"b c\"", [<<"a">>, {'!', {phrase, <<"b c">>}}]}],
    [ ?_assertEqual(E, lucene:parse(I)) || {I, E} <- Queries ].

field_labeled_queries_test_() ->
    Tests = [
             {"a_field:aterm", [[{field, <<"a_field">>}, {term, <<"aterm">>}]]},
             {"afield:(a b c)",
              [[{field, <<"afield">>},
                {term, {group, [<<"a">>, <<"b">>, <<"c">>]}}]]},
             {"afield:\"a b\"", [[{field, <<"afield">>}, {term, {phrase, <<"a b">>}}]]}
            ],
    [ ?_assertEqual(E, lucene:parse(I)) || {I, E} <- Tests ].

term_boosting_test_() ->
    Tests = [{"word^0.5", {boost, <<"word">>, 0.5}},
             {"\"one two\"^10", {boost, {phrase, <<"one two">>}, 10}}
            ],
    [ ?_assertEqual(E, hd(lucene:parse(I))) || {I, E} <- Tests ].

fuzzy_query_test_() ->
    Tests = [{"word~", {fuzzy, <<"word">>}},
             {"word~0.5", {fuzzy, <<"word">>, 0.5}}],
    [ ?_assertEqual(E, hd(lucene:parse(I))) || {I, E} <- Tests ].

fielded_fuzzy_query_test_() ->
    Tests = [{"afield:word~", [{field, <<"afield">>}, {term, {fuzzy, <<"word">>}}]},
             {"afield:word~0.5", [{field, <<"afield">>}, {term, {fuzzy, <<"word">>, 0.5}}]}],
    [ ?_assertEqual(E, hd(lucene:parse(I))) || {I, E} <- Tests ].

%% TODO: support for proximity queries?
%%     {"\"one two\"~10", {proximity, {phrase, <<"one two">>}, 10}}],
%% as well as then a fielded version

% TODO
% - fields and boolean, unary ops
% - range queries and boolean, unary ops
% - failure modes
%   - invalid queries
%   - queries with invalid/lone NOT/!
%
% - new transform module for solr schema
%   - implement test to use same set of input
%
