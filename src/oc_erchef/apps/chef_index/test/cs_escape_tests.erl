%% Copyright 2015 Chef Server, Inc. All Rights Reserved.
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

-module(cs_escape_tests).
-include_lib("eunit/include/eunit.hrl").

cs_escape_test_() ->
    [
     {"doesn't alter data with no special characters", ?_assertEqual(cs_escape:escape("FooBar"), "FooBar")},
     {"replaces a bunch of special characters", ?_assertEqual(cs_escape:escape("Foo-()*Bar"), "Foo–⟨⟩⦁Bar")},
     {"doesn't replace underscore", ?_assertEqual(cs_escape:escape("Foo_Bar"), "Foo_Bar")},
     {"doesn't replace period", ?_assertEqual(cs_escape:escape("Foo.Bar"), "Foo.Bar")},
     {"doesn't replace integers", ?_assertEqual(cs_escape:escape("Foo9Bar"), "Foo9Bar")},
     {"doesn't replace single quotes", ?_assertEqual(cs_escape:escape("Foo'Bar"), "Foo'Bar")},
     {"throws a badarg on binaries", ?_assertError(badarg, cs_escape:escape(<<"FooBar">>))}
    ].



cs_escape_term_test_() ->
    [{lists:flatten(io_lib:format("doesn't replace known solr special char '~c' via escape_term_safe", [Char])),
      ?_assertEqual([Char], cs_escape:escape_term_safe([Char]))} || Char <- "!\"&'" ].
