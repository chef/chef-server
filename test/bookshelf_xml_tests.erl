%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@opscode.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.

-module(bookshelf_xml_tests).
-include("bookshelf.hrl").

list_buckets_test_() ->
    [{"should render ListAllMyBucketsResult",
      fun() ->
              Date = calendar:now_to_datetime(erlang:now()),
              IsoDate = bookshelf_format:to_date(Date),
              Buckets = [#bucket{name="lol", date=Date},
                         #bucket{name="cat", date=Date}],
              ?assertMatch(
                 {'ListAllMyBucketsResult',undefined,
                  {'CanonicalUser',undefined,"abc123","Bobo T. Clown"},
                  {'ListAllMyBucketsList',undefined,
                   [{'ListAllMyBucketsEntry',undefined,"lol", IsoDate},
                    {'ListAllMyBucketsEntry',undefined,"cat", IsoDate}]}},
                 bookshelf_xml:list_buckets(Buckets)
                )
      end
     }].
