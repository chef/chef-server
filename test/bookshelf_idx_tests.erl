%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <timd@opscode.com>
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

-module(bookshelf_idx_tests).
-include("bookshelf.hrl").

rest_init_test_() ->
    [{"should populate the state with the base dir from handler opts",
      fun() ->
              Dir = "/tmp",
              ?assertMatch({ok, _, #state{dir=Dir}},
                           bookshelf_idx:rest_init(#http_req{},
                                                   [{dir, Dir}]))
      end
     }].

allowed_methods_test_() ->
    [{"should only support 'GET'",
      fun() ->
              Expected = ['GET'],
              {Allowed, _, _} =
                  bookshelf_idx:allowed_methods(#http_req{}, #state{}),
              ?assertEqual(length(Expected), length(Allowed)),
              Result = sets:from_list(lists:merge(Expected, Allowed)),
              ?assertEqual(length(Expected), sets:size(Result))
      end
     }].

content_types_provided_test_() ->
    [{"should only support text/xml output",
      fun() ->
              {Types, _, _} =
                  bookshelf_idx:content_types_provided(#http_req{}, #state{}),
              ?assertEqual(1, length(Types)),
              ?assert(lists:keymember({<<"text">>, <<"xml">>, []}, 1, Types))
      end
     }].

resource_exists_test_() ->
    [{"should only return true if our service is setup correctly",
      fun() ->
              ?assertMatch(
                 {true, _, _},
                 bookshelf_idx:resource_exists(
                   #http_req{}, #state{dir="/tmp"}
                  )
                ),
              ?assertMatch(
                 {false, _, _},
                 bookshelf_idx:resource_exists(
                   #http_req{}, #state{dir="/tmp/6100156912837015691273"}
                  )
                )
      end
     }].
