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

-module(bookshelf_bkt_tests).
-include("bookshelf.hrl").

rest_init_test_() ->
    [{"should populate the state with the base dir from handler opts",
      fun() ->
              Dir = "/tmp",
              ?assertMatch({ok, _, #state{dir=Dir}},
                           bookshelf_bkt:rest_init(#http_req{},
                                                   [{dir, Dir}]))
      end
     }].

allowed_methods_test_() ->
    [{"should only support 'PUT' and 'DELETE'",
      fun() ->
              Expected = ['PUT', 'DELETE'],
              {Allowed, _, _} =
                  bookshelf_bkt:allowed_methods(#http_req{}, #state{}),
              ?assertEqual(2, length(Allowed)),
              Result = sets:from_list(lists:merge(Expected, Allowed)),
              ?assertEqual(2, sets:size(Result))
      end
     }].

content_types_accepted_test_() ->
    [{"should only support PUT with 'undefined' (absent) Content-Type",
      fun() ->
              {Types, _, _} =
                  bookshelf_bkt:content_types_accepted(#http_req{}, #state{}),
              ?assertEqual(1, length(Types)),
              ?assert(lists:keymember(undefined, 1, Types))
      end
     }].

content_types_provided_test_() ->
    [{"should only support text/xml outputs",
      fun() ->
              {Types, _, _} =
                  bookshelf_bkt:content_types_provided(#http_req{}, #state{}),
              ?assertEqual(1, length(Types)),
              ?assert(lists:keymember({<<"text">>, <<"xml">>, []}, 1, Types))
      end
     }].

resource_exists_test_() ->
    [{"should only return true if the bucket exists",
      fun() ->
              {Dir, Bucket} = test_bucket(),
              ?assertMatch(
                 {true, _, _},
                 bookshelf_bkt:resource_exists(
                   #http_req{host=[Bucket]},
                   #state{dir=Dir}
                  )
                ),
              ?assertMatch(
                 {false, _, _},
                 bookshelf_bkt:resource_exists(
                   #http_req{host=[<<"batman!">>]},
                   #state{dir=Dir}
                  )
                )
      end
     }].

delete_resource_test_() ->
    [{"should delete the bucket and if the bucket exists",
      fun() ->
              {Dir, Bucket} = test_bucket(),
              {true, _, _} =
                  bookshelf_bkt:delete_resource(#http_req{host=[Bucket]},
                                                #state{dir=Dir}),
              {false, _, _} =
                  bookshelf_bkt:delete_resource(#http_req{host=[<<"derp">>]},
                                                #state{dir=Dir})
      end
     }].

create_resource_test_() ->
    [{"should create the bucket only if the bucket doesn't exist",
      fun() ->
              {Dir, Bucket} = test_bucket(),
              {false, _, _} =
                  bookshelf_bkt:create_resource(#http_req{host=[Bucket]},
                                                #state{dir=Dir}),
              {true, _, _} =
                  bookshelf_bkt:create_resource(#http_req{host=[<<"hurp">>]},
                                                #state{dir=Dir})
      end
     }].

test_bucket() ->
    {Ma, Se, Mi} = erlang:now(),
    Dir = filename:join("/tmp", io_lib:format("~p~p~p", [Ma,Se,Mi])),
    file:make_dir(Dir),
    Bucket = io_lib:format("~p~p~p", [Ma,Se,Mi]),
    ?assertEqual(ok, bookshelf_fs:bucket_create(Dir, Bucket)),
    {Dir, Bucket}.
