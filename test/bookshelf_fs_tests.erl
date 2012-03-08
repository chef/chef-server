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

-module(bookshelf_fs_tests).
-include("bookshelf.hrl").

%% ===================================================================
%%                         Bucket functions
%% ===================================================================

bookshelf_fs_test_() ->
    [{"should be able to create, list & delete buckets",
      fun() ->
              {Ma, Se, Mi} = erlang:now(),
              Bucket = list_to_binary(io_lib:format("~p~p~p", [Ma,Se,Mi])),
              Dir = filename:join("/tmp", Bucket),
              file:make_dir(Dir),
              Buckets = ["lol", "cat", "walrus", "bukkit"],
              lists:foreach(
                fun(B) ->
                        ?assertEqual(ok, bookshelf_fs:bucket_create(Dir, B)),
                        ?assert(bookshelf_fs:bucket_exists(Dir, B)),
                        ?assertMatch({error, _},
                                     bookshelf_fs:bucket_create(Dir, B))
                end,
                Buckets),
              ?assertEqual(ok, bookshelf_fs:bucket_delete(Dir, "cat")),
              Pass2 = bookshelf_fs:bucket_list(Dir),
              ?assertEqual(3, length(Pass2)),
              ?assertNot(bookshelf_fs:bucket_exists(Dir, "cat"))
      end
     }].

bookshelf_fs_object_test_() ->
    [{"should be able to list objects",
      fun() ->
              {Ma, Se, Mi} = erlang:now(),
              Dir = filename:join("/tmp", io_lib:format("~p~p~p",
                                                        [Ma,Se,Mi])),
              Bucket = "bukkit",
              BucketPath = filename:join(Dir, Bucket),
              ?assertEqual(ok, filelib:ensure_dir(BucketPath)),
              ?assertEqual(ok, bookshelf_fs:bucket_create(Dir, Bucket)),
              ?assertEqual([], bookshelf_fs:obj_list(Dir, Bucket)),
              Objs = ["testing/123/hello", "hello"],
              lists:foreach(
                fun(F) ->
                        ?assertEqual(ok, fixture_file(BucketPath, F, F))
                end,
                Objs
               ),
              Records = bookshelf_fs:obj_list(Dir, Bucket),
              ?assertEqual(2, length(Records))
      end
     }].

fixture_file(BucketPath, ObjectPath, Contents) ->
    FilePath = filename:join(BucketPath, ObjectPath),
    ?assertEqual(ok, filelib:ensure_dir(FilePath)),
    case file:open(FilePath, [write]) of
        {ok, IODevice} ->
            ?assertEqual(ok, file:write(IODevice, Contents)),
            ?assertEqual(ok, file:close(IODevice));
        E -> E
    end.
