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

-module(bookshelf_bkt).
-include("bookshelf.hrl").
-include_lib("cowboy/include/http.hrl").

-include("amazon_s3.hrl").


-compile(export_all).

%% ===================================================================
%%                              Cowboy
%% ===================================================================

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

%% ===================================================================
%%                         Cowboy HTTP REST
%% ===================================================================

rest_init(Rq, Opts) ->
    {dir, Dir} = lists:keyfind(dir, 1, Opts),
    {ok, bookshelf_req:with_amz_request_id(Rq), #state{dir = Dir}}.

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{undefined, create_resource}], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

resource_exists(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    {?BACKEND:bucket_exists(Dir, Bucket), Rq, St}.

delete_resource(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    case ?BACKEND:bucket_delete(Dir, Bucket) of
        ok -> {true, Rq, St};
        _  -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Accepted
%% ===================================================================

create_resource(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    case ?BACKEND:bucket_create(Dir, Bucket) of
        ok -> {true, Rq, St};
        _  -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Provided
%% ===================================================================

to_xml(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    Objects = ?BACKEND:obj_list(Dir, Bucket),
    Term    = bookshelf_xml:list_objects(Bucket, Objects),
    Body    = bookshelf_xml:write(Term),
    {Body, Rq, St}.

%% ===================================================================
%%                         Eunit Tests
%% ===================================================================
-ifndef(NO_TESTS).
-include_lib("eunit/include/eunit.hrl").

rest_init_test_() ->
    [{"should populate the state with the base dir from handler opts",
      fun() ->
              Dir = "/tmp",
              ?assertMatch({ok, _, #state{dir=Dir}},
                           rest_init(#http_req{},
                                                   [{dir, Dir}]))
      end
     }].

allowed_methods_test_() ->
    [{"should only support 'PUT' and 'DELETE'",
      fun() ->
              Expected = ['GET', 'PUT', 'DELETE'],
              {Allowed, _, _} =
                  allowed_methods(#http_req{}, #state{}),
              ?assertEqual(length(Expected), length(Allowed)),
              Result = sets:from_list(lists:merge(Expected, Allowed)),
              ?assertEqual(length(Expected), sets:size(Result))
      end
     }].

content_types_accepted_test_() ->
    [{"should only support PUT with 'undefined' (absent) Content-Type",
      fun() ->
              {Types, _, _} =
                  content_types_accepted(#http_req{}, #state{}),
              ?assertEqual(1, length(Types)),
              ?assert(lists:keymember(undefined, 1, Types))
      end
     }].

content_types_provided_test_() ->
    [{"should only support text/xml output",
      fun() ->
              {Types, _, _} =
                  content_types_provided(#http_req{}, #state{}),
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
                 resource_exists(
                   #http_req{host=[Bucket]},
                   #state{dir=Dir}
                  )
                ),
              ?assertMatch(
                 {false, _, _},
                 resource_exists(
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
                  delete_resource(#http_req{host=[Bucket]},
                                                #state{dir=Dir}),
              {false, _, _} =
                  delete_resource(#http_req{host=[<<"derp">>]},
                                                #state{dir=Dir})
      end
     }].

create_resource_test_() ->
    [{"should create the bucket only if the bucket doesn't exist",
      fun() ->
              {Dir, Bucket} = test_bucket(),
              {false, _, _} =
                  create_resource(#http_req{host=[Bucket]},
                                                #state{dir=Dir}),
              {true, _, _} =
                  create_resource(#http_req{host=[<<"hurp">>]},
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

-endif.
