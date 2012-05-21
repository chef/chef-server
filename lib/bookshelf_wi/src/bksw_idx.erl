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
-module(bksw_idx).

-export([init/3, rest_init/2, allowed_methods/2, content_types_provided/2,
         resource_exists/2, to_xml/2]).

-include("bookshelf.hrl").

-include_lib("cowboy/include/http.hrl").


%%===================================================================
%% Public API
%%===================================================================

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Rq, Opts) ->
    {dir, Dir} = lists:keyfind(dir, 1, Opts),
    {ok, bookshelf_req:with_amz_request_id(Rq),
     #req_state{dir = Dir}}.

allowed_methods(Rq, St) -> {['GET'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

resource_exists(Rq, #req_state{dir = Dir} = St) ->
    {filelib:is_dir(Dir), Rq, St}.

to_xml(Rq, #req_state{dir = Dir} = St) ->
    Buckets = (?BACKEND):bucket_list(Dir),
    Term = bookshelf_xml:list_buckets(Buckets),
    Body = bookshelf_xml:write(Term),
    {Body, Rq, St}.

%%===================================================================
%% Eunit Tests
%%===================================================================
-ifndef(NO_TESTS).

-include_lib("eunit/include/eunit.hrl").

rest_init_test_() ->
    [{"should populate the state with the base "
      "dir from handler opts",
      fun () ->
              Dir = "/tmp",
              ?assertMatch({ok, _, #req_state{dir = Dir}},
                           (rest_init(#http_req{}, [{dir, Dir}])))
      end}].

allowed_methods_test_() ->
    [{"should only support 'GET'",
      fun () ->
              Expected = ['GET'],
              {Allowed, _, _} = allowed_methods(#http_req{},
                                                #req_state{}),
              ?assertEqual((length(Expected)), (length(Allowed))),
              Result = sets:from_list(lists:merge(Expected, Allowed)),
              ?assertEqual((length(Expected)), (sets:size(Result)))
      end}].

content_types_provided_test_() ->
    [{"should only support text/xml output",
      fun () ->
              {Types, _, _} = content_types_provided(#http_req{},
                                                     #req_state{}),
              ?assertEqual(1, (length(Types))),
              ?assert((lists:keymember({<<"text">>, <<"xml">>, []}, 1,
                                       Types)))
      end}].

resource_exists_test_() ->
    [{"should only return true if our service "
      "is setup correctly",
      fun () ->
              ?assertMatch({true, _, _},
                           (resource_exists(#http_req{},
                                            #req_state{dir = "/tmp"}))),
              ?assertMatch({false, _, _},
                           (resource_exists(#http_req{},
                                            #req_state{dir =
                                                           "/tmp/6100156912837015691273"})))
      end}].

-endif.
