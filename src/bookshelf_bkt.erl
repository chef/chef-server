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

-module(bookshelf_bkt).
-include("bookshelf.hrl").
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
    {ok, ?req(with_amz_request_id, Rq), #state{dir = Dir}}.

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{undefined, create_resource}], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

resource_exists(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    {bookshelf_fs:bucket_exists(Dir, Bucket), Rq, St}.

delete_resource(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    case bookshelf_fs:bucket_delete(Dir, Bucket) of
        ok -> {true, Rq, St};
        _  -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Accepted
%% ===================================================================

create_resource(#http_req{host=[Bucket|_]}=Rq, #state{dir=Dir}=St) ->
    case bookshelf_fs:bucket_create(Dir, Bucket) of
        ok -> {true, Rq, St};
        _  -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Provided
%% ===================================================================
