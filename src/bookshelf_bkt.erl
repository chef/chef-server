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
%% Cowboy functions
%% ===================================================================

init(_Transport, _Rq, _Opts) ->
    %% we need to add model to our opts
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Rq, Opts) ->
    {dir, Dir}     = lists:keyfind(dir, 1, Opts),
    {model, Model} = lists:keyfind(model, 1, Opts),
    {ok, ?req(with_amz_request_id, Rq), #state{dir = Dir, model = Model}}.

%% ===================================================================
%% REST callbacks
%% ===================================================================

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, from_xml}], Rq, St}.

resource_exists(Rq, #state{dir=Dir}=St) ->
    {filelib:is_dir(Dir), Rq, St}.

to_xml(Rq, #state{dir=Dir, model=Model}=St) ->
    {bookshelf_xml:list_buckets_xml(buckets(Dir), Model), Rq, St}.

delete_resource(Rq, St) ->
    {true, Rq, St}.

%% ===================================================================
%% Internal functions
%% ===================================================================

buckets(Dir) ->
    {ok, Files} = file:list_dir(Dir), %% crash if no access to base dir
    lists:map(fun(P) -> %% crash if no access to any bucket dir
                      {ok, #file_info{ctime=Date}} =
                          file:read_file_info(P, [{time, universal}]),
                      #bucket{ name=filename:basename(P),
                               date=iso8601:format(Date) }
              end,
              lists:filter(fun filelib:is_dir/1,
                           lists:map(fun(F) ->
                                             filename:join(Dir, F)
                                     end,
                                     Files))).
