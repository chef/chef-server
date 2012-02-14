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

-module(bookshelf_fs).
-include("bookshelf.hrl").
-export([
         bucket_list/1,
         bucket_exists/2,
         bucket_create/2,
         bucket_delete/2
        ]).

%% ===================================================================
%% Bucket functions
%% ===================================================================

bucket_list(Dir) ->
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

bucket_exists(Dir, Bucket) ->
    filelib:is_dir(filename:join(Dir, Bucket)).

bucket_create(Dir, Bucket) ->
    file:make_dir(filename:join(Dir, Bucket)).

bucket_delete(Dir, Bucket) ->
    file:del_dir(filename:join(Dir, Bucket)).
