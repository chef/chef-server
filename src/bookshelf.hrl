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

-include_lib("kernel/include/file.hrl").
-include_lib("cowboy/include/http.hrl").

%% amazon s3 model for erlsom
-include("amazon_s3.hrl").

%% records
-record(state, {dir}).
-record(bucket, {name, date}).
-record(object, {name, date, size, digest}).

%% settings
-define(BACKEND, bookshelf_fs).
-define(TIMEOUT_MS, 4096).
-define(BLOCK_SIZE, 16384).

%% shortcut macro for a path to a file in our priv/ dir
-define(file(F), filename:join(code:priv_dir(bookshelf), F)).

%% shortcut macro to apply 1 argument to the env module/function
-define(env(F, A), apply(bookshelf_env, F, [A])).
