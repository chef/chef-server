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

-include_lib("cowboy/include/http.hrl").
-include_lib("kernel/include/file.hrl").

%% dict
-define(is_dict(Any), (is_tuple(Any) andalso element(1, Any) == dict)).

%% request
-define(is_request(Any), (is_tuple(Any) andalso element(1, Any) == http_req)).

%% state
-record(state, {dir, model}).
-define(is_state(Any), (is_tuple(Any) andalso element(1, Any) == state)).

%% buckets
-record(bucket, {name, date}).
-define(is_bucket(Any), (is_tuple(Any) andalso element(1, Any) == bucket)).
