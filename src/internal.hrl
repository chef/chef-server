%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


%% settings
-define(BOOKSHELF_CONFIG, bookshelf).
-define(TIMEOUT_MS, 4096).
-define(BLOCK_SIZE, 16384).

-record(context, {
                  %% AWS credentials
                  access_key_id :: binary(),
                  secret_access_key :: binary(),

                  stream_download :: any(),

                  %% unique request ID from nginx header (or generated if not
                  %% found) set by opscoderl_wm:read_req_id.
                  reqid :: binary(),

                  %% The name of the HTTP request header containing the unique ID set by the load
                  %% balancer
                  reqid_header_name :: string()
              }).
