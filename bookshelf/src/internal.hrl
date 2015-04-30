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

%% logging utilities
-compile([{parse_transform, lager_transform}]).
%% For general info and error logging, we use error_logger and take advantage of lager's
%% error_logger handler. The main benefit is seeing the logs via sasl instead of lager for
%% common test. We _could_ take this approach to make lager a soft dependency, but once you
%% want to add debug-level logging you need direct lager calls.
-define(LOG_INFO(X, Y), error_logger:info_msg(X, Y)).
-define(LOG_INFO(X), error_logger:info_msg(X)).
-define(LOG_ERROR(X, Y), error_logger:error_msg(X, Y)).
-define(LOG_ERROR(X), error_logger:error_msg(X)).
-define(LOG_DEBUG(X, Y), lager:debug(X, Y)).
-define(LOG_DEBUG(X), lager:debug(X)).

-include("bksw_obj.hrl").

-record(entryref, {fd :: file:io_device(),
                   path :: string() | binary(),
                   bucket :: binary(),
                   entry :: binary(),
                   ctx :: undefined | binary()}).

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
                  reqid_header_name :: string(),

                  entry_ref :: #entryref{},
                  entry_md :: #object{}
              }).
