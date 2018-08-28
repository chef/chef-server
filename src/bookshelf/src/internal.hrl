%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
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
-define(BLOCK_SIZE, 64*1024). %% 256k seems to cause a substantial slowdown and timeouts.
-define(PGSQL_RETRY_INTERVAL, 5).

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

-record(db_file, {
          bucket_name :: binary(),
          bucket_id   :: integer(),
          name        :: binary(),
          file_id     :: integer(),
          created_at  :: any(), % refine type
          data_id     :: integer(),
          complete    :: boolean(),
          data_size   :: integer(),
          chunk_count :: integer(),
          hash_md5    :: binary(),
          hash_sha256 :: binary(),
          hash_sha512 :: binary()
         }).

-define(DB_FILE_TX_FM, [db_file, [bucket_name, bucket_id, name, file_id, created_at, data_id, complete, data_size, chunk_count, hash_md5, hash_sha256, hash_sha512]]).

-record(db_bucket, {
          bucket_name :: binary(),
          created_at  :: any(),
          bucket_id   :: integer()
         }).

-define(DB_BUCKET_TX_FM, [db_bucket, [name, created_at, id]]).

-record(file_transfer_state, {
          size = 0 :: integer(), % transferred size
          next_chunk = 0:: integer(),
          %% These hashes are computed on both upload and download to help insure file integrity
          hash_context_md5 :: any(), %
          hash_context_sha256 :: any(),
          hash_context_sha512 :: any() % refine
         }).

-record(context, {
                  auth_check_disabled = false :: boolean(),
                  %% AWS credentials
                  access_key_id :: binary(),
                  secret_access_key :: binary(),

                  stream_download :: any(),

                  % Do we retry sql when we get no_connections. ms to wait
                  sql_retry_delay :: pos_integer(),
                  sql_retry_count = 0 :: non_neg_integer(),

                  %% unique request ID from nginx header (or generated if not
                  %% found) set by opscoderl_wm:read_req_id.
                  reqid :: binary(),

                  %% The name of the HTTP request header containing the unique ID set by the load
                  %% balancer
                  reqid_header_name :: string(),
                  %% List of known buckets, only populated in places we want to avoid
                  %% multiple lookups
                  bucket_list :: list(),

                  entry_ref :: #entryref{}, % null in sql mode

                  entry_md :: #object{} | #db_file{},

                  transfer_state :: #file_transfer_state{}

              }).
