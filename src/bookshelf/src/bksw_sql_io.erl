%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2015-16 Chef, Inc. All Rights Reserved.
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

-module(bksw_sql_io).

-export([bucket_list/0,
         bucket_exists/1,
         bucket_delete/1,
         bucket_create/1]).

-export([entry_list/1,
         entry_delete/2,
         entry_exists/2]).

-include("internal.hrl").

-spec bucket_list() -> [#bucket{}] | [].
bucket_list() ->
    ?LOG_DEBUG("reading bucket list"),
    bksw_sql:list_buckets().

-spec bucket_exists(binary()) -> boolean().
bucket_exists(Bucket) ->
    case bksw_sql:find_bucket(Bucket) of
        {ok, none} ->
            false;
        {ok, _} ->
            true
end.

-spec bucket_create(binary()) -> boolean().
bucket_create(Bucket) ->
    case bksw_sql:find_bucket(Bucket) of
        {ok, none} ->
            case bksw_sql:create_bucket(Bucket) of
                {ok, 1} ->
                    true;
                _Error ->
                    false
            end;
        {ok, _} ->
            true
    end.

-spec bucket_delete(binary()) -> boolean().
bucket_delete(Bucket) ->
    case bksw_sql:delete_bucket(Bucket) of
        ok ->
            true;
        _ ->
            false
    end.

-spec entry_list(binary()) -> [#object{}] | [].
entry_list(Bucket) ->
    ?LOG_DEBUG("reading entries for bucket '~p'", [Bucket]),
    bksw_sql:list_bucket(Bucket).

-spec entry_delete(binary(), binary()) -> boolean().
entry_delete(Bucket, Entry) ->
    case bksw_sql:delete_file(Bucket, Entry) of
        ok ->
            true;
        _ ->
            false
    end.

%% Stub function; unsure if we really need it.
entry_exists(undefined, undefined) ->
    ok.
