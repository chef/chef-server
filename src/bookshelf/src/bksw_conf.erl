%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
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

-module(bksw_conf).

%% API
-export([get_wm_configuration/0,
         get_context/1,
         access_key_id/1,
         disk_store/0,
         storage_type/0,
         reset_dispatch/0,
         secret_access_key/1,
         stream_download/0,
         summarize_config/0]).

%% Exported for common test
-ifdef(TEST).
-export([port/0, ip/0, keys/0]).
-endif.

-include("internal.hrl").

%%%===================================================================
%%% types
%%%===================================================================
-type context() :: #context{}.


%%%===================================================================
%%% API
%%%===================================================================
-spec get_context(proplists:proplist()) -> context().
get_context(Config) ->
    #context{auth_check_disabled = proplists:get_value(auth_check_disabled, Config),
             access_key_id = proplists:get_value(access_key_id, Config),
             secret_access_key = proplists:get_value(secret_access_key, Config),
             stream_download = proplists:get_value(stream_download, Config),
             reqid_header_name = proplists:get_value(reqid_header_name, Config),
             sql_retry_delay = proplists:get_value(sql_retry_delay, Config),
             sql_retry_count = proplists:get_value(sql_retry_count, Config)
            }.

-spec summarize_config() -> proplists:proplist().
summarize_config() ->
    {KeyId, _Secret} = keys(),
    [{ip, ip()},
     {port, port()},
     {log_dir, log_dir()},
     {storage_type, storage_type()},
     {disk_store, disk_store()},
     {stream_download, stream_download()},
     {auth_check_disabled, auth_check_disabled()},
     {reqid_header_name, reqid_header_name()},
     {access_key_id, KeyId},
     {sql_retry_delay, sql_retry_delay()},
     {sql_retry_count, sql_retry_count()}].

-spec get_wm_configuration() -> list().
get_wm_configuration() ->
    [{ip, ip()},
     {port, port()},
     {dispatch, dispatch()},
     {log_dir, log_dir()}].

-spec access_key_id(context()) -> binary().
access_key_id(#context{access_key_id=AccessKeyId}) ->
    AccessKeyId.

-spec secret_access_key(context()) -> binary().
secret_access_key(#context{secret_access_key=SecretAccessKey}) ->
    SecretAccessKey.

-spec disk_store() -> string().
-ifdef(EUNIT_TEST).
disk_store() ->
    "/tmp/".
-else.
disk_store() ->
    case envy:get(bookshelf, disk_store, undefined, any) of
        undefined ->
            error({missing_config, {bookshelf, disk_store}});
        Path when is_list(Path) ->
            case ends_with($/, Path) of
                true ->
                    Path;
                false ->
                    Path ++ "/"
            end
    end.

ends_with(Char, String) ->
    lists:last(String) =:= Char.
-endif.

reset_dispatch() ->
    Dispatch = dispatch(),
    error_logger:info_msg("resetting webmachine dispatch_list: ~p~n", [Dispatch]),
    application:set_env(webmachine, dispatch_list, Dispatch).

%%%===================================================================
%%% Internal functions
%%%===================================================================
dispatch() ->
    {AccessKeyId, SecretAccessKey} = keys(),
    Config = [{stream_download, stream_download()},
              {auth_check_disabled, auth_check_disabled()},
              {access_key_id, AccessKeyId},
              {secret_access_key, SecretAccessKey},
              {sql_retry_count, sql_retry_count()},
              {sql_retry_delay, sql_retry_delay()}],
    %% per wm docs, init args for resources should be a list
    dispatch_by_storage(storage_type(), Config).

dispatch_by_storage(filesystem, Config) ->
    [{[bucket, obj_part, '*'], bksw_wm_object, Config},
     {[bucket], bksw_wm_bucket, Config},
     {[], bksw_wm_index, Config}];
dispatch_by_storage(sql, Config) ->
    [{[bucket, obj_part, '*'], bksw_wm_sql_object, Config},
     {[bucket], bksw_wm_sql_bucket, Config},
     {[], bksw_wm_sql_index, Config}].

ip() ->
    envy:get(bookshelf, ip, "127.0.0.1", string).

port() ->
    envy:get(bookshelf, port, 4321, positive_integer).

keys() ->
    {ok, AWSAccessKey} = chef_secrets:get(<<"bookshelf">>, <<"access_key_id">>),
    {ok, SecretKey} = chef_secrets:get(<<"bookshelf">>, <<"secret_access_key">>),
    {AWSAccessKey, SecretKey}.

log_dir() ->
    envy:get(bookshelf, log_dir, code:priv_dir(bookshelf), any).

auth_check_disabled() ->
    envy:get(bookshelf, auth_check_disabled, false, boolean).

storage_type() ->
    envy:get(bookshelf, storage_type, filesystem, atom).

reqid_header_name() ->
    envy:get(bookshelf, reqid_header_name, undefined, string).

stream_download() ->
    envy:get(bookshelf, stream_download, false, boolean).

sql_retry_delay() ->
    envy:get(bookshelf, sql_retry_delay, ?PGSQL_RETRY_INTERVAL, integer).

sql_retry_count() ->
    envy:get(bookshelf, sql_retry_count, 0, integer).
