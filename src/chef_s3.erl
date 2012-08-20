%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
%% @doc chef_s3 - Manage S3 activities for erchef
%%
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

-module(chef_s3).

-include("chef_types.hrl").

-export([
         check_checksums/2,
         delete_checksums/2,
         generate_presigned_url/4,
         generate_presigned_urls/4,
         make_key/2,
         bucket/0,
         get_config/0
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-type http_verb() :: put | get.

%% @doc Given a OrgGuid and a list of checksums, returns a list of ones that are present,
%% missing and errors
-spec check_checksums(OrgId :: object_id(),
                      Checksums :: [binary()]) ->
                             {{ok, [binary()]},
                              {missing, [binary()]},
                              {error, non_neg_integer()}}.
check_checksums(OrgId, Checksums) ->
    chef_s3_ops:fetch_md(OrgId, Checksums).

%% @doc Given a OrgGuid and a list of checksums, delete the checksums, returns a list of
%% the ones deleted, missing, timeout and errors
-spec delete_checksums(OrgId :: object_id(),
                       Checksums :: [binary()]) ->
                              {{ok, [binary()]},
                               {missing, [binary()]},
                               {timeout, non_neg_integer()},
                               {error, non_neg_integer()}}.
delete_checksums(OrgId, Checksums) ->
    chef_s3_ops:delete(OrgId, Checksums).

%% @doc Given an OrgGuid, an expire time, and a list of checksums, returns a list of
%% {Checksum, Url} tuples
-spec generate_presigned_urls(OrgId :: object_id(),
                              Lifetime :: integer(),
                              Method :: http_verb(),
                              Checksums :: [binary()]) -> Urls :: [{binary(), binary()}].
generate_presigned_urls(OrgId, Lifetime, Method, Checksums) ->
    Bucket = bucket(),
    AwsConfig = get_config(),
    Urls = [{Checksum, generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig)}
            || Checksum <- Checksums],
    Urls.

generate_presigned_url(OrgId, Lifetime, Method, Checksum) ->
    Bucket = bucket(),
    AwsConfig = get_config(),
    generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig).

generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig) ->
    Headers = headers_for_type(Method, Checksum),
    mini_s3:s3_url(Method,
                       as_string(Bucket),
                       make_key(OrgId, Checksum),
                       Lifetime,
                       Headers,
                       AwsConfig).

%% @doc Utility function to normalize inputs to Erlang strings.  Needed to bridge our binary
%% string standard with mini_s3's string standard.
-spec as_string(bin_or_string()) -> string().
as_string(B) when is_binary(B) ->
    binary_to_list(B);
as_string(S) ->
    S.

%% @doc Create the key for the final resting place of a checksum.
-spec make_key(OrgId :: object_id(),
                     Checksum :: binary()) -> Key :: string().
make_key(OrgId, Checksum) ->
    lists:flatten(["organization-", as_string(OrgId), "/",
                   "checksum-", as_string(Checksum)]).

%% @doc Base64-encode an MD5 hex string.
-spec base64_checksum(Checksum::binary()) -> binary().
base64_checksum(Checksum) ->
    Bin = mochihex:to_bin(as_string(Checksum)),
    base64:encode(Bin).

%% @doc Utility function for fetching the S3 bucket name from config.
-spec bucket() -> string().
bucket() ->
    {ok, Bucket} = application:get_env(chef_objects, s3_platform_bucket_name),
    Bucket.

-spec headers_for_type(http_verb(), Checksum::binary()) -> [ {string(), string()} ].
%% @doc helper function for generating headers for the S3 URL
%%
headers_for_type(put, Checksum) ->
    Base64Sum = base64_checksum(Checksum),

    %% Knife adds the "content-type" header when uploading, so we need to make sure it's in
    %% the data-to-be-signed.
    [{"content-md5", Base64Sum},
     {"content-type", "application/x-binary"}];
headers_for_type(get, _Checksum) ->
    [].

get_config() ->
    {ok, S3AccessKeyId } =  application:get_env(chef_objects, s3_access_key_id),
    {ok, S3SecretKeyId } =  application:get_env(chef_objects, s3_secret_key_id),

    S3Url = case application:get_env(chef_objects, s3_url) of
        undefined ->
            throw({error, missing_s3_url});
        {ok, Url} ->
            Url
    end,

    mini_s3:new(S3AccessKeyId, S3SecretKeyId, S3Url, path).
