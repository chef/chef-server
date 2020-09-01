%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
%% @author Ho-Sheng Hsiao <hosh@chef.io>
%% @doc chef_s3 - Manage S3 activities for erchef
%%
%% Copyright 2012-2013 Opscode, Inc. All Rights Reserved.
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
% is this needed?
%-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([
         check_checksums/2,
         delete_checksums/2,
         generate_presigned_url/5,
         generate_presigned_urls/5,
         make_key/2,
         bucket/0,
         get_internal_config/0,
         get_external_config/1
        ]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-type http_verb() :: put | get.

%% @doc Given a OrgGuid and a list of checksums, returns a list of ones that are present,
%% missing and errors
-spec check_checksums(OrgId :: object_id(),
                      Checksums :: [binary()]) ->
                             {{ok, [binary()]},
                              {missing, [binary()]},
                              {timeout, [binary()]},
                              {error, [binary()]}}.
check_checksums(OrgId, Checksums) ->
    chef_s3_ops:fetch_md(OrgId, Checksums).

%% @doc Given a OrgGuid and a list of checksums, delete the checksums, returns a list of
%% the ones deleted, missing, timeout and errors
-spec delete_checksums(OrgId :: object_id(),
                       Checksums :: [binary()]) ->
                              {{ok, [binary()]},
                               {missing, [binary()]},
                               {timeout, [binary()]},
                               {error, [binary()]}}.
delete_checksums(OrgId, Checksums) ->
    chef_s3_ops:delete(OrgId, Checksums).

%% @doc Given an OrgGuid, an expire time, and a list of checksums, returns a list of
%% {Checksum, Url} tuples
-spec generate_presigned_urls(OrgId :: object_id(),
                              Lifetime :: integer(),
                              Method :: http_verb(),
                              Checksums :: [binary()],
                              ExternalUrl :: string()) -> Urls :: [{binary(), binary()}].
generate_presigned_urls(OrgId, Lifetime, Method, Checksums, ExternalUrl) ->
    Bucket = bucket(),
    AwsConfig = get_external_config(ExternalUrl),
    Urls = [{Checksum, generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig)}
            || Checksum <- Checksums],
    Urls.

-spec generate_presigned_url(OrgId :: object_id(),
                              Lifetime :: integer(),
                              Method :: http_verb(),
                              Checksum :: binary(),
                              ExternalUrl :: string()) -> Url :: binary().
generate_presigned_url(OrgId, Lifetime, Method, Checksum, ExternalUrl) ->
    Bucket = bucket(),
    AwsConfig = get_external_config(ExternalUrl),
    generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig).

generate_presigned_url(OrgId, Bucket, Lifetime, Method, Checksum, AwsConfig) ->
    Headers = headers_for_type(Method, Checksum),
    Expiry = case application:get_env(chef_objects, s3_url_expiry_window_size) of
        {ok, {X, percent}} ->
            Interval = round((X / 100) * Lifetime),
            {Lifetime, Interval};
        {ok, {X, minutes}} ->
            %% Convert X to seconds
            {Lifetime, (X * 60)};
        _ ->
            Lifetime
    end,
    mini_s3:s3_url(Method,
                   as_string(Bucket),
                   make_key(OrgId, Checksum),
                   Expiry,
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
    lists:append(["organization-", as_string(OrgId), "/checksum-", as_string(Checksum)]).

%% @doc Base64-encode an MD5 hex string.
-spec base64_checksum(Checksum::binary()) -> binary().
base64_checksum(Checksum) ->
    {ok, [BigNum], []} = io_lib:fread("~16u", as_string(Checksum)),
    Bin = <<BigNum:128>>,
    base64:encode(Bin).

%% @doc Utility function for fetching the S3 bucket name from config.
-spec bucket() -> string().
bucket() ->
    envy:get(chef_objects, s3_platform_bucket_name, string).

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

%% @doc returns the S3 credentials for erchef to talk to bookshelf or S3
get_internal_config() ->
    aws_config(s3_internal_url()).

%% @doc returns the S3 credentials for generating a presigned url
%% to send back to the requestor. This url will be used by the
%% requestor to  contact bookshelf or S3 directly, and as such,
%% the URL needs to be publicly accessible.
get_external_config(VHostUrl) ->
    aws_config(s3_external_url(VHostUrl)).

aws_config(S3Url) ->
    {ok, S3AccessKeyId} = chef_secrets:get(<<"bookshelf">>, <<"access_key_id">>),
    {ok, S3SecretKeyId} = chef_secrets:get(<<"bookshelf">>, <<"secret_access_key">>),
    SslOpts = envy:get(chef_objects, s3_ssl_opts, [], list),
    mini_s3:new(erlang:binary_to_list(S3AccessKeyId), erlang:binary_to_list(S3SecretKeyId), S3Url, path, SslOpts).

%% @doc returns a url for accessing s3 internally. This is used
%% to contact bookshelf or S3.
s3_internal_url() ->
    envy:get(chef_objects, s3_url, string).

%% @doc returns a url for generating a presigned url to send back
%% to the requestor. This url will be used by the requestor to
%% contact bookshelf or S3 directly, and as such, the URL needs
%% to be publicly accessible. If the url is configured with the
%% atom host_header, then use the passed-in vhost url parameter.
s3_external_url(VHostUrl) ->
    case envy:get(chef_objects, s3_external_url, [atom,string]) of
        host_header ->
            VHostUrl;
         "http" ++ _ = Url ->
            Url;
        BadUrl ->
            {invalid_s3_url, BadUrl}
    end.
