%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@chef.io>
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Chisamore <schisamo@chef.io>
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

-module(chef_s3_ops).

-include("chef_types.hrl").

-export([
         delete/2,
         fetch_md/2
        ]).

%% These don't need to be exported, strictly speaking, but could be helpful in live system
%% debugging scenarios.
-export([
         check_file/4,
         delete_file/4
        ]).

-type individual_op_return() ::  {'error' | 'missing' | 'ok', binary()}.

-type bulk_op_return() :: {{ok, [binary()]},
                           {missing, [binary()]},
                           {timeout, [binary()]},
                           {error, [binary()]}}.

%% @doc Delete each checksummed file in S3.
%%
%% Returns a tuple of tagged tuples indicating which checksums were successfully deleted and
%% which were not found, along with both the number of timeouts that occured, as well as the
%% number of other errors occurred.
%%
%% Note that the return value information is all disjoint; that is, the number of found and
%% missing checksums, plus the number of errors and timeouts, will be equal to the length of
%% `Checksums'.
-spec delete(OrgId :: object_id(),
             Checksums :: [binary()]) -> bulk_op_return().
delete(OrgId, Checksums) when is_list(Checksums),
                              is_binary(OrgId) ->
    s3_checksum_op(OrgId,
                   Checksums,
                   fun delete_file/4,
                   "Deletion of checksum: ~p for org ~p from bucket ~p has taken longer than ~p ms~n").

%% @doc Verify that each checksummed file is stored in S3 by checking its metadata.
%%
%% Returns a tuple of tagged tuples indicating which checksums were found and which were
%% not, along with both the number of errors (including timeouts, as distinct from
%% `delete/2') that occurred.
%%
%% Note that the return value information is all disjoint; that is, the number of found and
%% missing checksums, plus the number of errors, will be equal to the length of `Checksums'.
-spec fetch_md(OrgId :: object_id(),
              Checksums :: [binary()]) -> bulk_op_return().
fetch_md(OrgId, Checksums) when is_list(Checksums),
                                is_binary(OrgId) ->
    s3_checksum_op(OrgId,
                   Checksums,
                   fun check_file/4,
                   "Checking presence of checksum: ~p for org ~p from bucket ~p has taken longer than ~p ms~n").

%% HTTP Operation Functions
%%
%% These functions are responsible for actually issuing an HTTP request to S3 to either
%% delete a file or to verify its existence.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Delete an existing file (identified by `Checksum') in S3
-spec delete_file(OrgId :: object_id(),
                  AwsConfig :: mini_s3:config(),
                  Bucket :: string(),
                  Checksum :: binary()) -> individual_op_return().
delete_file(OrgId, AwsConfig, Bucket, Checksum) ->
    Key = chef_s3:make_key(OrgId, Checksum),

    try mini_s3:delete_object(Bucket, Key, AwsConfig) of
        %% Return value doesn't much matter here but for documentation's sake it looks like:
        %%
        %%  [{delete_marker, list_to_existing_atom(Marker)}, {version_id, Id}]
        %%
        %% The main take-away here is that the call to delete_object/3 was successful and
        %% didn't throw an error
        _Response ->
            {ok, Checksum}
    catch
        error:{aws_error, {http_error,404,_}} ->
            %% We got a 404.  Since this *may* be indicative of weirdness, we'll log it, but
            %% we don't need to crash or raise an exception.
            error_logger:warning_msg("Deletion of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) failed because the file was not found~n",
                                     [Checksum, OrgId, Bucket, Key]),
            {missing, Checksum};
        ExceptionClass:Reason->
            %% Something unanticipated happened.  We should log the specific reason for
            %% later analysis, but as far as the overall deletion operation is concerned,
            %% this is "just an error", and we can continue along.
            error_logger:error_msg("Deletion of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) raised exception ~p:~p~n",
                                   [Checksum, OrgId, Bucket, Key, ExceptionClass, Reason]),
            {error, Checksum}
    end.

%% @doc See if the file represented by the given `Checksum' exists in S3.
-spec check_file(OrgId :: object_id(),
                 AwsConfig :: mini_s3:config(),
                 Bucket :: string(),
                 Checksum :: binary()) -> individual_op_return().
check_file(OrgId, AwsConfig, Bucket, Checksum) ->
    check_file(OrgId, AwsConfig, Bucket, Checksum, 2).

check_file(_, _, _, Checksum, 0) ->
    {error, Checksum};
check_file(OrgId, AwsConfig, Bucket, Checksum, AttemptsLeft) ->
    Key = chef_s3:make_key(OrgId, Checksum),
    Result = try mini_s3:get_object_metadata(Bucket, Key, [], AwsConfig) of
                 _Response ->
                     %% Actual return value doesn't matter, just that the call didn't throw
                     %% an error.
                     {ok, Checksum}
             catch
                 error:{aws_error, {http_error,404,_}} ->
                     %% The file wasn't found. Log it and move on.
                     error_logger:error_msg("Checking presence of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) failed because the file was not found~n",
                                            [Checksum, OrgId, Bucket, Key]),
                     {missing, Checksum};
                 %% TODO(ssd) 2020-09-03: should likely remove this case when we move to erlcloud
                 error:{aws_error, {socket_error,retry_later}} ->
                     error_logger:error_msg("Checking presence of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) returned retry_later (retries left: ~p)~n",
                                            [Checksum, OrgId, Bucket, Key, AttemptsLeft - 1]),
                     check_file(OrgId, AwsConfig, Bucket, Checksum, AttemptsLeft - 1);
                 ExceptionClass:Reason->
                     %% Something unanticipated happened.  We should log the specific reason
                     %% for later analysis, but as far as the overall checking operation is
                     %% concerned, this is "just an error", and we can continue along.
                     error_logger:error_msg("Checking presence of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) raised exception ~p:~p~n",
                                            [Checksum, OrgId, Bucket, Key, ExceptionClass, Reason]),
                     {error, Checksum}
             end,
    Result.

%% @doc Utility function to handle the commonalities of deleting or verifying the presence
%% of files in S3.
%%
%% Basically, `Fun' is going to be either `delete_file/4' or `check_file/4'.
%% `TimeoutMsgTemplate' should be a template string for an `error_logger' message that takes
%% a Checksum, an OrgId, a Bucket name, and a Timeout value (in that order).
-spec s3_checksum_op(OrgId :: object_id(),
                     Checksums :: [binary()],
                     Fun :: fun((object_id(), mini_s3:config(), string(), binary()) -> individual_op_return()),
                     TimeoutMsgTemplate :: string()) ->
                            bulk_op_return().
s3_checksum_op(OrgId, Checksums, Fun, TimeoutMsgTemplate) ->
    Bucket = chef_s3:bucket(),
    AwsConfig = chef_s3:get_internal_config(),
    Timeout = chef_config:config_option(chef_objects, s3_parallel_ops_timeout, pos_integer),
    Fanout = chef_config:config_option(chef_objects, s3_parallel_ops_fanout, pos_integer),

    RequestFun = fun(Checksum) ->
                         Fun(OrgId, AwsConfig, Bucket, Checksum)
                 end,

    TimeoutHandler = fun(Checksum) ->
                             error_logger:error_msg(TimeoutMsgTemplate,
                                                    [Checksum, OrgId, Bucket, Timeout]),
                             {timeout, Checksum}
                     end,

    Results = chef_parallel:parallelize_all_with_timeout(Checksums, RequestFun, Fanout, Timeout, TimeoutHandler),
    %% Now we need to consolidate our results based on:
    %%
    %%   Did the operation succeed?
    %%   Was the checksum even found?
    %%   Did the operation timeout?
    %%   Did some other error occur during the operation?.
    %%
    {OkChecksums, MissingChecksums, Timeouts, Errors} =
        lists:foldl(fun(Result, {Ok, Missing, Timeouts, Errors}) ->
                            case Result of
                                {ok, Checksum} -> {[Checksum | Ok], Missing, Timeouts, Errors};
                                {missing, Checksum} -> {Ok, [Checksum | Missing], Timeouts, Errors};
                                {timeout, Checksum} -> {Ok, Missing, [Checksum |Timeouts], Errors};
                                {error, Checksum} -> {Ok, Missing, Timeouts, [Checksum | Errors]}
                            end
                    end,
                    {[], [], [], []},
                    Results),

    %% Sort the checksums and package everything up properly
    {{ok, lists:sort(OkChecksums)},
     {missing, lists:sort(MissingChecksums)},
     {timeout, lists:sort(Timeouts)},
     {error, lists:sort(Errors)}}.
