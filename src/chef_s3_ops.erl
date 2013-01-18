%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% @author Seth Chisamore <schisamo@opscode.com>
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

-type checksum_op_return_type() :: {{ok, [binary()]},
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
             Checksums :: [binary()]) -> checksum_op_return_type().
delete(OrgId, Checksums) when is_list(Checksums),
                              is_binary(OrgId) ->
    parallelize_checksum_op(OrgId, Checksums, delete_file).

%% @doc Verify that each checksummed file is stored in S3 by checking its metadata.
%%
%% Returns a tuple of tagged tuples indicating which checksums were found and which were
%% not, along with both the number of errors (including timeouts, as distinct from
%% `delete/2') that occurred.
%%
%% Note that the return value information is all disjoint; that is, the number of found and
%% missing checksums, plus the number of errors, will be equal to the length of `Checksums'.
-spec fetch_md(OrgId :: object_id(),
              Checksums :: [binary()]) -> checksum_op_return_type().
fetch_md(OrgId, Checksums) when is_list(Checksums),
                                is_binary(OrgId) ->
    parallelize_checksum_op(OrgId, Checksums, check_file).

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
                  Checksum :: binary()) -> {'error', binary()} |
                                           {'missing', binary()} |
                                           {'ok', binary()}.
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
                 Checksum :: binary()) -> {'error', binary()} |
                                          {'missing', binary()} |
                                          {'ok', binary()}.
check_file(OrgId, AwsConfig, Bucket, Checksum) ->
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
                 ExceptionClass:Reason->
                     %% Something unanticipated happened.  We should log the specific reason
                     %% for later analysis, but as far as the overall checking operation is
                     %% concerned, this is "just an error", and we can continue along.
                     error_logger:error_msg("Checking presence of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) raised exception ~p:~p~n",
                                            [Checksum, OrgId, Bucket, Key, ExceptionClass, Reason]),
                     {error, Checksum}
             end,
    Result.

%% Private Utility Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Specifies how many requests to S3 / Bookshelf are in-flight at a given time.
-spec fanout() -> Size :: pos_integer().
fanout() ->
    chef_config:config_option(chef_objects, s3_parallel_ops_fanout, pos_integer).

%% @doc Specifies the maximum amount of time (in milliseconds) to wait for a SINGLE request
%% to S3 / Bookshelf to complete.
-spec timeout() -> MS :: pos_integer().
timeout() ->
    chef_config:config_option(chef_objects, s3_parallel_ops_timeout, pos_integer).

-type request_fun() :: check_file | delete_file.

%% @doc Parallelizes HTTP requests to S3.
%%
%% `Operation' is an atom naming a function in this module that will be used to actually
%% make the HTTP requests. The literal atom is also used for informative error logging.
-spec parallelize_checksum_op(OrgId :: object_id(),
                              Checksums :: [binary()],
                              Operation :: request_fun()) -> checksum_op_return_type().
parallelize_checksum_op(OrgId, Checksums, Operation) ->
 
    Bucket = chef_s3:bucket(),
    AwsConfig = chef_s3:get_config(),
    Timeout = timeout(),

    %% Create a function to map across the list of checksums.  It spawns an additional
    %% process to perform the request in order to control timeout situations.
    %% 
    %% This is necessary because we'd like to keep track of individual operation information
    %% like this, but erlware_commons now obscures timeout information.  Additionally,
    %% erlware_commons appears to only allow clients to specify a timeout on an entire list
    %% operation as a whole, instead of a timeout on each individual list item operation.
    %%
    %% By spawning a separate process and managing the timeout ourselves, we can once again
    %% capture this information.  Additionally, we no longer need to specify a timeout via the
    %% ec_plist "malt" configuration, since that now basically takes care of itself.
    MapFun = fun(Checksum) ->
                     Me = self(),
    
                     %% This token is used below in the receive block (just look!) to make
                     %% absolutely certain that we are only processing the exact message we
                     %% are expecting, as opposed to any other messages the receiving
                     %% process may be getting from elsewhere in the system.
                     %%
                     %% It's admittedly a bit of paranoia, but should help insulate from
                     %% potential future changes in erlware_commons (that involve message
                     %% passing).
                     %%
                     %% Also, paranoia.
                     Token = erlang:make_ref(),
                     Worker = proc_lib:spawn_link(fun() ->
                                                          %% Figure out what private function we should use
                                                          OpFun = get_fun(Operation),
                                                          Result = OpFun(OrgId,
                                                                         AwsConfig,
                                                                         Bucket,
                                                                         Checksum),
                                                          Me ! {Token, Result, self()}
                                                  end),
                     receive
                         {Token, Response, Worker} ->
                             Response
                     after Timeout ->
                             error_logger:error_msg("Operation '~p' on checksum: ~p for org ~p from bucket ~p has taken longer than ~p ms; killing spawned worker process ~p~n",
                                                    [Operation, Checksum, OrgId, Bucket, Timeout, Worker]),
                             erlang:unlink(Worker),
                             erlang:exit(Worker, kill),
                             {timeout, Checksum}
                     end
             end,
    
    %% Since making an HTTP request is a high-latency, IO-bound operation, we use a
    %% configuration where each list chunk has a single item, but we have multiple processes
    %% processing chunks.  This prevents pathological situations (like, for example, a large
    %% chunk of requests that all timeout taking LENGTH * TIMEOUT ms to complete), while
    %% maximizing the overall throughput.
    %%
    %% This configuration will also allow there to be `fanout()' HTTP requests in flight at
    %% any given time, which makes for more even throughput.
    %%
    %% While we can also specify a `{timeout, Millis}' tuple, this only applies to how long
    %% it takes to process the entire list; there does not currently appear to be a built-in
    %% way to manage individual process timeouts. This is why we manage our own timeouts, as
    %% implemented above in `MapFun'.
    %%
    %% See the documentation for erlware_commons' ec_plists module for more details.
    ParallelConfig = [1, {processes, fanout()}],

    %% Now we actually get to perform the requests!
    Results = ec_plists:ftmap(MapFun, Checksums, ParallelConfig),
    
    %% Now we need to consolidate our results based on:
    %%
    %%   Did the operation succeed?
    %%   Was the checksum even found?
    %%   Did the operation timeout?
    %%   Did some other error occur during the operation?.
    %%
    %% Note that items in `Results' are each wrapped in a `{value, Term}' tuple (this is an
    %% erlware_commons thing).  I think this wrapping is a hold-over from a previous
    %% implementation of erlware_commons that doesn't really make much sense anymore, but
    %% whatever...
    {OkChecksums, MissingChecksums, Timeouts, Errors} =
        lists:foldl(fun({value, Result}, {Ok, Missing, Timeouts, Errors}) ->
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

%% @doc This bit of gymnastics is required to both simplify the implementations of
%% `delete/2' and `fetch_md/2', and to make these private functions available for use by
%% erlware_commons.
%%
%% It just maps the name of the function to the actual function itself.
-spec get_fun(request_fun()) -> 
                     fun((object_id(), mini_s3:config(), string(), binary()) -> 
                                {'error' | 'missing' | 'ok', binary()} ).
get_fun(check_file)  -> fun check_file/4;
get_fun(delete_file) -> fun delete_file/4.
