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

-define(MAX, 5).
-define(TIMEOUT, 5 * 1000).

-include("chef_types.hrl").

-export([
         fetch_md/2,
         delete/2
        ]).

%% @doc Specifies how many requests to S3 / Bookshelf are in-flight at a given time.
-spec fanout() -> Size :: pos_integer().
fanout() ->
    fetch_and_validate_config_option(chef_objects, s3_parallel_ops_fanout).

%% @doc Specifies the maximum amount of time (in milliseconds) to wait for a SINGLE request
%% to S3 / Bookshelf to complete.
-spec timeout() -> MS :: pos_integer().
timeout() ->
    fetch_and_validate_config_option(chef_objects, s3_parallel_ops_timeout).

%% @doc Fetch a configuration value via `application:get_env/2` and verify it is a
%% non-negative integer.  Valid values are returned; invalid values trigger an error
%% @end
%%
%% The spec is specifically tailored for use in this module (to make Dialyzer happy), but it
%% is conceivable that the function could be used elsewhere as well.
-spec fetch_and_validate_config_option(Application :: chef_objects,
                                       OptionName :: s3_parallel_ops_fanout |
                                                     s3_parallel_ops_timeout) ->
                                              Value :: pos_integer().
fetch_and_validate_config_option(Application, OptionName) ->
    {ok, Value} = application:get_env(Application, OptionName),
    case {is_integer(Value), Value > 0} of
        {true, true} ->
            Value;
        _ ->
            error_logger:error_msg("Improper Configuration: ~p / ~p was ~p; should be a non-negative integer~n", [Application, OptionName, Value]),
            erlang:error({configuration, Application, OptionName, Value})
    end.

%% @doc Delete each checksummed file in S3
delete(OrgId, Checksums) when is_list(Checksums),
                              is_binary(OrgId) ->

    Bucket = chef_s3:bucket(),
    AwsConfig = chef_s3:get_config(),
    Timeout = timeout(),

    %% Using erlware_commons' ftmap ("fault-tolerant map") to handle the gory details of
    %% chunking and parallelizing the work of deleting files from S3 / Bookshelf.
    %%
    %% A 'malt' is erlware_commons jargon for the configuration 'blob' that specifies how an
    %% operation is to be parallelized using the ec_plist module (my favorite option for its
    %% meaning is "Malt is A List Tearing Specification"; see the module docs for more
    %% details on the various options available)
    %%
    %% Since making an HTTP request is a high-latency, IO-bound operation, we use a
    %% configuration where each list chunk has a single item, but we have multiple processes
    %% processing chunks.  This prevents pathological situations (like, for example, a large
    %% chunk of requests that all timeout taking LENGTH * TIMEOUT ms to complete), while
    %% maximizing the overall throughput.
    %%
    %% This configuration will also allow there to be fanout() HTTP requests in flight at
    %% any given time, which makes for more even throughput.
    %%
    %% While we can specify a {timeout, Millis} tuple in the malt, this only applies to how
    %% long it takes to process the entire list; there does not currently appear to be a
    %% built-in way to manage individual process timeouts.  To circumvent this, we'll spawn
    %% helper processes to manage timeouts ourselves (see comments for spawn_deleter/5
    %% for more details).
    Malt = [1,                      %% Number of items to process in a given batch
            {processes, fanout()}], %% Have fanout() processes working on the entire list

    Results = ec_plists:ftmap(fun(Checksum) ->
                                      spawn_deleter(OrgId, AwsConfig, Timeout, Bucket, Checksum)
                              end,
                              Checksums,
                              Malt),

    %% The results will be a list of lists, so we must flatten it.  We then process the
    %% flattened results, splitting them into successfully-deleted checksums, missing
    %% checksums (i.e., we got a 404 when trying to delete them), timeout count and error
    %% count.
    %%
    %% Note that ec_plists:ftmap/3 wraps each resulting list item in a {value, X} tuple.  I
    %% think this is a hold-over from a previous implementation that doesn't really make
    %% much sense anymore, but whatever...
    {Ok, Missing, NumTimeouts, NumErrors} =
        lists:foldl(fun({value, Result}, {Ok, Missing, Timeouts, Errors}) ->
                            case Result of
                                {ok, Checksum} -> {[Checksum | Ok], Missing, Timeouts, Errors};
                                {missing, Checksum} -> {Ok, [Checksum | Missing], Timeouts, Errors};
                                {error, _Checksum} -> {Ok, Missing, Timeouts, Errors + 1};
                                {timeout, _Checksum} -> {Ok, Missing, Timeouts + 1, Errors}
                            end
                    end,
                    {[], [], 0, 0},
                    lists:flatten(Results)),

    %% A final bit of processing to sort the lists of checksums before we return
    {{ok, lists:sort(Ok)},
     {missing, lists:sort(Missing)},
     {timeout, NumTimeouts},
     {error, NumErrors}}.

%% @doc Delete an existing checksum file in S3
-spec delete_file(OrgId :: object_id(),
                  AwsConfig :: mini_s3:config(),
                  Bucket :: string(),
                  Checksum :: binary()) -> {'error', binary()} |
                                           {'missing', binary()} |
                                           {'ok', binary()}.
delete_file(OrgId, AwsConfig, Bucket, Checksum) ->
    Key = chef_s3:make_key(OrgId, Checksum),

    try mini_s3:delete_object(Bucket, Key, AwsConfig) of
        %% Return value doesn't much matter here but it looks like, but
        %% for documentation's sake it looks like:
        %%
        %%  [{delete_marker, list_to_existing_atom(Marker)}, {version_id, Id}]
        %%
        %% The main take-away here is that the call to delete_object/3 was successful and
        %% didn't throw an error
        _Response ->
            {ok, Checksum}
    catch
        error:{aws_error, {http_error,404,_}} ->
            %% We got a 404.  No biggie; mark it as 'missing' and move on
            {missing, Checksum};
        ExceptionClass:Reason->
            %% Something unanticipated happened.  We should log the specific reason for
            %% later analysis, but as far as the overall deletion operation is concerned,
            %% this is "just an error", and we can continue along.
            error_logger:error_msg("Deletion of file (checksum: ~p) for org ~p from bucket ~p (key: ~p) raised exception ~p:~p~n",
                                   [Checksum, OrgId, Bucket, Key, ExceptionClass, Reason]),
            {error, Checksum}
    end.

%% @doc Spawns a worker process to perform the actual deletion of an object from S3 /
%% Bookshelf so that we can impose a timeout on the operation.
%%
%% This is necessary because we'd like to keep track of individual operation information
%% like this, but erlware_commons now obscures timeout information.  Additionally,
%% erlware_commons appears to only allow clients to specify a timeout on an entire list
%% operation as a whole, instead of a timeout on each individual list item operation.
%%
%% By spawning a separate process and managing the timeout ourselves, we can once again
%% capture this information.  Additionally, we no longer need to specify a timeout via the
%% ec_plist "malt" configuration, since that now basically takes care of itself.
-spec spawn_deleter(OrgId :: object_id(),
                    AwsConfig :: mini_s3:config(),
                    Timeout :: integer(),
                    Bucket::string(),
                    Checksum :: binary()) -> {'error', binary()} |
                                             {'missing', binary()} |
                                             {'ok', binary()} |
                                             {'timeout', binary()}.
spawn_deleter(OrgId, AwsConfig, Timeout, Bucket, Checksum) ->
    Me = self(),

    %% This token is used below in the receive block (just look!) to make absolutely certain
    %% that we are only processing the exact message we are expecting, as opposed to any
    %% other messages the receiving process may be getting from elsewhere in the system.
    %%
    %% It's admittedly a bit of paranoia, but should help insulate from potential future
    %% changes in erlware_commons (that involve message passing).
    %%
    %% Also, paranoia.
    Token = erlang:make_ref(),

    Worker = erlang:spawn(fun() ->
                                  Result = delete_file(OrgId, AwsConfig, Bucket, Checksum),
                                  Me ! {Token, Result, self()}
                          end),
    receive
        {Token, Response, Worker} ->
            Response
    after Timeout ->
            error_logger:error_msg("Deletion of file (checksum: ~p) for org ~p from bucket ~p has taken longer than ~p ms; killing spawned worker process ~p~n",
                                   [Checksum, OrgId, Bucket, Timeout, Worker]),
            erlang:exit(Worker, kill),
            {timeout, Checksum}
    end.

%% File Checking Code Below
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {
          token :: reference(),
          checksums = [] :: [binary()],
          org_id :: object_id(),
          bucket :: string(),
          count = 0 :: non_neg_integer(),
          workers = [] :: [pid()],

          ok = [] :: [binary()],
          missing = [] :: [binary()],
          errors = 0 :: non_neg_integer()
         }).



%% @doc Verify that each checksummed file is stored in S3 by checking its metadata
fetch_md(OrgId, Checksums) when is_list(Checksums),
                                is_binary(OrgId) ->
    parallel_fetch(OrgId, Checksums).

parallel_fetch(OrgId, Checksums) ->

    Bucket = chef_s3:bucket(),

    Remainder = length(Checksums) rem ?MAX,
    Token = erlang:make_ref(),

    % Using a record since there's quite a bit to keep track of;
    % otherwise the function signatures would get cumbersome
    State = #state{checksums=Checksums,
                   org_id=OrgId,
                   bucket=Bucket,
                   token=Token},

    StateBeforeRemainder = lists:foldl(fun fetcher/2, State, Checksums),
    fetch_remainder(StateBeforeRemainder#state{count=Remainder}).

fetcher(Checksum, #state{count=?MAX}=State) ->
    State1 = gather(State),
    fetcher(Checksum, State1#state{count=0, workers=[]});

fetcher(Checksum, #state{count=Count,
                         token=Token,
                         org_id=OrgId,
                         bucket=Bucket,
                         workers=Workers}=State) ->
    Worker = spawn_worker(OrgId, Bucket, Checksum, Token),
    State#state{count=Count+1,
                workers=[Worker|Workers]}.

fetch_remainder(#state{}=State) ->
    #state{ok=Ok, errors=Errors, missing=Missing} = gather(State),
    Result = {{ok, lists:sort(Ok)},
              {missing, lists:sort(Missing)},
              {error, Errors}},
    Result.

gather(#state{count=0}=State) ->
    State;
gather(#state{token=Token, ok=Ok, missing=Missing, errors=Errors, count=Count, workers=Workers}=State) ->
    receive
        {Token, {Status, Checksum}, WorkerPid} ->
            ResultState0 = case Status of
                               ok ->
                                   State#state{ok=[Checksum|Ok]};
                               missing ->
                                   State#state{missing=[Checksum|Missing]};
                               error ->
                                   State#state{errors=Errors + 1}
                           end,
            ResultState = ResultState0#state{count=Count-1,
                                             workers=lists:delete(WorkerPid, Workers)},
            gather(ResultState)
    after ?TIMEOUT ->
            %% Kill remaining workers; they're taking too long
            [ erlang:exit(W, kill) || W <- Workers ],
            State#state{workers=[], errors=Errors + length(Workers)}
    end.

-spec spawn_worker(OrgId :: object_id(),
                   Bucket :: string(),
                   Checksum :: binary(),
                   Token :: reference()) -> Worker :: pid().
spawn_worker(OrgId, Bucket, Checksum, Token) ->
    Master = self(), %% This is the same as the process that is doing the scatter / gather
    erlang:spawn(fun() ->
                         Result = check_file(OrgId, Bucket, Checksum),
                         Master ! {Token, Result, self()}
                 end).

%% @doc Check to see if a checksum file is stored in S3 already by checking for file
%% metadata.
-spec check_file(OrgId :: object_id(),
                 Bucket :: string(),
                 Checksum :: binary()) -> {'error', binary()} |
                                          {'missing', binary()} |
                                          {'ok',binary()}.
check_file(OrgId, Bucket, Checksum) ->
    Key = chef_s3:make_key(OrgId, Checksum),
    AwsConfig = chef_s3:get_config(),
    Result = try mini_s3:get_object_metadata(Bucket, Key, [], AwsConfig) of
                 _V ->
                     {ok, Checksum}
             catch
                 error:{aws_error, {http_error,404,_}} ->
                     {missing, Checksum};
                 _X:_Y->
                     {error, Checksum}
             end,
    Result.
