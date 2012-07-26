%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
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


-module(s3_metadata).

-define(MAX, 5).
-define(TIMEOUT, 5 * 1000).

-include("chef_types.hrl").

-export([
         fetch/2
        ]).

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
fetch(OrgId, Checksums) when is_list(Checksums),
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
                 error:{aws_error, {http_error,404,_,_}} ->
                     {missing, Checksum};
                 _X:_Y->
                     {error, Checksum}
             end,
    Result.
