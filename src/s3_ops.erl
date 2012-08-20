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

-module(s3_ops).

-define(MAX, 5).
-define(TIMEOUT, 5 * 1000).
-define(CHUNKSIZE, 20). %% TODO - make this configurable

-include("chef_types.hrl").

-export([
         fetch_md/2,
         delete/2
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
          timeouts = 0,
          errors = 0 :: non_neg_integer()
         }).

%% @doc Delete each checksummed file in S3
delete(OrgId, Checksums) when is_list(Checksums),
                             is_binary(OrgId) ->
    %% we'll split our checksum list into chunks
    %% so we don't pound the S3 store by
    %% parallelizing ALL THE THINGS!!
    Results = chunk_map(fun(Chunk) -> parallel_delete(OrgId, Chunk) end,
                        Checksums, chunk_size()),
    %% The results will be a list of lists so flatten it
    FlattenedResults = lists:flatten(Results),
    %% Process the flattened results, splitting them into success checksums,
    %% missing checksums, timeout count and error count
    #state{ok=Ok, missing=Missing, timeouts=Timeouts, errors=Errors} =
        lists:foldl(fun(A, #state{ok=Ok,missing=Missing,timeouts=Timeouts,errors=Errors}=State) ->
            case A of
                {value, Checksum} -> State#state{ok=[Checksum|Ok]};
                {missing, Checksum} -> State#state{missing=[Checksum|Missing]};
                timeout -> State#state{timeouts=Timeouts + 1};
                _Error -> State#state{errors=Errors + 1}
            end
        end,
        #state{},
        FlattenedResults
        ),

    error_logger:info_msg(
        "Deleted ~p/~p checksums with ~p missing, ~p timeouts and ~p errors~n",
        [length(Ok), length(Checksums), length(Missing), Timeouts, Errors]
    ),
    Result = {{ok, lists:sort(Ok)},
              {missing, lists:sort(Missing)},
              {timeout, Timeouts},
              {error, Errors}},
    Result.

parallel_delete(OrgId, Checksums) ->
    Bucket = chef_s3:bucket(),
    %% We'll take advantage of Erlware's excellent ec_plists:ftmap/3 which
    %% applies a function to a list, in parrellel, in a fault tolerant way.
    %% The return result looks something like:
    %%
    %%  [{value, 1}, {value, 2}, timeout, {badmatch, ...}]
    %%
    ec_plists:ftmap(fun(Checksum) ->
                    case delete_file(OrgId, Bucket, Checksum) of
                        {ok, Checksum} -> Checksum;
                        {Error, Checksum} -> throw({Error, Checksum})
                    end
                end,
                Checksums,
                ?TIMEOUT).

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

%% @doc Delete an existing checksum file in S3
-spec delete_file(OrgId :: object_id(),
                 Bucket :: string(),
                 Checksum :: binary()) -> {'error', binary()} |
                                          {'missing', binary()} |
                                          {'ok', binary()}.
delete_file(OrgId, Bucket, Checksum) ->
    Key = chef_s3:make_key(OrgId, Checksum),
    AwsConfig = chef_s3:get_config(),
    Result = try mini_s3:delete_object(Bucket, Key, AwsConfig) of
                %% Return value doesn't much matter here but it looks like, but
                %% for documentation sake it looks like:
                %%
                %%  [{delete_marker, list_to_existing_atom(Marker)}, {version_id, Id}]
                %%
                 _Response ->
                     {ok, Checksum}
             catch
                error:{aws_error, {http_error,404,_}} ->
                     {missing, Checksum};
                X:Y->
                    error_logger:error_msg(
                        "Could not delete checksum ~p from bucket ~p: Reason -> ~p:~p~n",
                        [Checksum, Bucket, X, Y]
                    ),
                    {error, Checksum}
             end,
    Result.

%% TODO - pull this from config
chunk_size() ->
    ?CHUNKSIZE.

%% ----------------------------------------------------------------------------
%% opscode_commons candidate code
%% ----------------------------------------------------------------------------

%% chunk_list(List, ChunkSize) ->
%%     chunk_list(List, ChunkSize, []).
%
%% chunk_list([], _, Result) ->
%%     lists:reverse(Result);
%% chunk_list(List, ChunkSize, Result) ->
%%     {Chunk, Rest} = safe_split(ChunkSize, List),
%%     chunk_list(Rest, ChunkSize, [Chunk | Result]).

chunk_map(Fun, List, ChunkSize) ->
    chunk_map(Fun, List, ChunkSize, []).

chunk_map(_Fun, [], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_map(Fun, List, ChunkSize, Acc) ->
    {Chunk, Rest} = safe_split(ChunkSize, List),
    chunk_map(Fun, Rest, ChunkSize, [Fun(Chunk) | Acc]).

safe_split(N, L) ->
    try
        lists:split(N, L)
    catch
        error:badarg ->
            {L, []}
    end.
