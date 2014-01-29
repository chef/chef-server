%% Copyright 2014 Chef Software, Inc. All Rights Reserved.
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

-module(bookshelf_bench).

-behaviour(gen_server).

%% API
-export([
         random_data/1,
         random_get/1,
         random_put/1,
         start_link/0
        ]).

%% gen_server callbacks
-export([
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {checksums,
                num_checksums,
                s3_config}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% add all of the deps to the code path because
    %% basho bench doesn't allow us to specify wildcards
    %% in the code_path
    %%
    %% basho_bench sets cwd to ./tests/current so we need to
    %% traverse up an extra directory to get to deps
    code:add_paths(filelib:wildcard("../../../deps/*/ebin")),

    DataProfile = basho_bench_config:get(bookshelf_data_profile),
    ChecksumSizes = parse_distro_file(DataProfile),
    NumChecksums = length(ChecksumSizes),

    S3AccessKeyId = basho_bench_config:get(s3_access_key_id),
    S3SecretAccessKey = basho_bench_config:get(s3_secret_access_key),
    S3Host = basho_bench_config:get(s3_host),

    S3Config = mini_s3:new(S3AccessKeyId, S3SecretAccessKey, S3Host, path),

    %% todo:
    %% this is ugly here, but we need to get ibrowse started
    ok = application:start(ibrowse),
    ibrowse_http_client:start({"localhost", 4321}),

    %% create all of the checksums in our bucket
    [begin
         Data = crypto:rand_bytes(Size),
         mini_s3:put_object("bookshelf",           % bucket name
                            integer_to_list(Size), % key
                            Data,                  % object data
                            [],                    % options
                            [],                    % headers
                            S3Config)              % config
     end || Size <- ChecksumSizes],

    {ok, #state{checksums = ChecksumSizes,
                num_checksums = NumChecksums,
                s3_config = S3Config}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({random_get}, _From, State) ->
    random_url(get, State);

handle_call({random_put}, _From, State) ->
    random_url(put, State);

handle_call({random_data}, _From, #state{checksums = Checksums,
                                         num_checksums = NumChecksums} = State) ->
    Index = random:uniform(NumChecksums),
    Size = lists:nth(Index, Checksums),
    Data = crypto:rand_bytes(Size),
    {reply, Data, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

random_get(_ID) ->
    fun() ->
            gen_server:call(?MODULE, {random_get})
    end.

random_put(_ID) ->
    fun() ->
            gen_server:call(?MODULE, {random_put})
    end.

random_data(_ID) ->
    fun() ->
            gen_server:call(?MODULE, {random_data})
    end.

random_url(Method, #state{checksums = Checksums,
                          num_checksums = NumChecksums,
                          s3_config = S3Config} = State) ->
    Index = random:uniform(NumChecksums),
    Size = lists:nth(Index, Checksums),
    FullUrl = mini_s3:s3_url(Method,
                             "bookshelf",
                             integer_to_list(Size),
                             30,
                             [],
                             S3Config),
    "http://localhost:4321/bookshelf/" ++ S3Path = binary_to_list(FullUrl),
    {reply, S3Path, State}.


parse_distro_file(FileName) ->
    case file:open(FileName, [read]) of
        {ok, IO} ->
            parse_lines(IO);
        {error, _} = Error ->
            Error
    end.

parse_lines(IO) ->
    parse_lines(file:read_line(IO), IO, []).

parse_lines(eof, IO, Acc) ->
    file:close(IO),
    Acc;
parse_lines({ok, Line}, IO, Acc) ->
    {Num, Size} = parse_line(Line),
    %% Add Num elements of Size to the accumulator list
    Acc1 = lists:foldl(fun(_X, Acc0) ->
                               [ Size | Acc0 ]
                       end, Acc, lists:seq(1, Num)),
    parse_lines(file:read_line(IO), IO, Acc1).

parse_line(Line) ->
    Regexp = "(?<num>[0-9]+)[^0-9]*(?<size>[0-9]+)",
    case re:run(Line, Regexp, [{capture, all, list}]) of
        {match, [_Match, Num, Size]} ->
            {list_to_integer(Num), list_to_integer(Size)};
        Error ->
            Error
    end.
