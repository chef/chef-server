%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Mark Anderson <mark@chef.io>
%% @author Tim Dysinger <dysinger@chef.io>
%% Copyright 2012-2015 Opscode, Inc. All Rights Reserved.
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

-module(bksw_wm_sql_object).

-include_lib("mixer/include/mixer.hrl").
-mixin([{bksw_wm_base, [init/1,
                        is_authorized/2,
                        malformed_request/2,
                        finish_request/2,
                        service_available/2]}]).

%% Webmachine callbacks
-export([allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         generate_etag/2,
         last_modified/2,
         resource_exists/2,

         %% Override
         validate_content_checksum/2,

         %% Resource helpers
         download/2,
         upload/2]).


-include_lib("webmachine/include/webmachine.hrl").
-include("internal.hrl").

%%===================================================================
%% Public API
%%===================================================================

%% By default, if wm sees a 'content-md5' header, it will read the request body to compute
%% the md5 and compare to the header value. A 400 will then be returned automagically by wm
%% if the digests do not match. Since we wish to read request bodies in a streaming fashion,
%% we need to handle our own checksum validation. Using wm's default would mean having a
%% full copy of the request body buffered into the request process state. So we define this
%% resource callback to blindly say the content is valid and then do the verification in the
%% upload/2 flow.
validate_content_checksum(Rq, Ctx) ->
    {true, Rq, Ctx}.

allowed_methods(Rq, Ctx) ->
    {['HEAD', 'GET', 'PUT', 'DELETE'], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    CType = case wrq:get_req_header("accept", Rq) of
                undefined ->
                    "application/octet-stream";
                "*/*" ->
                    "application/octet-stream";
                C ->
                    C
            end,
    {[{CType, download}], Rq, Ctx}.

content_types_accepted(Rq, Ctx) ->
    CT = case wrq:get_req_header("content-type", Rq) of
             undefined ->
                 "application/octet-stream";
             X ->
                 X
         end,
    {MT, _Params} = webmachine_util:media_type_to_detail(CT),
    {[{MT, upload}], Rq, Ctx}.

resource_exists(Rq0, Ctx) ->
    case wrq:method(Rq0) of
        'PUT' ->
            case fetch_entry_md(Rq0, Ctx) of
                {error, _Error} ->
                    {false, Rq0, Ctx};
                {not_found, #context{} = Ctx1 } ->
                    %% Buckets always exist for writes since we create them on the fly
                    %% This follows the pattern established in bksw_wm_object.erl
                    {true, Rq0, Ctx1};
                {#db_file{}, Ctx1} ->
                    {true, Rq0, Ctx1}
            end;
        _ ->
            %% determine if the entry exists by opening it. This way, we can cache the fd
            %% and avoid extra system calls. It also helps to keep the request processing
            %% more consistent since we will open the fd once at start and hold on to it.
            %% Note that there is still a possible discrepency when we read the meta data.
            case fetch_entry_md(Rq0, Ctx) of
                {error, _Error} ->
                    {false, Rq0, Ctx};
                {not_found, #context{} = Ctx1 } ->
                    {false, Rq0, Ctx1};
                {#db_file{}, Ctx1} ->
                    {true, Rq0, Ctx1}
            end
    end.

last_modified(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{created_at = {datetime, Date}}, CtxNew} ->
            {Date, Rq0, CtxNew};
        _ ->
            %% common errors (not found, etc, will have been caught by resource_exists
            %% because that executes first (G7 in webmachine_decision_core.erl)
            {halt, Rq0, Ctx}
    end.

generate_etag(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{hash_md5 = Digest}, CtxNew} ->
            {bksw_format:to_base64(Digest), Rq0, CtxNew};
        _ ->
            %% common errors (not found, etc, will have been caught by resource_exists
            %% because that executes first (G7 in webmachine_decision_core.erl)
            {halt, Rq0, Ctx}
    end.

delete_resource(Rq0, Ctx) ->
    case fetch_entry_md(Rq0, Ctx) of
        {#db_file{file_id = FileId}, CtxNew} ->
            case bksw_sql:delete_file(FileId) of
                ok ->
                    {true, Rq0, CtxNew};
                _ ->
                    {halt, Rq0, CtxNew}
            end;
        _ ->
            %% common errors (not found, etc, will have been caught by resource_exists
            %% because that executes first (G7 in webmachine_decision_core.erl)
            {halt, Rq0, Ctx}
    end.

maybe_retry(#context{sql_retry_count = 0}, _RetryFun, FailFun) ->
    FailFun();
maybe_retry(#context{sql_retry_delay = Delay, sql_retry_count = Count} = Ctx, RetryFun, _FailFun) ->
    timer:sleep(Delay),
    RetryFun(Ctx#context{sql_retry_count = Count - 1}).

%% Return `{Obj, CtxNew}' where `Obj' is the entry meta data `#db_file{}' record or the atom
%% `error'. The `CtxNew' may have been updated and should be kept. Accessing entry md
%% through this function ensures we only ever read the md from the file system once.
-spec fetch_entry_md(#wm_reqdata{}, #context{}) -> {#db_file{}, #context{}}|{not_found, #context{}}|{error, #context{}}.
fetch_entry_md(_Req, #context{entry_md = #db_file{} = Obj} = Ctx) ->
    {Obj, Ctx};
fetch_entry_md(Req, #context{} = Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Req),
    case bksw_sql:find_file(Bucket,Path) of
        {ok, #db_file{} = Object} ->
            {Object, Ctx#context{entry_md = Object}};
        {ok, not_found} ->
            {not_found, Ctx#context{entry_md = #db_file{bucket_name = Bucket, name = Path}}};
        {error, no_connections} ->
            maybe_retry(Ctx, fun(Ctx1) -> fetch_entry_md(Req, Ctx1) end,
                        fun() ->
                                error_logger:error_msg("fetch_entry_md no_connections", []),
                                {error, no_connections}
                        end);
        {error, Error} ->
            error_logger:error_msg("Error occurred during fetch_entry_md: B:~p P:~p '~p'~n", [Bucket, Path, Error]),
            {error, Ctx}
    end.

%%
%% Resource Helpers
%%

download(Rq0, #context{stream_download = true} = Ctx0) ->
    Ctx = Ctx0#context{transfer_state = bksw_sql:init_transfer_state()},
    {{stream, send_streamed_body(Ctx)}, Rq0, Ctx};
download(Rq0, #context{stream_download = false} = Ctx0) ->
    Ctx = Ctx0#context{transfer_state = bksw_sql:init_transfer_state()},
    {fully_read(Ctx, []), Rq0, Ctx}.

maybe_upload(Rq, #context{entry_md = #db_file{} = Obj0}= Ctx0, {ok, DataId}) ->
    Obj1 = Obj0#db_file{data_id = DataId},
    Ctx1 = Ctx0#context{entry_md = Obj1, transfer_state = bksw_sql:init_transfer_state()},
    Resp = write_streamed_body(wrq:stream_req_body(Rq, ?BLOCK_SIZE), Rq, Ctx1),
    Resp;
maybe_upload(Rq, Ctx, {error, Error}) -> %% Clean up and return useful error
    error_logger:error_msg("maybe_upload error: ~p~n", [Error]),
    {halt, Rq, Ctx};
maybe_upload(Rq, Ctx, Error) -> %% Clean up and return useful error
    error_logger:error_msg("maybe_upload unknown response: ~p~n", [Error]),
    {halt, Rq, Ctx}.

upload(Rq, Ctx) ->
    %% Uploads create a new data segment, regardless of file existence
    maybe_upload(Rq, Ctx, bksw_sql:insert_file_data()).

%%===================================================================
%% Internal Functions
%%===================================================================

send_streamed_body(#context{entry_md = #db_file{chunk_count = ChunkCount, hash_sha512 = ShaExpected},
                            transfer_state = #file_transfer_state{next_chunk = ChunkCount,
                                                                  hash_context_sha512 = ShaSent
                                                                 } = _TransferState0}) ->
    case crypto:hash_final(ShaSent) of
        ShaExpected ->
            ok;
        S ->
            lager:error("checksum mismatch on download: expected: ~p; sent: ~p", [ShaExpected, S])
    end,
    {<<>>, done};
send_streamed_body(#context{entry_md = #db_file{data_id = DataId} = DbFile,
                            transfer_state = #file_transfer_state{next_chunk = ChunkId} = TransferState0} = Ctx) ->
    case bksw_sql:get_chunk_data(DataId, ChunkId) of
        {ok, not_found} ->
            error_logger:error_msg("Error occurred during content download: missing chunk ~p ~p ~n", [ChunkId, DbFile]),
            {error, missing_chunk};
        {ok, []} ->
            error_logger:error_msg("Empty chunk sent Chunk ~p ~p~n", [ChunkId, DbFile]),
            {[], fun() -> send_streamed_body(Ctx) end};
        {ok, Data} ->
            TransferState1 = bksw_sql:update_transfer_state(TransferState0, Data, 1),
            {Data, fun() -> send_streamed_body(Ctx#context{transfer_state = TransferState1}) end};
        {error, no_connections} ->
            maybe_retry(Ctx, fun(Ctx1) -> send_streamed_body(Ctx1) end,
                        fun() ->
                                error_logger:error_msg("send_streamed_body no_connections", []),
                                {error, no_connections}
                        end);
        {error, _} = Error ->
            %% This crashes webmachine; we need a smoother way of handling this.
            error_logger:error_msg("Error occurred during content download: d~p c~p ~p~n", [DataId, ChunkId, Error]),
            Error
    end.

fully_read(#context{} = Ctx, []) ->
    fully_read(send_streamed_body(Ctx), []);
fully_read({_, done}, Accum) ->
    lists:reverse(Accum);
fully_read({error, Error}, Accum) ->
    error_logger:error_msg("Error occurred during content download: ~p~n", [Error]),
    lists:reverse(Accum);
fully_read({Data, Next}, Accum) ->
    fully_read(Next(), [Data | Accum]).

%% Creates or replaces a file.
%% Existing files have their data segment replaced.
%% New files will have no file_id, and neeed a new file name entry
finalize_maybe_create_file(_Rq, _Ctx,
                           #db_file{file_id = undefined,
                                    bucket_name = BucketName,
                                    name = Name,
                                    data_id = DataId}) ->
    case bksw_sql:create_file_link_data(BucketName, Name, DataId) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("create_file_link_data ~p~n", [Reason]),
            fail
    end;
%% If we have a file_id, we're replacing an old file
finalize_maybe_create_file(_Rq, _Ctx,
                           #db_file{file_id = FileId,
                                    data_id = DataId}) ->
    case bksw_sql:link_file_data(FileId, DataId) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("link_file_data ~p~n", [Reason]),
            fail
    end.

%%
%% Takes a chunk from webmachine, and streams it out to the db
%%
%% A future version of this may want to re-chunk things;
write_streamed_body({Data, done}, Rq0,
                    #context{entry_md = #db_file{data_id = DataId} = File0,
                             transfer_state = #file_transfer_state{next_chunk = ChunkId} = TransferState0} = Ctx0 ) ->
    #file_transfer_state{next_chunk = ChunkCount} = TransferState1 =
        case Data of
            <<"">> ->
                TransferState0;
            Data ->
                ok = bksw_sql:add_file_chunk(DataId, ChunkId, Data),
                bksw_sql:update_transfer_state(TransferState0, Data, 1)
        end,


    #db_file{data_size = Size,
             hash_md5 = HashMd5,
             hash_sha256 = HashSha256,
             hash_sha512 = HashSha512} = File1 =
        bksw_sql:finalize_transfer_state(TransferState1, File0),
    Ctx1 = Ctx0#context{entry_md = File1},
    bksw_sql:update_metadata(DataId, Size, ChunkCount, HashMd5, HashSha256, HashSha512),

    case get_header('Content-MD5', Rq0) of
        undefined ->
            Rq1 = bksw_req:with_etag(base64:encode(HashMd5), Rq0),
            %% Failures here turn into 500s and stack traces; we might want to do better if this
            %% happens much.
            ok = finalize_maybe_create_file(Rq1, Ctx0, File1),
            {{halt, 204}, Rq1, Ctx1};
        RawRequestMd5 ->
            RequestMd5 = base64:decode(RawRequestMd5),
            case RequestMd5 of
                HashMd5 ->
                    Rq1 = bksw_req:with_etag(RawRequestMd5, Rq0),
                    %% Failures here turn into 500s and stack traces; we might want to do better if this
                    %% happens much.
                    ok = finalize_maybe_create_file(Rq1, Ctx0, File1),
                    {{halt, 204}, Rq1, Ctx1};
                _ ->
                    lager:error("Mismatch between Content-MD5 and actual content. Content-MD5: ~p; Actual: ~p", [RequestMd5, HashMd5]),
                    %% Exiting here causes uploads to be abandoned, but the upload_cleanup task will
                    %% eventually clean things up.
                    {{halt, 406}, Rq0, Ctx1}
            end
    end;
write_streamed_body({Data, Next}, Rq0,
                    #context{entry_md = #db_file{data_id = DataId},
                             transfer_state = #file_transfer_state{next_chunk = ChunkId} = TransferState0} = Ctx0 ) ->
    ok = bksw_sql:add_file_chunk(DataId, ChunkId, Data),
    TransferState1 = bksw_sql:update_transfer_state(TransferState0, Data, 1),

    Ctx = Ctx0#context{transfer_state = TransferState1},
    write_streamed_body(Next(), Rq0, Ctx).

get_header(Header, Rq) ->
    wrq:get_req_header(Header, Rq).
