%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
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

-module(bksw_wm_object).

-include_lib("mixer/include/mixer.hrl").
-mixin([{bksw_wm_base, [init/1,
                        is_authorized/2,
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
    CType =
    case wrq:get_req_header("accept", Rq) of
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
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    %% Buckets always exist for writes since we create them on the fly
    case wrq:method(Rq0) of
        'PUT' ->
            {true, Rq0, Ctx};
        _ ->
            %% determine if the entry exists by opening it. This way, we can cache the fd
            %% and avoid extra system calls. It also helps to keep the request processing
            %% more consistent since we will open the fd once at start and hold on to it.
            %% Note that there is still a possible discrepency when we read the meta data.
            case bksw_io:open_for_read(Bucket, Path) of
                {error, enoent} ->
                    {false, Rq0, Ctx};
                {ok, Ref} ->
                    {true, Rq0, Ctx#context{entry_ref = Ref}}
            end
    end.

last_modified(Rq0, Ctx) ->
    case entry_md(Ctx) of
        {#object{date = Date}, CtxNew} ->
            {Date, Rq0, CtxNew};
        _ ->
            {halt, Rq0, Ctx}
    end.

generate_etag(Rq0, Ctx) ->
    case entry_md(Ctx) of
        {#object{digest = Digest}, CtxNew} ->
            {bksw_format:to_base64(Digest), Rq0, CtxNew};
        _ ->
            {halt, Rq0, Ctx}
    end.

delete_resource(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    {bksw_io:entry_delete(Bucket, Path), Rq0, Ctx}.

%% Return `{Obj, CtxNew}' where `Obj' is the entry meta data `#object{}' record or the atom
%% `error'. The `CtxNew' may have been updated and should be kept. Accessing entry md
%% through this function ensures we only ever read the md from the file system once.
entry_md(#context{entry_md = #object{} = Obj} = Ctx) ->
    {Obj, Ctx};
entry_md(#context{entry_ref = Ref, entry_md = undefined} = Ctx) ->
    case bksw_io:entry_md(Ref) of
        {ok, #object{} = Obj} ->
            {Obj, Ctx#context{entry_md = Obj}};
        Error ->
            {Error, Ctx}
    end.

%%
%% Resource Helpers
%%

download(Rq0, #context{entry_ref = Ref, stream_download = true} = Ctx) ->
    {{stream, send_streamed_body(Ref)}, Rq0, Ctx};
download(Rq0, #context{entry_ref = Ref, stream_download = false} = Ctx) ->
    {fully_read(Ref, []), Rq0, Ctx}.

upload(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    case bksw_io:open_for_write(Bucket, Path) of
        {ok, Ref} ->
            write_streamed_body(wrq:stream_req_body(Rq0, ?BLOCK_SIZE), Ref, Rq0, Ctx);
        Error ->
            error_logger:error_msg("Erroring opening ~p/~p for writing: ~p~n", [Bucket, Path, Error]),
            {false, Rq0, Ctx}
    end.


%%===================================================================
%% Internal Functions
%%===================================================================
send_streamed_body(Ref) ->
     case bksw_io:read(Ref, ?BLOCK_SIZE) of
         {ok, eof} ->
            bksw_io:finish_read(Ref),
            {<<>>, done};
         {ok, Data} ->
             case byte_size(Data) < ?BLOCK_SIZE of
                 true ->
                     bksw_io:finish_read(Ref),
                     {Data, done};
                 false ->
                     {Data, fun() -> send_streamed_body(Ref) end}
             end;
        Error = {error, _} ->
             bksw_io:finish_read(Ref),
             error_logger:error_msg("Error occurred during content download: ~p~n", [Error]),
             Error
     end.

fully_read(Ref, Accum) ->
    case bksw_io:read(Ref, ?BLOCK_SIZE) of
        {ok, eof} ->
            lists:reverse(Accum);
        {ok, Data} ->
            fully_read(Ref, [Data|Accum]);
        Error ->
            error_logger:error_msg("Error occurred during content download: ~p~n", [Error]),
            lists:reverse(Accum)
    end.

write_streamed_body({Data, done}, Ref, Rq0, Ctx) ->
    {ok, Ref1} = bksw_io:write(Ref, Data),
    {ok, Digest} = bksw_io:finish_write(Ref1),
    case get_header('Content-MD5', Rq0) of
        undefined ->
            Rq1 = bksw_req:with_etag(base64:encode(Digest), Rq0),
            {true, wrq:set_response_code(202, Rq1), Ctx};
        RawRequestMd5 ->
            RequestMd5 = base64:decode(RawRequestMd5),
            case RequestMd5 of
                Digest ->
                    Rq1 = bksw_req:with_etag(RawRequestMd5, Rq0),
                    {true, wrq:set_response_code(202, Rq1), Ctx};
                _ ->
                    {true, wrq:set_response_code(406, Rq0), Ctx}
            end
    end;
write_streamed_body({Data, Next}, Ref, Rq0, Ctx) ->
    {ok, Ref1} = bksw_io:write(Ref, Data),
    write_streamed_body(Next(), Ref1, Rq0, Ctx).

get_header(Header, Rq) ->
    wrq:get_req_header(Header, Rq).
