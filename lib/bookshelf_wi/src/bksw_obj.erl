%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_obj).

-export([allowed_methods/2, content_types_accepted/2,
         content_types_provided/2, delete_resource/2, download/2,
         generate_etag/2, init/1, is_authorized/2, last_modified/2,
         resource_exists/2, upload_or_copy/2]).

-include_lib("bookshelf_store/include/bookshelf_store.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include("internal.hrl").

%%===================================================================
%% Public API
%%===================================================================

init(_Context) ->
    {ok, bksw_conf:get_context()}.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

allowed_methods(Rq, Ctx) ->
    {['HEAD', 'GET', 'PUT', 'DELETE'], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    CType =
    case wrq:get_req_header("accept", Rq) of
        undefined ->
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
    {[{MT, upload_or_copy}], Rq, Ctx}.

resource_exists(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    %% Buckets always exist for writes since we create them on the fly
    case wrq:method(Rq0) of
        'PUT' ->
            {true, Rq0, Ctx};
        _ ->
            {bookshelf_store:obj_exists(Bucket, Path), Rq0, Ctx}
    end.

delete_resource(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    case bookshelf_store:obj_delete(Bucket, Path) of
        ok ->
            {true, Rq0, Ctx};
        _ ->
            {false, Rq0, Ctx}
    end.

last_modified(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{date = Date}} ->
            {Date, Rq0, Ctx};
        _ ->
            {halt, Rq0, Ctx}
    end.

generate_etag(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{digest = Digest}} ->
            {bksw_format:to_base64(Digest), Rq0, Ctx};
        _ ->
            {halt, Rq0, Ctx}
    end.

upload_or_copy(Rq0, Ctx) ->
    Rq1 = wrq:set_resp_header("Obj", "Obj", Rq0),
    case get_header("X-Amz-Copy-Source", Rq1) of
        undefined ->
            upload(Rq1, Ctx);
        Source ->
            copy(Rq1, Ctx, Source)
    end.

download(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, _} ->
            {ok, Ref} = bookshelf_store:obj_out_start(Bucket, Path, ?BLOCK_SIZE),
            {{stream, send_streamed_body(Ref)},
             Rq0,
             Ctx};
        _ ->
            {false, Rq0, Ctx}
    end.
%%===================================================================
%% Internal Functions
%%===================================================================
send_streamed_body(Ref) ->
    case bookshelf_store:obj_out(Ref) of
        {ok, Data1} ->
            {Data1, fun() -> send_streamed_body(Ref) end};
        done ->
            {<<>>, done};
        Error = {error, _} ->
            Error
    end.

halt(Code, Rq, Ctx) ->
    {{halt, Code}, Rq, Ctx}.

upload(Rq0, Ctx) ->
    {ok, Bucket, Path} = bksw_util:get_object_and_bucket(Rq0),
    {ok, Ref} = bookshelf_store:obj_in_start(Bucket, Path),
    get_streamed_body(wrq:stream_req_body(Rq0, ?BLOCK_SIZE),
                      Ref, Rq0, Ctx).

get_streamed_body({Data, done}, Ref, Rq0, Ctx) ->
    ok = bookshelf_store:obj_in(Ref, Data),
    case bookshelf_store:obj_in_end(Ref) of
        {ok, Digest} ->
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
        {error, Data} ->
            halt(500, Rq0, Ctx)
    end;
get_streamed_body({Data, Next}, Ref, Rq0, Ctx) ->
    ok = bookshelf_store:obj_in(Ref, Data),
    get_streamed_body(Next(), Ref, Rq0, Ctx).



copy(Rq0, Ctx, <<"/", FromFullPath/binary>>) ->
    {ok, ToBucket, ToPath} = bksw_util:get_object_and_bucket(Rq0),
    [FromBucket, FromPath] = binary:split(FromFullPath,
                                          <<"/">>),
    case bookshelf_store:obj_copy(bksw_util:to_binary(FromBucket),
                                  bksw_util:to_binary(FromPath),
                                  ToBucket, ToPath)
    of
        {ok, _} ->
            {true, Rq0, Ctx};
        _ ->
            {false, Rq0, Ctx}
    end.

get_header(Header, Rq) ->
    wrq:get_req_header(Header, Rq).
