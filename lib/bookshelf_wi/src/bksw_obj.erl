%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tim Dysinger <dysinger@opscode.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_obj).

-export([allowed_methods/2, content_types_accepted/2,
         content_types_provided/2, delete_resource/2, download/2,
         generate_etag/2, init/3, last_modified/2,
         resource_exists/2, rest_init/2, upload_or_copy/2]).

-include_lib("bookshelf_store/include/bookshelf_store.hrl").
-include_lib("cowboy/include/http.hrl").
-include("internal.hrl").

%%===================================================================
%% Public API
%%===================================================================

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Rq, _Opts) ->
    %% We dont actually make use of state here at all
    {ok, Rq, undefined}.

allowed_methods(Rq, St) ->
    {['HEAD', 'GET', 'PUT', 'DELETE'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"*">>, <<"*">>, []}, download}], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{'*', upload_or_copy}], Rq, St}.

resource_exists(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    {bookshelf_store:obj_exists(Bucket, Path), Rq2, St}.

delete_resource(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    case bookshelf_store:obj_delete(Bucket, Path) of
        ok ->
            {true, Rq2, St};
        _ ->
            {false, Rq2, St}
    end.

last_modified(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{date = Date}} ->
            {Date, Rq2, St};
        _ ->
            {halt, Rq2, St}
    end.

generate_etag(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{digest = Digest}} ->
            {{strong,
              list_to_binary(bksw_format:to_hex(Digest))},
             Rq2, St};
        _ ->
            {halt, Rq2, St}
    end.

upload_or_copy(Rq, St) ->
    case cowboy_http_req:parse_header(<<"X-Amz-Copy-Source">>,
                                     Rq) of
        {undefined, undefined, Rq2} ->
            upload(Rq2, St);
        {undefined, Rq2} ->
            upload(Rq2, St);
        {Source, Rq2} ->
            copy(Rq2, St, Source)
    end.

download(Rq0, St) ->
    {ok, Transport, Socket} = cowboy_http_req:transport(Rq0),
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),

    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{size = Size}} ->
            SFun = fun () ->
                           Bridge = bkss_transport:new(bksw_socket_transport,
                                                       [Transport, Socket,
                                                        ?TIMEOUT_MS]),
                           case bookshelf_store:obj_send(Bucket, Path, Bridge) of
                               {ok, Size} ->
                                   sent;
                               _ ->
                                   {error, "Download unsuccessful"}
                           end
                   end,
            {{stream, Size, SFun}, Rq2, St};
        _ ->
            {false, Rq2, St}
    end.

%%===================================================================
%% Internal Functions
%%===================================================================
get_object_name(Rq0) ->
    case cowboy_http_req:path_info(Rq0) of
        {undefined, _} ->
            erlang:error(missing_path_info);
        {Path, Rq1} ->
            %% Paths are always one name joined by a path seperator. Cowboy splits them up
            %% but we don t actually want that.
            {filename:join(Path), Rq1}
    end.


halt(Code, Rq, St) ->
    {ok, Rq2} = cowboy_http_req:reply(Code, Rq),
    {halt, Rq2, St}.

%% This direct access to the body really needs to go away. This is an cowboy internal
%% streaming protocol that is undocumented and subject to change. There is a set of
%% cowboy_http_req:stream_body apis that should probably be used instead.
upload(#http_req{body_state = waiting,
                 buffer = Buffer} = Rq0, St) ->
    {ok, Transport, Socket} = cowboy_http_req:transport(Rq0),
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    {Length, Rq3} =
        cowboy_http_req:parse_header('Content-Length', Rq2),
    Bridge = bkss_transport:new(bksw_socket_transport,
                                [Transport, Socket, ?TIMEOUT_MS]),
    case bookshelf_store:obj_recv(Bucket, Path, Bridge,
                                  Buffer, Length)
    of
        {ok, Digest} ->
            OurMd5 = bksw_format:to_hex(Digest),
            case cowboy_http_req:parse_header('Content-MD5', Rq3) of
                {undefined, undefined, Rq4} ->
                    Rq5 = bksw_req:with_etag(OurMd5, Rq4),
                    halt(202, Rq5, St);
                {undefined, Rq4} ->
                    Rq5 = bksw_req:with_etag(OurMd5, Rq4),
                    halt(202, Rq5, St);
                {RequestMd5, Rq4} ->
                    case RequestMd5 =:= OurMd5 of
                        true ->
                            Rq5 = bksw_req:with_etag(RequestMd5, Rq4),
                            halt(202, Rq5, St);
                        _ ->
                            halt(406, Rq4, St)
                    end
            end;
        {error, timeout} ->
            halt(408, Rq3, St);
        _ ->
            halt(500, Rq3, St)
    end.

copy(Rq0, St, <<"/", FromFullPath/binary>>) ->
    {ToBucket, Rq1} = bksw_util:get_bucket(Rq0),
    {ToPath, Rq2} = get_object_name(Rq1),
    [FromBucket, FromPath] = binary:split(FromFullPath,
                                          <<"/">>),
    case bookshelf_store:obj_copy(FromBucket, FromPath,
                                  ToBucket, ToPath)
    of
        {ok, _} ->
            {true, Rq2, St};
        _ ->
            {false, Rq2, St}
    end.
