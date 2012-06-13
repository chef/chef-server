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

resource_exists(#http_req{host = [Bucket | _],
                          raw_path = <<"/", Path/binary>>} = Rq,
                St) ->
    {bookshelf_store:obj_exists(Bucket, Path), Rq, St}.

delete_resource(#http_req{host = [Bucket | _],
                          raw_path = <<"/", Path/binary>>} = Rq,
                St) ->
    case bookshelf_store:obj_delete(Bucket, Path) of
        ok ->
            {true, Rq, St};
        _ ->
            {false, Rq, St}
    end.

last_modified(#http_req{host = [Bucket | _],
                        raw_path = <<"/", Path/binary>>} = Rq,
              St) ->
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{date = Date}} ->
            {Date, Rq, St};
        _ ->
            {halt, Rq, St}
    end.

generate_etag(#http_req{host = [Bucket | _],
                        raw_path = <<"/", Path/binary>>} = Rq,
              St) ->
    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{digest = Digest}} ->
            {{strong,
              list_to_binary(bksw_format:to_hex(Digest))},
             Rq, St};
        _ ->
            {halt, Rq, St}
    end.

upload_or_copy(Rq, St) ->
    case
        cowboy_http_req:parse_header(<<"X-Amz-Copy-Source">>,
                                     Rq)
    of
        {undefined, Rq2} ->
            upload(Rq2, St);
        {Source, Rq2} ->
            copy(Rq2, St, Source)
    end.

download(#http_req{host = [Bucket | _],
                   raw_path = <<"/", Path/binary>>, transport = Transport,
                   socket = Socket} = Rq,
         St) ->
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
            {{stream, Size, SFun}, Rq, St};
        _ ->
            {false, Rq, St}
    end.

%%===================================================================
%% Internal Functions
%%===================================================================

halt(Code, Rq, St) ->
    {ok, Rq2} = cowboy_http_req:reply(Code, Rq),
    {halt, Rq2, St}.

upload(#http_req{host = [Bucket | _],
                 raw_path = <<"/", Path/binary>>, body_state = waiting,
                 socket = Socket, transport = Transport,
                 buffer = Buffer} = Rq,
       St) ->
    {Length, Rq2} =
        cowboy_http_req:parse_header('Content-Length', Rq),
    Bridge = bkss_transport:new(bksw_socket_transport,
                                [Transport, Socket, ?TIMEOUT_MS]),
    case bookshelf_store:obj_recv(Bucket, Path, Bridge,
                                  Buffer, Length)
    of
        {ok, Digest} ->
            OurMd5 = bksw_format:to_hex(Digest),
            case cowboy_http_req:parse_header('Content-MD5', Rq2) of
                {undefined, Rq3} ->
                    Rq4 = bksw_req:with_etag(OurMd5, Rq3),
                    halt(202, Rq4, St);
                {RequestMd5, Rq3} ->
                    case RequestMd5 =:= OurMd5 of
                        true ->
                            Rq4 = bksw_req:with_etag(RequestMd5, Rq3),
                            halt(202, Rq4, St);
                        _ ->
                            halt(406, Rq3, St)
                    end
            end;
        {error, timeout} ->
            halt(408, Rq2, St);
        _ ->
            halt(500, Rq2, St)
    end.

copy(#http_req{host = [ToBucket | _],
               raw_path = <<"/", ToPath/binary>>} =
         Rq,
     St, <<"/", FromFullPath/binary>>) ->
    [FromBucket, FromPath] = binary:split(FromFullPath,
                                          <<"/">>),
    case bookshelf_store:obj_copy(FromBucket, FromPath,
                                  ToBucket, ToPath)
    of
        {ok, _} ->
            {true, Rq, St};
        _ ->
            {false, Rq, St}
    end.
