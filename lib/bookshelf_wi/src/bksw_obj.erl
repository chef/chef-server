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
            {{strong, Digest}, Rq2, St};
        _ ->
            {halt, Rq2, St}
    end.

upload_or_copy(Rq0, St) ->
    {ok, Rq1} = cowboy_http_req:set_resp_header(<<"Obj">>, <<"Obj">>, Rq0),
    case cowboy_http_req:parse_header(<<"X-Amz-Copy-Source">>,
                                      Rq1) of
        {undefined, undefined, Rq2} ->
            upload(Rq2, St);
        {undefined, Rq2} ->
            upload(Rq2, St);
        {Source, Rq2} ->
            copy(Rq2, St, Source)
    end.

download(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),

    case bookshelf_store:obj_meta(Bucket, Path) of
        {ok, #object{size = Size}} ->
            SFun = fun () ->
                           Bridge = bkss_transport:new(bksw_socket_transport, Rq2),
                           case bookshelf_store:obj_send(Bucket, Path, Bridge) of
                               {_Rq2, {ok, Size}} ->
                                   sent;
                               {_Rq2, _} ->
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

upload(Rq0, St) ->
    {Bucket, Rq1} = bksw_util:get_bucket(Rq0),
    {Path, Rq2} = get_object_name(Rq1),
    {Length, Rq3} =
        cowboy_http_req:parse_header('Content-Length', Rq2),
    Bridge = bkss_transport:new(bksw_socket_transport, Rq3),
    case bookshelf_store:obj_recv(Bucket, Path, Bridge, <<>>, Length) of
        {Rq4, {ok, Digest}} ->
            OurMd5 = bksw_format:to_hex(Digest),
            case cowboy_http_req:parse_header('Content-MD5', Rq4) of
                {undefined, undefined, Rq5} ->
                    Rq6 = bksw_req:with_etag(OurMd5, Rq5),
                    halt(202, Rq6, St);
                {undefined, Rq5} ->
            error_logger:error_msg("A3:~s", [Path]),
                    Rq6 = bksw_req:with_etag(OurMd5, Rq5),
                    halt(202, Rq6, St);
                {RequestMd5, Rq5} ->
            error_logger:error_msg("A4:~s", [Path]),
                    case RequestMd5 =:= OurMd5 of
                        true ->
                            Rq6 = bksw_req:with_etag(RequestMd5, Rq5),
                            halt(202, Rq6, St);
                        _ ->
                            halt(406, Rq5, St)
                    end
            end;
        {Rq4, {error, timeout}} ->
            error_logger:error_msg("A5"),
            halt(408, Rq4, St);
        {Rq4, _} ->
            error_logger:error_msg("A6"),
            halt(500, Rq4, St)
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
