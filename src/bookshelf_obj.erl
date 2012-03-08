%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <dysinger@opscode.com>
%%
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed
%% with this work for additional information regarding copyright
%% ownership.  The ASF licenses this file to you under the Apache
%% License, Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain a copy of
%% the License at http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
%% implied.  See the License for the specific language governing
%% permissions and limitations under the License.

-module(bookshelf_obj).
-include("bookshelf.hrl").
-compile(export_all).

%% ===================================================================
%%                              Cowboy
%% ===================================================================

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

%% ===================================================================
%%                         Cowboy HTTP REST
%% ===================================================================

rest_init(Rq, Opts) ->
    {dir, Dir} = lists:keyfind(dir, 1, Opts),
    {ok, bookshelf_req:with_amz_request_id(Rq), #state{dir = Dir}}.

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"*">>, <<"*">>, []}, download}], Rq, St}.

content_types_accepted(Rq, St) ->
    {[{'*', upload_or_copy}], Rq, St}.

resource_exists(#http_req{host=[Bucket|_],
                          raw_path= <<"/",Path/binary>>}=Rq,
                #state{dir=Dir}=St) ->
    {?BACKEND:obj_exists(Dir, Bucket, Path), Rq, St}.

delete_resource(#http_req{host=[Bucket|_],
                          raw_path= <<"/",Path/binary>>}=Rq,
                #state{dir=Dir}=St) ->
    case ?BACKEND:obj_delete(Dir, Bucket, Path) of
        ok -> {true, Rq, St};
        _  -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Accepted
%% ===================================================================

upload_or_copy(Rq, St) ->
    io:fwrite("Upload or Copy?", []),
    case cowboy_http_req:parse_header(<<"X-Amz-Copy-Source">>, Rq) of
        {undefined, undefined, Rq2} ->
            io:fwrite(" Upload!~n", []),
            io:fwrite("Rq~p~n", [Rq2]),
            upload(Rq2, St);
        {undefined, Source, Rq2} ->
            io:fwrite(" Copy!~n", []),
            copy(Rq2, St, Source)
    end.

upload(#http_req{host=[Bucket|_],
                 raw_path= <<"/",Path/binary>>,
                 body_state=waiting,
                 socket=Sock,
                 transport=Trans,
                 buffer=Buf}=Rq,
       #state{dir=Dir}=St) ->
    {Len, Rq2} = cowboy_http_req:parse_header('Content-Length', Rq),
    case ?BACKEND:obj_open_w(Dir, Bucket, Path) of
        {ok, FsSt} ->
            case write(FsSt, Trans, Sock, Len, Buf) of
                {ok, FsSt2} ->
                    {ok, Digest} = ?BACKEND:obj_close(FsSt2),
                    Etag = bookshelf_req:to_hex(Digest),
                    case cowboy_http_req:parse_header('Content-MD5', Rq2) of
                        {undefined, undefined, Rq3} ->
                            halt(202, bookshelf_req:with_etag(Etag, Rq3), St);
                        {Md5, Rq3} ->
                            case Md5 =:= Etag of
                                true ->
                                    halt(202,
                                         bookshelf_req:with_etag(Etag, Rq3),
                                         St);
                                false ->
                                    ?BACKEND:obj_close(FsSt2),
                                    ?BACKEND:obj_delete(Dir, Bucket, Path),
                                    halt(406, Rq3, St)
                            end
                    end;
                {error, timeout} ->
                    ?BACKEND:obj_close(FsSt),
                    ?BACKEND:obj_delete(Dir, Bucket, Path),
                    halt(408, Rq, St);
                _ ->
                    ?BACKEND:obj_close(FsSt),
                    ?BACKEND:obj_delete(Dir, Bucket, Path),
                    halt(500, Rq, St)
            end;
        _ ->
            halt(500, Rq, St)
    end.

copy(#http_req{host=[ToBucket|_], raw_path= <<"/",ToPath/binary>>}=Rq,
     #state{dir=Dir}=St,
     <<"/",FromFullPath/binary>>) ->
    [FromBucket, FromPath] = binary:split(FromFullPath, <<"/">>),
    case ?BACKEND:obj_copy(Dir, FromBucket, FromPath, ToBucket, ToPath) of
        {ok, _} -> {true, Rq, St};
        _       -> {false, Rq, St}
    end.

%% ===================================================================
%%                         Content Provided
%% ===================================================================

download(#http_req{host=[Bucket|_], raw_path= <<"/",Path/binary>>}=Rq,
         #state{dir=Dir} = St) ->
    case ?BACKEND:obj_meta(Dir, Bucket, Path) of
        {ok, #object{name=_Name, date=_Date, size=_Size}} ->
            %% TODO Add object md5 to the types.hrl
            %% TODO Set the Size header
            %% TODO set the Etag header
            {ok, Transport, Socket} = cowboy_http_req:transport(Rq),
            case ?BACKEND:obj_open_w(Dir, Bucket, Path) of
                {ok, FsSt} ->
                    case read(FsSt, Transport, Socket) of
                        {ok, FsSt2} ->
                            {ok, _Digest} =
                                ?BACKEND:close(FsSt2),
                            %% TODO set the Md5 response Etag late?
                            halt(200, Rq, St);
                        _ ->
                            ?BACKEND:close(FsSt),
                            halt(500, Rq, St)
                    end;
                _ -> halt(500, Rq, St)
            end;
        _ -> halt(500, Rq, St)
    end.

%% ===================================================================
%%                        Internal Functions
%% ===================================================================

read(St, Transport, Socket) ->
    case ?BACKEND:obj_read(St) of
        {ok, NewSt, Chunk} ->
            Transport:send(Socket, Chunk),
            read(NewSt, Transport, Socket);
        Any -> Any
    end.

write(St, Trans, Sock, Len, <<>>) ->
    write(St, Trans, Sock, Len);
write(St, Trans, Sock, Len, Buf) ->
    case ?BACKEND:obj_write(St, Buf) of
        {ok, NewSt} -> write(NewSt, Trans, Sock, Len-byte_size(Buf));
        Any         -> Any
    end.
write(St, Trans, Sock, Len) when Len =< ?BLOCK_SIZE ->
    case Trans:recv(Sock, Len, ?TIMEOUT_MS) of
        {ok, Chunk} -> ?BACKEND:obj_write(St, Chunk);
        Any         -> Any
    end;
write(St, Trans, Sock, Len) ->
    case Trans:recv(Sock, ?BLOCK_SIZE, ?TIMEOUT_MS) of
        {ok, Chunk} ->
            case ?BACKEND:obj_write(St, Chunk) of
                {ok, NewSt} -> write(NewSt, Trans, Sock, Len-?BLOCK_SIZE);
                Any         -> Any
            end;
        Any -> Any
    end.

halt(Code, Rq, St) ->
    {ok, Rq2} = cowboy_http_req:reply(Code, Rq),
    {halt, Rq2, St}.
