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

-module(bookshelf_fs).
-include("bookshelf.hrl").
-export([
         bucket_list/1,
         bucket_exists/2,
         bucket_create/2,
         bucket_delete/2,
         obj_list/2,
         obj_exists/3,
         obj_delete/3,
         obj_meta/3,
         obj_open_w/3,
         obj_write/2,
         obj_close/1,
         obj_copy/5,
         obj_send/5,
        ]).

%% ===================================================================
%%                         Bucket functions
%% ===================================================================

bucket_list(Dir) ->
    {ok, Files} = file:list_dir(Dir), %% crash if no access to base dir
    lists:map(fun(P) -> %% crash if no access to any bucket dir
                      {ok, #file_info{ctime=Date}} =
                          file:read_file_info(P, [{time, universal}]),
                      #bucket{ name=filename:basename(P),
                               date=iso8601:format(Date) }
              end,
              lists:filter(fun filelib:is_dir/1,
                           lists:map(fun(F) ->
                                             filename:join(Dir, F)
                                     end,
                                     Files))).

bucket_exists(Dir, Bucket) ->
    filelib:is_dir(filename:join(Dir, Bucket)).

bucket_create(Dir, Bucket) ->
    file:make_dir(filename:join(Dir, Bucket)).

bucket_delete(Dir, Bucket) ->
    file:del_dir(filename:join(Dir, Bucket)).

%% ===================================================================
%%                         Object functions
%% ===================================================================

obj_list(Dir, Bucket) when is_list(Dir) ->
    obj_list(list_to_binary(Dir), Bucket);
obj_list(Dir, Bucket) when is_list(Bucket) ->
    obj_list(Dir, list_to_binary(Bucket));
obj_list(Dir, Bucket) when is_binary(Dir) andalso is_binary(Bucket) ->
    BucketPath = filename:join(Dir, Bucket),
    filelib:fold_files(
      BucketPath,
      ".*",
      true,
      fun(FilePath, Acc) ->
              case filelib:is_regular(FilePath) of
                  true ->
                      case file:read_file_info(FilePath,
                                               [{time, universal}]) of
                          {ok, #file_info{size=Size, mtime=Date}} ->
                              Pos = byte_size(FilePath),
                              Len = byte_size(BucketPath) + 1
                                  - byte_size(FilePath),
                              Name = binary:part(FilePath, Pos, Len),
                              lists:append(
                                Acc,
                                [#object{
                                    name=Name,
                                    date=iso8601:format(Date),
                                    size=Size
                                   }]
                               );
                          _                                       -> Acc
                      end;
                  _    -> Acc
              end
      end,
      []
     ).

obj_exists(Dir, Bucket, Path) when is_list(Dir) ->
    obj_exists(list_to_binary(Dir), Bucket, Path);
obj_exists(Dir, Bucket, Path) when is_list(Bucket) ->
    obj_exists(Dir, list_to_binary(Bucket), Path);
obj_exists(Dir, Bucket, Path) when is_list(Path) ->
    obj_exists(Dir, Bucket, list_to_binary(Path));
obj_exists(Dir, Bucket, Path)
  when is_binary(Dir) andalso is_binary(Bucket) andalso is_binary(Path) ->
    filelib:is_regular(filename:join([Dir, Bucket, Path])).

obj_delete(Dir, Bucket, Path) when is_list(Dir) ->
    obj_delete(list_to_binary(Dir), Bucket, Path);
obj_delete(Dir, Bucket, Path) when is_list(Bucket) ->
    obj_delete(Dir, list_to_binary(Bucket), Path);
obj_delete(Dir, Bucket, Path) when is_list(Path) ->
    obj_delete(Dir, Bucket, list_to_binary(Path));
obj_delete(Dir, Bucket, Path)
  when is_binary(Dir) andalso is_binary(Bucket) andalso is_binary(Path) ->
    ObjectPath = filename:join([Dir, Bucket, Path]),
    file:delete(ObjectPath).

obj_meta(Dir, Bucket, Path) ->
    %% FIXME TEMPORARY inefficient (non-cached) MD5 sum
    Filename = filename:join([Dir, Bucket, Path]),
    case file:open(Filename, [binary,raw,read_ahead]) of
        {ok, File} ->
            case file_md5(File, erlang:md5_init()) of
                {ok, Md5} ->
                    case file:read_file_info(Filename) of
                        {ok, #file_info{mtime=Date, size=Size}} ->
                            {ok, #object{name=Path,
                                         date=Date,
                                         size=Size,
                                         digest=Md5}};
                        Any -> Any
                    end;
                Any -> Any
            end;
        Any -> Any
    end.

file_md5(File, Ctx) ->
    case file:read(File, ?BLOCK_SIZE) of
        {ok, Bin} ->
            file_md5(File, erlang:md5_update(Ctx, Bin));
        eof ->
            file:close(File),
            {ok, erlang:md5_final(Ctx)}
    end.

obj_open(Dir, Bucket, Path, Opts) ->
    case file:open(filename:join([Dir, Bucket, Path]), Opts) of
        {ok, File} -> {ok, {File, erlang:md5_init()}};
        Any        -> Any
    end.

obj_open_w(Dir, Bucket, Path) ->
    obj_open(Dir, Bucket, Path, [raw, binary, write]).

obj_write({File, Ctx}, Chunk) ->
    case file:write(File, Chunk) of
        ok  -> {ok, {File, erlang:md5_update(Ctx, Chunk)}};
        Any -> Any
    end.

obj_close({File, Ctx}) ->
    case file:close(File) of
        ok  -> {ok, erlang:md5_final(Ctx)};
        Any -> Any
    end.

obj_copy(Dir, FromBucket, FromPath, ToBucket, ToPath) ->
    file:copy(filename:join([Dir, FromBucket, FromPath]),
              filename:join([Dir, ToBucket, ToPath])).

obj_send(Dir, Bucket, Path, _Transport, Socket) ->
    file:sendfile(filename:join([Dir, Bucket, Path]), Socket).
