%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Dan Deleo <dan@chef.io>
%% Copyright 2011-2018 Chef Software, Inc.
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


-module(chef_deep_merge).

-export([merge/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DEEP MERGING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Based on the Ruby deep_merge: https://github.com/danielsdeleo/deep_merge/
%
% Ruby deep_merge supports a lot of options that modify the behavior for
% tricky cases such as merging arrays of Hashes, etc. In Chef, we use the
% default behaviors, so this library only implements the default deep_merge
% behaviors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% About the ejson format:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% patterns:
%% { KVList } :: One element tuple with a list of KV duples. JSON 'object'
%% <<"BinString"">> :: string as binary. JSON string.
%% [] :: List. JSON Array.

-spec merge(any(), any()) -> any().
%% Both items are JSON Objects:
merge({Mergee}, {Other}) ->
  MergeeDict = dict:from_list(Mergee),
  OtherDict = dict:from_list(Other),
  Merged = dict:merge(fun merge_dict/3, MergeeDict, OtherDict),
  %% make sure to return the merged object in ejson-y format.
  {dict:to_list(Merged)};
%% Items are JSON strings or literals (null, true, false), or of incompatible
%% types:
merge(_Mergee, Other) ->
  Other. %%OVERWRITE_UNMERGABLES

-spec merge_dict(binary(), any(), any()) -> any().
%% Merge function suitable for dict:merge/3. Makes the deep merging magic
%% happen.
merge_dict(_Key, Mergee, Other) ->
  merge(Mergee, Other).
