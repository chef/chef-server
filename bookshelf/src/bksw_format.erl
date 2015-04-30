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
-module(bksw_format).

-export([to_base64/1,
         to_date/1,
         to_etag/1,
         to_hex/1]).

%%===================================================================
%% API functions
%%===================================================================
to_date(Date) ->
    iso8601:format(Date).

to_base64(Bin) ->
    base64:encode_to_string(Bin).

to_hex(Bin) ->
      string:to_lower(lists:flatten([io_lib:format("~2.16.0b",
                                                   [N])
                                     || <<N>> <= Bin])).

to_etag(Tag) when is_binary(Tag) ->
    to_etag(to_hex(Tag));
to_etag(Tag) ->
    io_lib:format("\"~s\"", [Tag]).
