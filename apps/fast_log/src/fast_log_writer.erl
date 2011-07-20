%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author Kevin Smith <kevin@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.1
% @end
-module(fast_log_writer).

-include("fast_log.hrl").

-export([open/4,
         write/3]).

-spec(open(string(), string(), pos_integer(), pos_integer()) ->
             {ok, #continuation{}} | {error, any()}).
open(Name, FileName, MaxFiles, MaxFileSize) ->
    disk_log:open([{name, Name},
                   {file, FileName},
                   {size, {MaxFileSize * 1024 * 1024, MaxFiles}},
                   {type, wrap},
                   {format, external}]).

-spec(write(#continuation{}, atom(), string()) -> ok).
write(Log, LogLevel0, Output) ->
    Timestamp = fast_log_util:time_iso8601(),
    Node = atom_to_list(node()),
    LogLevel = fast_log_util:log_level(LogLevel0),
    Prefix = io_lib:format("~s ~s ~s ", [Timestamp, Node, LogLevel]),
    Msg = iolist_to_binary([Prefix, Output, $\n]),
    disk_log:blog(Log, Msg).
