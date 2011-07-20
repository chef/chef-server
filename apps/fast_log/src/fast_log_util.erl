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
-module(fast_log_util).

-include("fast_log.hrl").

-export([time_iso8601/0,
         time_iso8601/1,
         log_level/1,
         log_pri/1,
         should_log/2,
         get_config/1,
         put_config/2]).

%% @doc Converts Erlang time-tuple to iso8601 formatted date string.
%%
%% Example output looks like <<"2003-12-13T18:30:02Z">>
-spec(time_iso8601() -> string()).
time_iso8601() ->
    time_iso8601(calendar:universal_time()).

-spec(time_iso8601(erlang_time()) -> string()).
time_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    % Is there a way to build a binary straight away?
    Fmt = "~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    lists:flatten(io_lib:format(Fmt, [Year, Month, Day, Hour, Min, Sec])).

-spec(should_log(atom(), atom()) -> boolean()).
should_log(ConfigTableName, ReqLogLevel) ->
    #config{min_log=MinLog} = get_config(ConfigTableName),
    log_pri(ReqLogLevel) >= log_pri(MinLog).

-spec(log_level(atom()) -> string()).
log_level(?LOG_DEBUG) ->
    "DEBUG";
log_level(?LOG_INFO) ->
    "INFO";
log_level(?LOG_WARN) ->
    "WARN";
log_level(?LOG_ERR) ->
    "ERR";
log_level(_) ->
    "UNKNOWN".

-spec(log_pri(atom()) -> pos_integer()).
log_pri(?LOG_DEBUG) ->
    1;
log_pri(?LOG_INFO) ->
    2;
log_pri(?LOG_WARN) ->
    3;
log_pri(?LOG_ERR) ->
    4;
log_pri(_) ->
    0.

-spec(get_config(atom()) -> #config{}).
get_config(Name) ->
    [#config{}= Config] = ets:lookup(Name, config),
    Config.

-spec(put_config(atom(), #config{}) -> ok).
put_config(Name, Config) ->
    true = ets:insert(Name, Config),
    ok.
