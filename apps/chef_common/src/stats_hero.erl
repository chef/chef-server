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
% @author John Keiser <jkeiser@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @doc Helper module for calling various Chef REST endpoints
% @end
-module(stats_hero).

-export([send/3]).

send(Server, Port, Metrics) ->
    send_payload(Server, Port, [ make_metric_line(Key, Value, Type) || {Key, Value, Type} <- Metrics ]).

send_payload(Server, Port, Payload) ->
    Length = iolist_size(Payload),
    Packet = io_lib:format("1|~B~n~s", [Length, Payload]),
    {ok, Socket} = gen_udp:open(0),
    try
        ok = gen_udp:send(Socket, Server, Port, Packet)
    after
        gen_udp:close(Socket)
    end.

make_metric_line(Key, Value, Type) when is_integer(Value) ->
    io_lib:format("~s:~B|~s~n", [Key, Value, Type]);
make_metric_line(Key, Value, Type) when is_float(Value) ->
    io_lib:format("~s:~f|~s~n", [Key, Value, Type]).
