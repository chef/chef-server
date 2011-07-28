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
-module(fast_log_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, I, Type, Config), {Name, {I, start_link, Config}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = build_children(application:get_env(fast_log, loggers)),
    {ok, { {one_for_one, 5, 10}, Children} }.

%% Internal functions
build_children({ok, Loggers}) ->
    build_children(Loggers, []);
build_children(_) ->
    [].

build_children([], Children) ->
    Children;
build_children([H|T], Children) ->
    Name = proplists:get_value(name, H),
    Child = ?CHILD(Name, fast_log, worker, [H]),
    build_children(T, [Child|Children]).
