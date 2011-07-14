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
% @version 0.0.2
% @end
-module(chef_common_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ConfigFile = filename:absname(filename:join(["etc", "ibrowse", "ibrowse.config"])),
    error_logger:info_msg("Loading ibrowse configuration from ~s~n", [ConfigFile]),
    ok = ibrowse:rescan_config(ConfigFile),
    chef_common_sup:start_link().

stop(_State) ->
    ok.
