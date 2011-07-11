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
-module(chef_keyring).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_key/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {keys=dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_key(KeyName) ->
    gen_server:call(?SERVER, {get_key, KeyName}, infinity).

init([]) ->
    {ok, KeyRing0} = application:get_env(chef_common, keyring),
    case init_keyring(KeyRing0) of
        {ok, KeyRing} ->
            {ok, #state{keys=KeyRing}};
        Error ->
            {stop, Error}
    end.

handle_call({get_key, KeyName}, _From, #state{keys=Keys}=State) ->
    Reply = case dict:find(KeyName, Keys) of
                {ok, RawKey} ->
                    {ok, RawKey};
                error ->
                    {error, unknown_key}
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
init_keyring(KeyRing) ->
    init_keyring(KeyRing, dict:new()).

init_keyring([], Keys) ->
    {ok, Keys};
init_keyring([{Name, Path}|T], Keys) ->
    case file:read_file(Path) of
        {ok, RawKey} ->
            init_keyring(T, dict:store(Name, RawKey, Keys));
        Error ->
            error_logger:error_msg("Error loading key '~p' on path ~p: ~p~n", [Name, Path, Error]),
            Error
    end.

