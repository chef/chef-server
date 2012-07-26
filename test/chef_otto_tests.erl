%%%
%%% License:: Apache License, Version 2.0
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @copyright Copyright 2011 Opscode, Inc.
%%% @version 0.0.2
%%% @end
-module(chef_otto_tests).

-include("chef_types.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(superuser_name,  <<"platform-superuser">>).
-define(no_such_id, <<"deadbeefdeadbeefdeadbeefdeadbeef">>).
-define(authz_host, "http://localhost:5959").
-define(chef_host_name, "localhost").
-define(chef_host_port, 5984).

-define(AUTOMECK_FILE(TestName), filename:join(["..", "test", "automeck_config",
                                                atom_to_list(?MODULE) ++ "_" ++ atom_to_list(TestName) ++
                                                    ".config"])).


fetch_by_name_test_() ->
    {foreach,
     fun() -> test_utils:test_setup() end,
     fun(_) -> meck:unload() end,
     [fun({Server, _AS}) ->
              [{"check fetch_by_name basic behavior",
                fun() -> ok
                        %% OrgId = chef_otto:fetch_org_id(Server, <<"clownco">>),
                        %% {ok, NodesContainer} = chef_otto:fetch_by_name(Server, OrgId, <<"nodes">>, authz_container),
                        %% ExpectKeys = [<<"_id">>, <<"containername">>, <<"couchrest-type">>,
                        %%               <<"requester_id">>],
                        %% [ ?assert(is_binary(proplists:get_value(Key, NodesContainer))) ||
                        %%     Key <- ExpectKeys ]
                end}]
      end]}.

%
% This is totally janky, because I don't have a test framework in place to create nodes, nor do
% I want one right now
fetch_node_test_() ->
    [fun() -> ?debugMsg("!!! TODO: Verify SQL nodes ops have good test coverage !!!\n\n\n"), ok end].
    %% {foreach,
    %%  fun() ->
        %%  AS = test_utils:automeck_setup(?automeck_state, ?MODULE, fetch_node,
        %%                              ?automeck_config_dir, ?automeck_config),
        %%  Server = test_setup(),
        %%  {Server, AS}
    %%  end,
    %%  fun({_Server, AS}) ->
        %%  test_utils:automeck_cleanup(?automeck_state, AS),
        %%  stopping
    %%  end,
    %%  [fun({Server, _AS}) ->
        %%       [{"check fetch_node basic behavior",
    %%             fun() ->
        %%              OrgId = chef_otto:fetch_org_id(Server, <<"clownco">>),
        %%              Node1 = chef_otto:fetch_node(Server, OrgId, "i-cleanup"),
        %%              ?assert(is_record(Node1, chef_node))
    %%             end}
        %%       ]
    %%   end
    %%  ]
    %% }.

%
% This is totally janky, because I don't have a test framework in place to create nodes, nor do
% I want one right now
fetch_nodes_test_() ->
    [fun() -> ?debugMsg("!!! TODO: Verify SQL nodes ops have good test coverage !!!\n\n\n"), ok end].
    %% {foreach,
    %% fun() ->
        %%  AS = test_utils:automeck_setup(?automeck_state, ?MODULE, fetch_nodes,
        %%                              ?automeck_config_dir, ?automeck_config),
        %%  Server = test_setup(),
        %%  {Server, AS}
    %%  end,
    %%  fun({_Server, AS}) ->
        %%  test_utils:automeck_cleanup(?automeck_state, AS),
        %%  stopping
    %%  end,
    %%  [fun({Server, _AS}) ->
        %%       [{"check fetch_node basic behavior",
    %%             fun() ->
        %%              OrgId = chef_otto:fetch_org_id(Server, <<"clownco">>),
        %%              Nodes = chef_otto:fetch_nodes(Server, OrgId),
        %%              ?assertEqual(Nodes,
        %%                           [<<"i-03dd7d6f">>,<<"i-03dd7d6f-cleanup">>,<<"i-07b22a66">>,
        %%                            <<"i-cleanup">>])
    %%             end}
        %%       ]
    %%   end
    %%  ]
    %% }.


test_setup() ->
    ok = ensure_ibrowse(),
    application:set_env(chef_common, authz_host, ?authz_host),
    chef_otto:connect(?chef_host_name, ?chef_host_port).

ensure_ibrowse() ->
    case ibrowse:start() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        Error ->
            Error
    end.
