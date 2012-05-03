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

-module(bookshelf_env).
-include("bookshelf.hrl").
-export([
         initialize/0,
         with_ip/1,
         with_dispatch/1,
         with_dir/1,
         with_pool/1
        ]).

%% ===================================================================
%%                          API functions
%% ===================================================================
initialize() ->
    with_dispatch(with_dir(with_pool(with_ip(application:get_all_env(bookshelf))))).

with_ip(Env) ->
    case lists:keyfind(interface, 1, Env) of
        {_, Interface} ->
            lists:keystore(ip, 1, Env, {ip, ip(Interface)});
        _              -> Env
    end.

with_dispatch(Env) ->
    case lists:keyfind(domains, 1, Env) of
        {_, Domains} ->
            lists:keystore(dispatch, 1, Env,
                           {dispatch, rules(Env, Domains)});
        _            -> Env
    end.

with_dir(Env) ->
    case lists:keyfind(dir, 1, Env) of
        false         -> priv_dir(Env);
        {_, priv_dir} -> priv_dir(Env);
        _             -> Env
    end.

with_pool(Env) ->
    case lists:keyfind(pool, 1, Env) of
        false -> lists:keystore(pool, 1, Env, {pool, 100});
        _     -> Env
    end.

%% ===================================================================
%%                        Internal functions
%% ===================================================================

ip(Interface) ->
    {ok, All}    = inet:getifaddrs(),
    {_, Attribs} = lists:keyfind(Interface, 1, All),
    Addrs        = lists:filter(fun(Attr) ->
                                        case Attr of
                                            {addr, {_,_,_,_}} -> true;
                                            _                 -> false
                                        end
                                end,
                                Attribs),
    [{addr, Addr}|_] = Addrs,
    Addr.

rules(Env, Domains) ->
    lists:flatten(lists:map(fun(D) -> rule(Env, D) end,
                            lists:map(fun bdomain/1, Domains))).

rule(Env, Domain) ->
    SubDomain = lists:append([bucket], Domain),
    FEnv = filter_env(Env),
    [
     {Domain,    [{[],      bookshelf_idx, FEnv}]},
     {SubDomain, [{[],      bookshelf_bkt, FEnv},
                  {['...'], bookshelf_obj, FEnv}]}
    ].

filter_env(Env) ->
    lists:filter(fun(A) ->
                         case A of
                             {dispatch, _} -> false;
                             _             -> true
                         end
                 end,
                 Env).

bdomain(Domain) ->
    lists:map(fun list_to_binary/1, string:tokens(Domain, ".")).

priv_dir(Env) ->
    lists:keystore(dir, 1, Env, {dir, ?file("data")}).

%% ===================================================================
%%                          Eunit Tests
%% ===================================================================
-ifndef(NO_TESTS).
-include_lib("eunit/include/eunit.hrl").
with_ip_test_() ->
    [{"should configure the listen ip address if the env has an 'interface'",
      fun() ->
              Env = with_ip([{interface, "lo"}]),
              ?assertMatch({ip, {127,0,0,1}}, lists:keyfind(ip, 1, Env))
      end
     }].

with_dispatch_test_() ->
    [{"should build proper 'cowboy' dispatch rules using env 'domains'",
      fun() ->
              EnvV1 = [{domains, ["clown.com", "school.com"]}],
              EnvV2 = with_dispatch(EnvV1),
              ?assertMatch({dispatch,
                            [{[<<"clown">>, <<"com">>],
                              [{[], bookshelf_idx, EnvV1}]},
                             {[bucket, <<"clown">>, <<"com">>],
                              [{[], bookshelf_bkt, EnvV1},
                               {['...'], bookshelf_obj, EnvV1}]},
                             {[<<"school">>, <<"com">>],
                              [{[], bookshelf_idx, EnvV1}]},
                             {[bucket, <<"school">>, <<"com">>],
                              [{[], bookshelf_bkt, EnvV1},
                               {['...'], bookshelf_obj, EnvV1}]}]},
                           lists:keyfind(dispatch, 1, EnvV2))
      end
     }].

with_dir_test_() ->
    [{"should use any env 'dir' if provided",
      fun() ->
              ?assertEqual([{dir, "/tmp"}], with_dir([{dir, "/tmp"}]))
      end
     },
     {"should use ${priv_dir}/data/ if env 'dir' is the atom 'priv_dir'",
      fun() ->
              ?assertEqual([{dir, ?file("data")}],
                           with_dir([{dir, priv_dir}]))
      end
     },
     {"should use ${priv_dir}/data/ if env 'dir' is absent",
      fun() ->
              ?_assertEqual([{dir, ?file("data")}], with_dir([]))
      end
     }
    ].
-endif.
