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

-module(bksw_env).

-include("bookshelf.hrl").

-export([initialize/0, with_dir/1, with_dispatch/1,
         with_ip/1, with_pool/1]).

%%===================================================================
%%  API functions
%%===================================================================
initialize() ->
    with_dispatch(with_dir(with_pool(with_ip(application:get_all_env(bookshelf))))).

with_ip(Env) ->
    case lists:keyfind(interface, 1, Env) of
      {_, Interface} ->
          lists:keystore(ip, 1, Env, {ip, ip(Interface)});
      _ -> Env
    end.

with_dispatch(Env) ->
    case lists:keyfind(domains, 1, Env) of
      {_, Domains} ->
          lists:keystore(dispatch, 1, Env,
                         {dispatch, rules(Env, Domains)});
      _ -> Env
    end.

with_dir(Env) ->
    case lists:keyfind(dir, 1, Env) of
      false -> priv_dir(Env);
      {_, priv_dir} -> priv_dir(Env);
      _ -> Env
    end.

with_pool(Env) ->
    case lists:keyfind(pool, 1, Env) of
      false -> lists:keystore(pool, 1, Env, {pool, 100});
      _ -> Env
    end.

%% ===================================================================
%%                        Internal functions
%% ===================================================================

ip(Interface) ->
    {ok, All} = inet:getifaddrs(),
    {_, Attribs} = lists:keyfind(Interface, 1, All),
    Addrs = [V1 || V1 <- Attribs, ip_1(V1)],
    [{addr, Addr} | _] = Addrs,
    Addr.

ip_1(Attr) ->
    case Attr of
      {addr, {_, _, _, _}} -> true;
      _ -> false
    end.

rules(Env, Domains) ->
    lists:flatten([rules_1(V2, Env)
                   || V2 <- [bdomain(V1) || V1 <- Domains]]).

rules_1(D, Env) -> rule(Env, D).

rule(Env, Domain) ->
    SubDomain = [bucket] ++ Domain,
    FEnv = filter_env(Env),
    [{Domain, [{[], bookshelf_idx, FEnv}]},
     {SubDomain,
      [{[], bookshelf_bkt, FEnv},
       {['...'], bookshelf_obj, FEnv}]}].

filter_env(Env) -> [V1 || V1 <- Env, filter_env_1(V1)].

filter_env_1(A) ->
    case A of
      {dispatch, _} -> false;
      _ -> true
    end.

bdomain(Domain) ->
    [fun list_to_binary/1(V1)
     || V1 <- string:tokens(Domain, ".")].

priv_dir(Env) ->
    lists:keystore(dir, 1, Env,
                   {dir, bookshelf_util:file("data")}).
