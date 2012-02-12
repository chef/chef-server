%% @copyright 2012 Opscode, Inc. All Rights Reserved
%% @author Tim Dysinger <timd@opscode.com>
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
-export([with_ip/1, with_dispatch/1, with_dir/1]).

%% ===================================================================
%% API functions
%% ===================================================================

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

%% ===================================================================
%% Internal functions
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
    Ds = lists:map(fun bdomain/1, Domains),
    lists:flatten(lists:map(fun(D) -> rule(Env, D) end, Ds)).

rule(Env, Domain) ->
    SubDomain = lists:append([bucket], Domain),
    FEnv = filter_env(Env),
    [
     {SubDomain, [{[], bookshelf_bkt, FEnv}]}
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
    lists:keystore(dir, 1, Env, {dir, code:priv_dir(bookshelf)}).
