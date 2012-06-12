%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_conf).

%% API
-export([start_link/0, get_configuration/0,
        keys/0]).

-include("internal.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_configuration() ->
    lists:flatten([sec_handler(),
                   ip(),
                   dispatch(),
                   pool(),
                   port(),
                   keys()]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

sec_handler() ->
    {onrequest, fun bksw_sec:handle_request/1}.

ip() ->
    case application:get_env(interface) of
        undefined ->
            [{ip, ip("lo")}];
        {ok, Interface} ->
            [{ip, ip(Interface)}]
    end.

dispatch() ->
    case application:get_env(domains) of
        undefined ->
            [{dispatch, rules(["localhost.localdomain"])}];
        {_, Domains} ->
            [{dispatch, rules(Domains)}]
    end.

pool() ->
    case application:get_env(pool) of
        undefined ->
            [{pool, 100}];
        {ok, Pool} ->
            [{pool, Pool}]
    end.

port() ->
    case application:get_env(port) of
        undefined ->
            {port, 4321};
        {ok, Port} ->
            {port, Port}
    end.

keys() ->
    case application:get_env(keys) of
        undefined ->
            {keys, {<<"">>, <<"">>}};
        {ok, {AWSAccessKey, SecretKey}} ->
            {keys, {bksw_util:to_binary(AWSAccessKey),
                    bksw_util:to_binary(SecretKey)}}
    end.

ip(Interface) ->
    {ok, All} = inet:getifaddrs(),
    Attribs = proplists:get_value(Interface, All),
    Addrs = [Attr || Attr <- Attribs, ip_filter(Attr)],
    [{addr, Addr} | _] = Addrs,
    Addr.

ip_filter(Attr) ->
    case Attr of
      {addr, {_, _, _, _}} -> true;
      _ -> false
    end.

rules(Domains) ->
    lists:flatten([rule(Domain)
                   || Domain <- format_domains(Domains)]).

format_domains(Domains) ->
    [create_domain(Domain) || Domain <- Domains].

rule(Domain) ->
    SubDomain = [bucket] ++ Domain,
    FEnv = dispatch_rules(),
    [{Domain, [{[], bksw_idx, FEnv}]},
     {SubDomain,
      [{[], bksw_bkt, FEnv},
       {['...'], bksw_obj, FEnv}]}].

dispatch_rules() ->
    case application:get_env(dispatch) of
        undefined ->
            [];
        Defined ->
            Defined
    end.

create_domain(Domain) ->
    [fun list_to_binary/1(Token)
     || Token <- string:tokens(Domain, ".")].
