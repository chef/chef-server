%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_conf).

%% API
-export([get_configuration/0, get_context/0,
        access_key_id/1, secret_access_key/1]).

-include("internal.hrl").

-record(context, {access_key_id,
                  secret_access_key}).

%%%===================================================================
%%% types
%%%===================================================================
-opaque context() :: record(context).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_context() -> context().
get_context() ->
    {keys, {AccessKeyId, SecretAccessKey}} = keys(),
    #context{access_key_id=AccessKeyId, secret_access_key=SecretAccessKey}.

-spec get_configuration() -> list().
get_configuration() ->
    lists:flatten([ip(),
                   dispatch(),
                   port(),
                   log_dir()]).

-spec access_key_id(context()) -> binary().
access_key_id(#context{access_key_id=AccessKeyId}) ->
    AccessKeyId.

-spec secret_access_key(context()) -> binary().
secret_access_key(#context{secret_access_key=SecretAccessKey}) ->
    SecretAccessKey.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ip() ->
    case application:get_env(ip) of
        undefined ->
            [{ip, "127.0.01"}];
        {ok, Ip} ->
            [{ip, Ip}]
    end.

dispatch() ->
    case application:get_env(domains) of
        undefined ->
            [{dispatch, rules(["localhost.localdomain"])}];
        {_, Domains} ->
            [{dispatch, rules(Domains)}]
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

rules(Domains) ->
    lists:flatten([rule(Domain)
                   || Domain <- format_domains(Domains)]).

format_domains(Domains) ->
    [create_domain(Domain) || Domain <- Domains].

rule(Domain) ->
    SubDomain = [bucket] ++ Domain,
    FEnv = undefined,
    [{SubDomain,
      [{[], bksw_bkt, FEnv},
       {[obj_part, '*'], bksw_obj, FEnv}]},
     {[bucket, obj_part, '*'], bksw_obj, FEnv},
     {[bucket], bksw_bkt, FEnv},
     {[], bksw_idx, FEnv}].

create_domain(Domain) ->
    [fun list_to_binary/1(Token)
     || Token <- string:tokens(Domain, ".")].

log_dir() ->
    case application:get_env(log_dir) of
        undefined ->
            Dir = code:priv_dir(bookshelf_wi),
            {log_dir, Dir};
        {ok, Dir} ->
            {log_dir, Dir}
    end.
