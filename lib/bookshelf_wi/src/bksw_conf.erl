%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_conf).

-behaviour(gen_server).

%% API
-export([start_link/0, get_configuration/0,
        setup_default_configuration/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("internal.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, Error::term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_configuration() ->
    lists:flatten([ip(),
                   dispatch(),
                   pool(),
                   {port, opset:get_value(port, [], ?BOOKSHELF_CONFIG)},
                   {keys, opset:get_value(keys, [], ?BOOKSHELF_CONFIG)}]).

setup_default_configuration() ->
    opset:create(?BOOKSHELF_CONFIG, []),
    opset:set_if_not_already_set([{domains, ["localhost.localdomain"]},
                                  {interface, "lo"},
                                  {port, 4321},
                                  {pool, 100},
                                  {keys, {"e1efc99729beb175",
                                          "fc683cd9ed1990ca"}}],
                                 ?BOOKSHELF_CONFIG).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, ok}.
init([]) ->
    {ok, ok}.

%% @doc this should not be called in this case.
-spec handle_call(Request::term(), From::pid(), ok) ->
                         {reply, Reply::term(), ok}.
handle_call(die_a_horrible_death, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc this should not be called in this case.
handle_cast(die_a_horrible_death, State) ->
    {noreply, State}.

%% @doc this will be called by the config server.
-spec handle_info(Info::term(), ok) -> {noreply, ok}.
handle_info({_, ?BOOKSHELF_CONFIG, ChangedValues}, _) ->
    case if_values_changed(ChangedValues, [interface, domains,
                                           dispatch, pool, port, keys]) of
        true ->
            bksw_sup:reconfigure_cowboy();
        _ ->
            ok
    end,
    {noreply, ok}.

-spec terminate(Reason::term(), ok) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn::term(), ok, Extra::term()) -> {ok, ok}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
if_values_changed(ChangedValues, ListOfChangedKeys) ->
    lists:any(fun({_, KVPairs}) ->
                      lists:any(fun({Key, _}) ->
                                         lists:member(Key, ListOfChangedKeys)
                                end, KVPairs)
              end, ChangedValues).

ip() ->
    case opset:get_value(interface, ?BOOKSHELF_CONFIG) of
        not_found ->
            [];
        {ok, Interface} ->
            [{ip, ip(Interface)}]
    end.

dispatch() ->
    case opset:get_value(domains, ?BOOKSHELF_CONFIG) of
        not_found ->
            [];
        {_, Domains} ->
            [{dispatch, rules(Domains)}]
    end.

pool() ->
    case opset:get_value(pool, ?BOOKSHELF_CONFIG) of
        not_found ->
            [{pool, 100}];
        {ok, Pool} ->
            [{pool, Pool}]
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
    opset:get_value(dispatch, [], ?BOOKSHELF_CONFIG).

create_domain(Domain) ->
    [fun list_to_binary/1(Token)
     || Token <- string:tokens(Domain, ".")].
