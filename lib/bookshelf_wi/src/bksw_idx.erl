%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_idx).

-export([init/1, is_authorized/2, allowed_methods/2, content_types_provided/2,
         resource_exists/2, to_xml/2]).

-include_lib("webmachine/include/webmachine.hrl").
%%===================================================================
%% Public API
%%===================================================================

init(_Context) ->
    {ok, bksw_conf:get_context()}.

is_authorized(Rq, Ctx) ->
    bksw_sec:is_authorized(Rq, Ctx).

allowed_methods(Rq, Ctx) ->
    {['GET'], Rq, Ctx}.

content_types_provided(Rq, Ctx) ->
    {[{"text/xml", to_xml}], Rq, Ctx}.

resource_exists(Rq, Ctx) ->
    {erlang:is_list(bookshelf_store:bucket_list()), Rq, Ctx}.

to_xml(Rq, Ctx) ->
    Buckets = bookshelf_store:bucket_list(),
    Term = bksw_xml:list_buckets(Buckets),
    Body = bksw_xml:write(Term),
    {Body, Rq, Ctx}.
