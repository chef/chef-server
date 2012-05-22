%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
%% @copyright Copyright 2012 Opscode, Inc.
-module(bksw_idx).

-export([init/3, rest_init/2, allowed_methods/2, content_types_provided/2,
         resource_exists/2, to_xml/2]).

-include_lib("cowboy/include/http.hrl").

%%===================================================================
%% Public API
%%===================================================================

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Rq, _Opts) ->
    {ok, bksw_req:with_amz_request_id(Rq), undefined}.

allowed_methods(Rq, St) -> {['GET'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

resource_exists(Rq, St) ->
    {erlang:is_list(bookshelf_store:bucket_list()), Rq, St}.

to_xml(Rq, St) ->
    Buckets = bookshelf_store:bucket_list(),
    Term = bksw_xml:list_buckets(Buckets),
    Body = bksw_xml:write(Term),
    {Body, Rq, St}.

%%===================================================================
%% Eunit Tests
%%===================================================================
-ifndef(NO_TESTS).

-include_lib("eunit/include/eunit.hrl").

allowed_methods_test_() ->
    [{"should only support 'GET'",
      fun () ->
              Expected = ['GET'],
              {Allowed, _, _} = allowed_methods(#http_req{pid=self()}, % make dialyzer happy
                                                undefined),
              ?assertEqual(Expected, Allowed)

      end}].

content_types_provided_test_() ->
    [{"should only support text/xml output",
      fun () ->
              {Types, _, _} = content_types_provided(#http_req{pid=self()}, % make dialyzer happy
                                                     undefined),
              ?assertEqual(1, length(Types)),
              ?assertEqual(true, lists:keymember({<<"text">>, <<"xml">>, []}, 1,
                                                 Types))
      end}].

-endif.
