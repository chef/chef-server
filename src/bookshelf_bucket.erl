%% @author Tim Dysinger <timd@opscode.com>
%% @copyright 2012 Opscode, Inc. All Rights Reserved

-module(bookshelf_bucket).
-compile(export_all).

init(_Transport, _Rq, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

allowed_methods(Rq, St) ->
    {['GET', 'PUT', 'DELETE'], Rq, St}.

content_types_provided(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, to_xml}], Rq, St}.

to_xml(Rq, St) ->
    {<<"<xml>hello</xml>">>, Rq, St}.

content_types_accepted(Rq, St) ->
    {[{{<<"text">>, <<"xml">>, []}, from_xml}], Rq, St}.

from_xml(Rq, St) ->
    {true, Rq, St}.
