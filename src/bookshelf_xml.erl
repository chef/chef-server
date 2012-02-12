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

-module(bookshelf_xml).
-include("bookshelf.hrl").
-export([
         hrl/0,
         list_buckets_xml/2,
         model/0
        ]).

%% ===================================================================
%% XML
%% ===================================================================

list_buckets_xml(Buckets, Model) ->
    {ok, Xml} = erlsom:write(list_buckets(Buckets), Model),
    xml_decl() ++ erlsom_ucs:to_utf8(Xml).

%% ===================================================================
%% Documents
%% ===================================================================

list_buckets(Buckets) ->
    #'ListAllMyBucketsResult'{ 'Owner'   = owner(),
                               'Buckets' = buckets(Buckets) }.

%% ===================================================================
%% Partials
%% ===================================================================

owner() ->
    #'CanonicalUser'{ 'ID' = "abc123",
                      'DisplayName' = "bobo-t-clown" }.

buckets(Buckets) ->
    #'ListAllMyBucketsList'{ 'Bucket' = lists:map(fun bucket/1, Buckets) }.

bucket({Name, Date}) ->
    #'ListAllMyBucketsEntry'{ 'Name' = Name,
                              'CreationDate' = Date }.

%% ===================================================================
%% Rendering, Parsing & Schema
%% ===================================================================

xml_decl() ->
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>".

model() ->
    {ok, Model} =
        erlsom:compile_xsd_file(?file("amazon_s3.xsd")),
    Model.

hrl() ->
    case file:read_file(?file("amazon_s3.xsd")) of
        {ok, Bin} ->
            Hrl = erlsom_writeHrl:writeXsdHrlFile(
                    erlsom_lib:toUnicode(Bin), []),
            file:write_file(?file("../include/amazon_s3.hrl"), Hrl);
        Error     -> Error
    end.
