%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Doug Triggs <doug@chef.io>
%% Copyright 2012-2018 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%


-module(chef_json).

-export([
         decode_body/1,
         decode/1,
         encode/1
        ]).

-include("chef_types.hrl").

decode(Bin) ->
    try
        jiffy:decode(Bin)
    catch
        throw:{error, _} ->
            throw({error, invalid_json})
    end.

encode(EJSON) ->
    erlang:iolist_to_binary(jiffy:encode(EJSON)).

-spec decode_body( binary() ) -> ejson_term(). % or throw
%% @doc Decodes JSON body and verifies valid payload type
decode_body(Bin) ->
    Body = decode(Bin),
    verify_json_type(Body).

verify_json_type({JSON}) when is_list(JSON) ->
    {JSON};
verify_json_type(_) ->
    throw(invalid_json_object).
