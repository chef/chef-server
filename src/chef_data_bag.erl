%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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


-module(chef_data_bag).

-export([
         parse_binary_json/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

-define(VALIDATION_CONSTRAINTS,
        [
         {<<"name">>,             {match, "^[[:alnum:]_\:\.\-]+$"}}
        ]).

%% @doc Convert a binary JSON string representing a Chef data_bag into an EJson-encoded
%% Erlang data structure.
%%
%% @end
-spec parse_binary_json( binary(), create ) ->
                               { ok, ejson_term() }. % or throw
parse_binary_json(Bin, create) ->
    %% TODO: invalid_json will get logged by do_malformed_request, but
    %% currently without any additional information.  Do we want to
    %% emit the JSON we recieved (size limited) or some details of the
    %% parse error from ejson if we can extract it?
    DataBag = ejson:decode(Bin),
    case validate_data_bag(DataBag, create) of
        %% Note: we fill some fields with default values if they are missing
        ok -> {ok, DataBag};
        X -> throw(X)
  end.

validate_data_bag(DataBag, create) ->
    chef_json_validator:validate_json_by_regex_constraints(DataBag,
                                                           ?VALIDATION_CONSTRAINTS).
