%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Eric B Merritt <ericbmerritt@gmail.com>
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

-module(bksw_util).

-export([get_bucket/1,
         get_object_and_bucket/1,
         file/1,
         service_available/2,
         to_integer/1,
         to_string/1,
         to_binary/1]).

-include("internal.hrl").

%%===================================================================
%% API functions
%%===================================================================
file(Path) ->
    filename:join(code:priv_dir(bookshelf), Path).

-spec to_integer(string() | binary()) -> integer().
to_integer(Val) ->
    erlang:list_to_integer(to_string(Val)).

-spec to_string(binary() | string()) -> string().
to_string(Val) when is_binary(Val) ->
    erlang:binary_to_list(Val);
to_string(Val) when is_list(Val) ->
    Val.

-spec to_binary(binary() | string()) -> binary().
to_binary(Val) when is_list(Val) ->
    erlang:list_to_binary(Val);
to_binary(Val) when is_binary(Val) ->
    Val.

-spec get_bucket(term()) -> term().
get_bucket(Req0) ->
    case wrq:path_info(bucket, Req0) of
        undefined ->
            %% We would through a bad match here but you cant have
            %% guards in a match which is really unfortunate
            erlang:error(bad_bucket_dep);
        GoodValue ->
            to_binary(GoodValue)
    end.

get_object_and_bucket(Rq0) ->
    case string:tokens(wrq:path(Rq0), "/") of
        [] ->
            {ok, <<"">>, <<"">>};
        [Bucket] ->
            {ok, bksw_util:to_binary(Bucket),
             <<"">>};
        [Bucket | Path] ->
            {ok, bksw_util:to_binary(Bucket),
             bksw_util:to_binary(filename:join(Path))}
    end.

service_available(Req, #context{reqid_header_name = HeaderName} = State) ->
    %% Extract or generate a request id
    ReqId = oc_wm_request:read_req_id(HeaderName, Req),

    %% If no UserId is generated, this will return undefined. The opscoderl_wm request
    %% logger will omit user=; downstream.
    UserId = wrq:get_req_header("x-ops-userid", Req),

    Req0 = oc_wm_request:add_notes([{req_id, ReqId},
                                    {user, UserId}], Req),

    {true, Req0, State#context{reqid = ReqId}}.
