%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2013-2018 Chef Software, Inc.
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
%% @doc A helper module for enforcing constraints on wm
%% requests. Currently, the only constraint is on request body
%% size. This gets called by {@link
%% oc_chef_wm_base:malformed_request/2}. Constraint failures are
%% communicated to callers via `throw' and are otherwise expected to
%% return a possibly modified request data item (`#wm_reqdata{}').
%%
%% @end
-module(chef_wm_enforce).

-export([
         max_size/1
        ]).

-include("oc_chef_wm.hrl").
%% This is the max size allowed for incoming request bodies.
-define(MAX_SIZE, 1000000).

-spec max_size(#wm_reqdata{}) -> #wm_reqdata{}.
%% Verify that the request body is not larger than ?MAX_SIZE bytes. Throws `{too_big, Msg}`
%% if the request body is too large.
max_size(Req) ->
    case envy:get(oc_chef_wm, max_request_size, ?MAX_SIZE, fun validate_size/1 ) of
        disabled ->
            Req;
        TunedMaxSize ->
            max_size(wrq:method(Req),Req, TunedMaxSize)
    end.

max_size(Method, Req, MaxSize) when Method =:= 'POST';
                                    Method =:= 'PUT' ->
    try
        %% Force a read of request body. Webmachine memoizes this in the process
        %% dictionary. Webmachine will read in chunks and call exit/1 if the body exceeds
        %% the max set above. It would be nice if there was something other than a string to
        %% match against. TODO:  webmachine.
        wrq:req_body(wrq:set_max_recv_body(MaxSize, Req)),
        Req
    catch
        exit:"request body too large" ->
            Msg = iolist_to_binary([<<"JSON must be no more than ">>,
                                    integer_to_list(MaxSize),
                                    <<" bytes.">>]),
            throw({too_big, Msg})
    end;
max_size(_Method, Req, _TunedMaxSize) ->
    Req.

validate_size(Integer) when is_integer(Integer) andalso Integer > 0 ->
    true;

validate_size(disabled) ->
    true;
validate_size(Value) ->
    {invalid_value, Value, "value must be positive int or atom 'disabled'"}.
