%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2015-2018 Chef Software, Inc. All Rights Reserved.
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


-module(chef_object_default_callbacks).

-export([fetch/2,
         update/2,
         delete/2,
         fields_for_insert/1,
         is_undefined/1]).

fetch(Rec, CallbackFun) ->
    Mod = element(1, Rec),
    CallbackFun({Mod:find_query(Rec), Mod:fields_for_fetch(Rec),
                 {first_as_record, [Mod, Mod:record_fields(Rec)]}}).

update(Rec, CallbackFun) ->
    Mod = element(1, Rec),
    CallbackFun({Mod:update_query(Rec), Mod:fields_for_update(Rec)}).

fields_for_insert(Rec) ->
    [_RecName, _ApiVersion|Tail] = tuple_to_list(Rec),
    %% We detect if any of the fields in the record have not been set
    %% and throw an error
    case lists:any(fun is_undefined/1, Tail) of
        true -> error({undefined_in_record, Rec});
        false -> ok
    end,
    Tail.

delete(Rec, CallbackFun) ->
    Mod = element(1, Rec),
    QueryName = Mod:delete_query(Rec),
    Id = Mod:id(Rec),
    CallbackFun({QueryName, [Id]}).

is_undefined(undefined) -> true;
is_undefined(_) -> false.
