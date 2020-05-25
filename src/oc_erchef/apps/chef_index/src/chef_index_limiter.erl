%% Copyright 2020 Chef Software, Inc
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
-module(chef_index_limiter).

-export([setup/0,
         take_ticket/0,
         return_ticket/0,
         return_tickets/1,
         tickets_left/0
        ]).

-define(TABLE_NAME, chef_index_limiter).
-define(KEY_NAME, inflight_updates).

-spec setup() -> ok.
setup() ->
    ets:new(?TABLE_NAME, [public, named_table]),
    ok.

-spec tickets() -> non_neg_integer().
tickets() ->
    envy:get(chef_index, search_batch_max_inflight_updates, 128, non_neg_integer).

-spec take_ticket() -> ok | {error, chef_index_update_limit_reached}.
take_ticket() ->
    Limit = tickets(),
    case ets:update_counter(?TABLE_NAME, ?KEY_NAME, [{2, 0}, {2, 1, Limit, Limit}], {2, 0}) of
        [Limit, Limit] ->
            {error, chef_index_update_limit_reached};
        [_OldCount, _NewCount] ->
            ok
    end.

-spec return_ticket() -> ok.
return_ticket() ->
    return_tickets(1).

-spec return_tickets(non_neg_integer()) -> ok.
return_tickets(Num) ->
    ets:update_counter(?TABLE_NAME, ?KEY_NAME, {2, -Num, 0, 0}, {2, 0}),
    ok.

-spec tickets_left() -> non_neg_integer().
tickets_left() ->
    TicketCount = ets:lookup_element(?TABLE_NAME, ?KEY_NAME, 2),
    tickets() - TicketCount.
