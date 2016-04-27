%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@chef.io>
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

-module(chef_sandbox).

-export([
         id/1,
         parse_binary_json/2,
         set_created/2,
         fields_for_fetch/1,
         find_query/1,
         record_fields/1,
         set_api_version/2,
         fetch/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"checksums">>, {fun_match, {fun valid_checksum_hash/1,
                                         object,
                                         <<"Bad checksums!">>}
                            }}
         ]}).

%% @doc Convert a binary JSON string representing a Chef Sandbox into an
%% EJson-encoded Erlang data structure.
-spec parse_binary_json( binary(), create ) -> { ok, ejson_term() }. % or throw
parse_binary_json(Bin, create) ->
    Sandbox = chef_json:decode(Bin),
    validate(Sandbox).

-spec validate(ej:json_object()) -> {ok, ej:json_object()}.
validate(Sandbox) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, Sandbox) of
        ok ->
            {ok, Sandbox};
        Bad ->
            throw(Bad)
    end.

%% @doc Validation function for a sandbox's checksum hash.  Ensures that at least one entry
%% is present.  The value must be null.
-spec valid_checksum_hash(Input :: any()) -> ok | error.
valid_checksum_hash({[]}) ->
    error;
valid_checksum_hash({[{Checksum, Value}|Rest]}) ->
    case {is_md5_hex(Checksum), Value} of
        {true, null} ->
            case Rest of
                [] -> ok;  % Easy termination condition that ensures at least one entry is present
                _ -> valid_checksum_hash({Rest})
            end;
        _ ->
            error
    end;
valid_checksum_hash(_) ->
    error.

is_md5_hex(<<Bin:32/binary>>) ->
    is_hex(Bin);
is_md5_hex(_) ->
    false.

is_hex(<<>>)                 -> true;
is_hex(<<"0", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"1", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"2", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"3", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"4", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"5", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"6", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"7", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"8", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"9", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"a", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"b", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"c", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"d", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"e", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"f", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"A", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"B", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"C", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"D", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"E", Rest/binary>>) -> is_hex(Rest);
is_hex(<<"F", Rest/binary>>) -> is_hex(Rest);
is_hex(_)                    -> false.

%% start of chef_object behaviour callbacks
-spec set_created(#chef_sandbox{}, object_id()) -> #chef_sandbox{}.
set_created(#chef_sandbox{} = Object, _ActorId) ->
    Now = chef_object_base:sql_date(now),
    Object#chef_sandbox{created_at = Now}.

id(#chef_sandbox{id = Id}) ->
    Id.

fields_for_fetch(#chef_sandbox{org_id = OrgId,
                               id = Id}) ->
    [OrgId, Id].

find_query(_ObjectRec) ->
    find_sandbox_by_id.

record_fields(_ApiVersion) ->
    record_info(fields, chef_sandbox).

-spec(fetch(#chef_sandbox{}, chef_object:select_callback()) -> chef_object:select_return()).
fetch(#chef_sandbox{org_id = OrgId, id = SandboxID}, CallbackFun) ->
    case CallbackFun({find_sandbox_by_id, [OrgId, SandboxID]}) of
        Rows when is_list(Rows) ->
            sandbox_join_rows_to_record(Rows);
        Other ->
            Other
    end.



%% @doc Transforms a collection of proplists representing a sandbox / checksum join query
%% result and collapses them all into a single sandbox record.  There is a row for each
%% checksum.  A checksum tuple is extracted from each row; sandbox information is extracted
%% from the final row (since it's the same in every row).
%%
%% See the 'find_sandbox_by_id' prepared query for the row "shape".
sandbox_join_rows_to_record(Rows) ->
    sandbox_join_rows_to_record(Rows, []).
sandbox_join_rows_to_record([LastRow|[]], Checksums) ->
    C = proplist_to_checksum(LastRow),
    #chef_sandbox{id = safe_get(<<"sandbox_id">>, LastRow),
                  org_id = safe_get(<<"org_id">>, LastRow),
                  created_at = safe_get(<<"created_at">>, LastRow),
                  checksums = lists:reverse([C|Checksums])};
sandbox_join_rows_to_record([Row|Rest], Checksums ) ->
    C = proplist_to_checksum(Row),
    sandbox_join_rows_to_record(Rest, [C|Checksums]).

%% @doc Safely retrieves a value from a proplist.  Throws an error if the specified key does
%% not exist in the list.
-spec safe_get(Key::binary(), Proplist::[{binary(), term()}]) -> term().
safe_get(Key, Proplist) ->
    {Key, Value} = lists:keyfind(Key, 1, Proplist),
    Value.

%% @doc Convenience function for assembling a checksum tuple from a proplist containing
%% 'checksum' and 'uploaded' keys.
proplist_to_checksum(Proplist) ->
    {safe_get(<<"checksum">>, Proplist),
     %% Normalize boolean representations
     %% TODO: It would be nice for this transformation to reside in sqerl
     case safe_get(<<"uploaded">>, Proplist) of
         0 -> false;
         1 -> true;
         true -> true;
         false -> false
     end}.

set_api_version(ObjectRec, ApiVersion) ->
    ObjectRec#chef_sandbox{server_api_version = ApiVersion}.

