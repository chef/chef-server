%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @author Marc Paradise <marc@chef.io>
%% Copyright 2017 Chef Software, Inc. All Rights Reserved.
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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%a
%%
%% The parse function of this module will convert an erlang term
%% representation of an DB operation as output by migrator_decode:parse/1
%% and convert it to a bnary SQL request suitable for use as a prepared
%% statement.  Note that the values included in the erlang terms are not
%% included in the statement - only bound parameter placeholders
%% ($1, $2 ... $n), suitable for use in prepared statements.
%
% Aside: a lot of the internal functions here can be consolidated.
%        I wrote them in get-it-done mode though, where copy-paste-modify
%        was the fastest path to working.
%
-module(migrator_encode).

-export([encode/1]).
encode(TXTerm) ->
    build_query(TXTerm).

build_query({_, _}) ->
    % begin_tx, end_tx will fit this form - we're not doing anything
    % with those yet.
    {ok, skip};
build_query({Entity, Type, {Fields, _Values}} ) ->
    build_typed_query(Entity, Type, Fields).

build_typed_query(Entity, <<"INSERT">>, Fields) ->
    FormattedFields = join_bin(Fields, <<",">>),
    Placeholders = placeholders_for_insert(length(Fields)),
    <<"INSERT INTO ",
      Entity/binary,
      " (",
      FormattedFields/binary,
      ") ",
      "VALUES (",
      Placeholders/binary,
      ")">>;
build_typed_query(Entity, <<"UPDATE">>, Fields) ->
    {PrimaryKeyClause, UpdateFields, UpdateStart} = format_primary_key(Entity, Fields),
    Placeholders = placeholders_for_update(UpdateFields, UpdateStart),
    <<"UPDATE ",
      Entity/binary,
      " SET ",
      Placeholders/binary,
      " WHERE ",
      PrimaryKeyClause/binary>>;
build_typed_query(Entity, <<"DELETE">>, Fields) ->
    FormattedPK = all_to_primary_key(Fields),
    <<"DELETE FROM ", Entity/binary, " WHERE ", FormattedPK/binary>>.

format_primary_key(<<"keys">>, [PK1, PK2 | Fields]) ->
  % This is an odd case. Most of our insert and update queries update
  % based on a single key, like id.  However, one query uses
  % a two-part key (the keys table: userid and the key's name)
  %
  % While the logical decoder seems to consistently put the
  % key fields first in order, it does not tell us how many fields
  % to use as the primary key.
  %
  % Since there's just one found case, we'll handle it directly as
  % a one-off; but a better solution would be to query the DB when
  % we build these prepared statements and have it tell us
  % what the PK is. This will likely become necessary as we expand functionalty
  % to include additoinal databases.
  {<<PK1/binary, " = $1 AND ", PK2/binary, "= $2">>, Fields, 3};
format_primary_key(_Entity, [PK | Fields]) ->
  {<<PK/binary, "= $1">>, Fields, 2}.

all_to_primary_key([First|Rest]) ->
    Acc = <<First/binary, " = $1">>,
    all_to_primary_key(Acc, 2, Rest).

all_to_primary_key(Acc, NextPos, [Field| Fields]) ->
    PosValue = integer_to_binary(NextPos),
    NewAcc = <<Acc/binary, " AND ", Field/binary, " = $", PosValue/binary>>,
    all_to_primary_key(NewAcc, NextPos + 1,Fields);
all_to_primary_key(Acc, _, []) ->
    Acc.

%% simple utility function to combine a list of binary strings
%% into a single binary with a separator.
join_bin(List, Sep) ->
    join_bin(<<>>, List, Sep).

join_bin(Acc, [Item], _Sep) ->
    <<Acc/binary, Item/binary>>;
join_bin(Acc, [Item|Items], Sep) ->
     join_bin(<<Acc/binary, Item/binary, Sep/binary>>, Items, Sep).


%% Generates field-assignment pairs for update in a prepared statemetn,
%% in the form F1=$1, F2=$, Fn=$n
placeholders_for_update([F| Fields], Start) ->
    StartBin = integer_to_binary(Start),
    placeholders_for_update(<<F/binary, " = $", StartBin/binary>>, Fields, Start + 1).

placeholders_for_update(Acc, [Field | Fields], Current) ->
  PosValue = integer_to_binary(Current),
  Acc2 = <<Acc/binary, ", ", Field/binary, " = $", PosValue/binary>>,
  placeholders_for_update(Acc2, Fields, Current + 1);
placeholders_for_update(Acc, [], _) ->
  Acc.

%% Generates placeholders for use in a SQL "VALUES" clause,
%% $1, $2... $n where N matches the number of input fields.
%% in the form F1=$1, F2=$, Fn=$n
placeholders_for_insert(Len) ->
    placeholders_for_insert(<<"$1">>, Len, 1).

placeholders_for_insert(Acc, Count, Count) ->
  Acc;
placeholders_for_insert(Acc, Count, Current) ->
  PosValue = integer_to_binary(Current + 1),
  Acc2 = <<Acc/binary, ", $", PosValue/binary>>,
  placeholders_for_insert(Acc2, Count, Current + 1).
