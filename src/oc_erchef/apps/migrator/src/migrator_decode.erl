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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The parse function of this module takes a binary string
%% emitted for a single transaction out of the `test_decode` postpgres
%% logical decoding plugin and converts it to erlang terms.
%%
%% Given:
%% % (Note: spaces and newlines added for readability.  Each name[type]:val pair is space-separated.)
%% Input = <<"table public.clients: INSERT: id[character varying]:'da11fd5c59ad7cd575c4d53f6836168a'
%%                                          org_id[character]:'b22a18ce74e549b0ccb7da11fd5c59ad'
%%                                          authz_id[character]:'c3250a4fc08c482076abbaf6ed0b0ead'
%%                                          name[text]:'pedant_admin_client_api_23519'
%%                                          public_key[text]:'...snip...' ...
%%                                          validator[boolean]:false
%%                                          last_updated_by[character]:'f1cd13cb541436c290c2b4eb3685f6c2'
%%                                          created_at[timestamp without time zone]:'2017-06-05 18:06:47'
%%                                          updated_at[timestamp without time zone]:'2017-06-05 18:06:47'
%%                                          pubkey_version[smallint]:0
%%                                          admin[boolean]:false">>,
%%
%% migrator_decode:decode(Input) yields:
%%
%% { <<"clients">>, <<"INSERT">>, { [<<"id">>, <<"org_id">>, "<<authz_id>>", <<"public_key">>,
%%                                   <<"validator">>, <<"last_updated_by">>, <<"created_at">>,
%%                                   <<"updated_at">>, <<"pubkey_version">>, <<"admin">>],
%%                                  [ <<"da11fd5c59ad7cd575c4d53f6836168a">>}, <<"b22a18ce74e549b0ccb7da11fd5c59ad">>},
%%                                    <<"c3250a4fc08c482076abbaf6ed0b0ead">>}, <<"...snip...">>,
%%                                    <<"false">>,, <<"f1cd13cb541436c290c2b4eb3685f6c2">>,
%%                                    <<"2017-06-05 18:06:47">>}, <<"2017-06-05 18:06:47">>, 0, false ]
%%                                }
%%
%% All fields values are kept as their original types received, compatible what
%% epgsql expects.
%%
%% NOTE: So far, it seems that UPDATE/DELETE statements all arrive with the update key field(s) first
%%       in the order, but this could a matter of the key fields coinciding with the natural
%%       field order of hte table.  It is something to be aware of if we do expand this to other databases.
%%       Ideally, this decodeer would know how to ask the DB for primary key data so that we can explicitly
%%       place them in generated statements.

-module(migrator_decode).

-export([parse/1]).

parse({_TlogIdx, _TxIdx, Data}) ->
    lager:debug("Decoding: ~p", [Data]),
    do_decode(Data);
parse(Other) ->
    lager:warning("Unknown record: ~p", [Other]),
    throw(unknown_record).


do_decode(<<"BEGIN ", TXID/binary>>) ->
    {ok, {tx_start, TXID}};
do_decode(<<"COMMIT ",TXID/binary>>) ->
    {ok, {tx_end, TXID}};
do_decode(<<"table public.", Rest/binary>>) ->
    decode_transaction(Rest);
do_decode(<<"table ", Rest/binary>>) ->
    {QualifiedTable, _Rest} = binary:split(Rest, <<":">>),
    {error, {unsupported_schema, QualifiedTable}};
do_decode(Other) ->
    {UnknownType, _Rest} = binary:split(Other, <<":">>),
    % TODO Not doing sequences or other object types yet either.
    {error, {unknown_type, UnknownType}}.

decode_transaction(Raw) ->
  decode_transaction2(extract_op_elements(Raw)).

decode_transaction2({Table, Operation, <<"(no-tuple-data)">>}) ->
    % This seems to happen for (maybe) some trigger-based deletes - I think in this cae
    % we have hit a cleanup path, because the same TX is used for a group of changes:
    % Subsequent changes - under the same TXID - looked like this:
    % <<"table public.cookbook_artifact_version_checksums: DELETE: (no-tuple-data)">>},
    % <<"table public.checksums: DELETE: org_id[character]:'b22a18ce74e549b0ccb7da11fd5c59ad' checksum[character]:'268750691044b4fbab541d0edb2e0d7b'">>},
    % <<"table public.cookbook_artifacts: DELETE: id[integer]:17">>},
    % <<"table public.cookbook_artifact_versions: DELETE: id[bigint]:22">>},
    %%
    %% TODO(ssd) 2017-06-15:
    %%    I think that the above mentioned delete is part of our delete_cookbook_artifact_version query:
    %%
    %%        {delete_cookbook_artifact_version_by_id, <<"SELECT * FROM delete_cookbook_artifact_version($1)">>}.
    %%
    %%    This function is in schema/deploy/delete_cookbook_artifact_version.sql.
    %%
    %%    This comment from the test_decoding source is a bit concerning:
    %%
    %%         /* if there was no PK, we only know that a delete happened */
    %%         if (change->data.tp.oldtuple == NULL)
    %%             appendStringInfoString(ctx->out, " (no-tuple-data)");
    %%
    %%    Perhaps the correct right thing to do here is to identify which query it is and redo that query?
    %%
    %%    The test_decoding source code can also emit (no-tuple-data) for INSERTS and UPDATES:
    %%
    %%      https://github.com/postgres/postgres/blob/master/contrib/test_decoding/test_decoding.c#L433
    %%      https://github.com/postgres/postgres/blob/master/contrib/test_decoding/test_decoding.c#L451
    %%
    %%    In short, more research needed here.
    {ok, {Table, Operation, noop}};
decode_transaction2({Table, Operation, {OldKey, NewTuple}}) ->
    {ok, {Table, Operation, extract_fields(OldKey, {[], []}), extract_fields(NewTuple, {[], []}) }};
decode_transaction2({Table, Operation, NewTuple}) ->
    {ok, {Table, Operation, extract_fields(NewTuple, {[], []}) }}.

%
%% Returns the entity being operated on, the operation itself, and the remaining unparsed binary.
extract_op_elements(Data) ->
    %% Data = clients: INSERT: id[character varying]:'$VALUE'
    [Table, Rest0] = binary:split(Data, <<": ">>),
    [Operation, Rest1] = binary:split(Rest0, <<": ">>),
    {Table, Operation, extract_new_tuple(Rest1)}.

%%
%% For UPDATES where one of the primary key fields is being changed,
%% we get data about the OLD primary key fields.  We need to use the
%% old primary keys to construct the WHERE clause:
%%
%% <<"table public.keys: UPDATE: old-key: id[character]:'8b3430304441db383fd877d1ed0f676a' key_name[text]:'alt_key' new-tuple: id[character]:'8b3430304441db383fd877d1ed0f676a' key_name[text]:'altname1' public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAx0FkgowHXIKeroJep8yx\n63ENXRARzluk3obSvpUbmPxwAvWP1gfqSi1JmRlhLufcWNJ+sMwWvp6YVlLnEkTW\nb85YuwPwnRp7ICs5ACLKbP11eO2hdLoxLIo4ryzNvzY6ufba2VjD++E4TkN0tL+h\nkJ6tqEfNOOtSkHns0axZnFrpEe2fhR7z6MpSm1aDQptHUEDHj176KJBCpyyeiQR7\nYQex72fy7wXD4XIp8cx37Ftb6jRJqKmtwCaHxj4OnxNMS0fdDWUeTk3gceQLdDex\nml5/zAYFxFjdUAMNnqAKSi4thySyVrBs21e8q2hRWcJZPh5gbzP1SeBzqDlBFgRb\nHQIDAQAB\n-----END PUBLIC KEY-----\n' key_version[integer]:0 created_at[timestamp without time zone]:'2017-06-15 08:22:27' expires_at[timestamp without time zone]:'2049-12-24 21:00:00' last_updated_by[character]:'0d1552523cccdf029a3d09a19b4a10b1' updated_at[timestamp without time zone]:'2017-06-15 08:22:27'">>
%%
extract_new_tuple(<<"old-key: ", Rest/binary>>) ->
    [OldKey, NewTuple] = binary:split(Rest, <<"new-tuple: ">>),
    {OldKey, NewTuple};
extract_new_tuple(Data) ->
    Data.

extract_fields(<<>>, {OutFields, OutValues}) ->
    { lists:reverse(OutFields), lists:reverse(OutValues)};
extract_fields(TxData, {OutFields, OutValues}) ->
    %% Data = id[character varying]:'$VALUE' something_else[....
    %% enable this when we fail to match as expected
    %% and you want to see where it left off:
    %% io:fwrite("In: ~p~n", [TxData]),
    [Field, Rest0] = binary:split(TxData, <<"[">>),
    %% We are not currently using type data when reconstituting these queries but
    %% this is where it comes from if we need it at any point.
    [Type, Rest1] = binary:split(Rest0, <<"]:">>),
    {Value, Rest3} = case Rest1 of
                         <<"'",Rest2/binary>> ->
                             {ok, Pos, R} = find_quoted_value_end(Rest2, 0),
                             V = binary:part(Rest1, 1, Pos),
                             {V, R};
                         Rest2 ->
                                                % This is unquoted, so we're looking for a space only.
                             {ok, Pos, R} = find_value_end(Rest2, 0),
                             V = binary:part(Rest1, 0, Pos),
                             {V, R}
                     end,
    CVal = coerce_type(Type, Value),
    extract_fields(Rest3, { [ Field | OutFields],  [CVal | OutValues] }).

coerce_type(_T, <<"null">>) ->
    undefined;
coerce_type(<<"text">>, V) ->
    V;
coerce_type(<<"character">>, V) ->
    V;
coerce_type(<<"character varying">>, V) ->
    V;
coerce_type(<<"password_hash_type">>, V) ->
    %% TODO(ssd) 2017-06-15: Verify what the right thing to do would
    %% be for enums
    V;
coerce_type(<<"bigint">>, V) ->
    binary_to_integer(V);
coerce_type(<<"smallint">>, V) ->
    binary_to_integer(V);
coerce_type(<<"integer">>, V) ->
    binary_to_integer(V);
coerce_type(<<"boolean">>, <<"false">>) ->
    false;
coerce_type(<<"boolean">>, <<"true">>) ->
    true;
coerce_type(<<"timestamp without time zone">>, <<"infinity">>) ->
    %% TODO(ssd) 2017-06-14: copy/pasta from chef_object's timestamp
    %% parsing code.
    {{294277,1,9},{4,0,54.775807}};
coerce_type(<<"timestamp without time zone">>, V) ->
    case ec_date:parse(binary_to_list(V)) of
        %% TODO(ssd) 2017-06-14: This throws out sub-second precision on SQL-produced timestamps because
        %% epgsql doesn't know how to deal with the extra term
        {D, {H, M, S, _}} ->
            {D, {H, M, S}};
        Other ->
            Other
    end;
coerce_type(T, V) ->
    lager:warning("UNHANDLED TYPE ~p ~p", [T, V]),
    V.

find_value_end(<<>>, Pos) ->
  {ok, Pos, <<>>};
find_value_end(<<" ",Rest/binary>>, Pos) ->
  {ok, Pos, Rest};
find_value_end(<<_:1/binary, Rest/binary>>, Pos) ->
  find_value_end(Rest, Pos + 1).

% We wil need two paths here. Instead of worrying about the
% mapping of types -> quoted/not, we will look for the
% field separator of :.  If the next char is ', we epxect a matching close;
% otherwise we will look only for space.
% There are probably better ways, but this works for now:
% First exclude escaped apostrophes
find_quoted_value_end(<<"\\'",Rest/binary>>, Pos) ->
  find_quoted_value_end(Rest, Pos + 2);
find_quoted_value_end(<<"'">>, Pos) ->
  % ' at the end of the line is also an end marker
  {ok, Pos, <<>>};
find_quoted_value_end(<<"' ",Rest/binary>>, Pos) ->
  {ok, Pos, Rest};
find_quoted_value_end(<<>>, Pos) ->
  {error, {expected_field_end, Pos}};
find_quoted_value_end(<<_:1/binary, Rest/binary>>, Pos) ->
  find_quoted_value_end(Rest, Pos + 1).
